// Shift nodes that have their address taken to the front of the nodes set,
// which clumps them closely together. This is an optimization.
u32 shuffle_addr_taken_nodes() {
  std::vector<Node *> old_nodes;
  old_nodes.swap(nodes);

  u32 old_nodes_sz = old_nodes.size();
  nodes.resize(old_nodes_sz);

  u32 *tmp = (u32 *) malloc(old_nodes_sz * 4);

  // Keep placeholder nodes at the front.
  for (u32 i = 0; i < First; i++) {
    nodes[i] = old_nodes[i];
    tmp[i] = i;
  }

  // First copy all address taken nodes.
  u32 node_counter = First;
  for (u32 i = First; i < old_nodes_sz; i++) {
    bool addr_taken = old_nodes[i]->obj_sz > 0;
    if (addr_taken) {
      nodes[node_counter] = old_nodes[i];
      tmp[i] = node_counter++;
    }
  }
  u32 last_object_node = node_counter - 1;

  // Then copy all of the others.
  for (u32 i = first_var_node; i < old_nodes_sz; i++) {
    bool addr_taken = old_nodes[i]->obj_sz > 0;
    if (!addr_taken) {
      nodes[node_counter] = old_nodes[i];
      tmp[i] = node_counter++;
    }
  }

  // Re-number the nodes in all constraints.
  for (int i = 0; i < constraints.size(); i++) {
    Constraint &c = constraints[i];
    c.dest = tmp[c.dest];
    c.src = tmp[c.src];
    assert(c.type != ConstraintAddrOf || c.src <= last_object_node);
  }
  
  // Re-number the nodes in all value-node maps.
  for (llvm::DenseMap<llvm::Value*, u32>::iterator i = value_nodes.begin(),
        e= value_nodes.end(); i != e; ++i) {
    i->second = tmp[i->second];
  }
  for (llvm::DenseMap<llvm::Value*, u32>::iterator i = object_nodes.begin(),
        e = object_nodes.end(); i != e; ++i) {
    i->second = tmp[i->second];
  }
  for (llvm::DenseMap<llvm::Function*, u32>::iterator i = ret_nodes.begin(),
        e = ret_nodes.end(); i != e; ++i) {
    i->second = tmp[i->second];
  }
  for (llvm::DenseMap<llvm::Function*, u32>::iterator i = vararg_nodes.begin(),
        e = vararg_nodes.end(); i != e; ++i) {
    i->second = tmp[i->second];
  }
  
  // Re-number the nodes in ind_calls.
  std::set<u32> old_ind_calls;
  old_ind_calls.swap(ind_calls);
  for (std::set<u32>::iterator i = old_ind_calls.begin(), e = old_ind_calls.end();
       i != e; ++i) {
    ind_calls.insert(tmp[*i]);
  }
  
  // Re-number the nodes in icall_cons.
  std::vector<std::pair<Constraint, std::set<llvm::Instruction*> > > old_icall_cons;
  for(llvm::DenseMap<Constraint, std::set<llvm::Instruction*> >::iterator
        i = icall_cons.begin(), e= icall_cons.end(); i != e; ++i) {    
    old_icall_cons.push_back(*i);
  }
  icall_cons.clear();  

  for (int i = 0; i < old_icall_cons.size(); i++) {
    Constraint &c = old_icall_cons[i].first;
    c.dest = tmp[c.dest];
    c.src = tmp[c.src];
    icall_cons[c] = old_icall_cons[i].second;
  }
  
  // Free the temporary structure.
  free(tmp);

  // Check that the nodes are sane.
  sanity_check();

  return last_object_node;
}

static std::vector<u32> rewrite_constraints_to_use_node_reps(AnalysisSet *as,
                                                             BDDSets *bdds) {

  llvm::DenseSet<Constraint> seen;
  std::vector<Constraint> old;
  old.swap(as->constraints);

  std::vector<u32>& redir;
  redir.assign(old.size(), 0);

  for (int i = 0; i < old.size(); i++) {
    Constraint old_con = old[i];

    Node *dest_node = as->nodes->nodes[old_con.dest];
    Node *src_node = as->nodes->nodes[old_con.src];

    if (dest_node->nonptr || src_node->nonptr) {
      redir[i] = MAX_U32;
      continue;
    }

    if (old_con.type != ConstraintStore &&
        old_con.type != ConstraintAddrOf &&
        pts_is_null(as, bdds, old_con.src, 0)) {
      redir[i] = MAX_U32;
      continue;
    }

    if (old_con.type == ConstraintStore &&
        pts_is_null(as, bdds, old_con.dest, 0)) {
      redir[i] = MAX_U32;
      continue;
    }

    Constraint c(old_con);
    c.dest = as->nodes->rep(c.dest);

    if (c.type != ConstraintAddrOf) {
      c.src = as->nodes->rep(c.src);
    }

    if (c.type == ConstraintCopy &&
        !c.off && c.src == c.dest) {
      redir[i] = MAX_U32;
      continue;
    }
    
    if (c.type != ConstraintLoad &&
        c.type != ConstraintStore) {
      seen.insert(c);
    }

    redir[i] = as->constraints.size();
    as->constraints.push_back(c);
  }

  return std::move(redir);
}


void shrink_indirect_constraints(AnalysisSet *as,
                                 std::vector<u32> &redir) {
  std::vector<u32> new_indirect;  
  for (ConstraintInstMap::iterator i = as->indirect_constraints.begin(),
         e = as->indirect_constraints.end(); i != e; i++) {
    if (redir[*i] != MAX_U32) {
      new_indirect.push_back(redir[*i]);
    }
  }

  std::sort(new_indirect.begin(), new_indirect.end());
  ConstraintInstMap::iterator e = std::unique(new_indirect.begin(),
                                              new_indirect.end());
  new_indirect.erase(e, new_indrect.end());
  as->indirect_constraints.swap(new_indirect);
}

void shrink_defs_and_uses(Processor *proc,
                          std::vector<u32> &redir) {
                          
  std::vector<u32> new_defs(proc->defs.size());
  std::vector<u32> new_uses(proc->uses.size());

  for (int i = 0; i < proc->defs.size(); i++) {
    if (redir[i] != MAX_U32) {
      if (proc->defs[i]) {
        new_defs[redir[i]] = defs[i];
      }

      if (proc->uses[i]) {
        new_uses[redir[i]] = uses[i];
      }
    }
  }

  proc->defs.swap(new_defs);
  proc->uses.swap(new_uses);
}
