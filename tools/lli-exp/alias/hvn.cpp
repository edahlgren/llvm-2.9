static void add_constraint_edges(AnalysisSet *as,
                                 OfflineGraph *og) {

  u32 num_copy = 0, num_load = 0, num_store = 0;
  u32 num_impl_addr = 0, n_impl_copy = 0;

  for (int i = 0; i < as->constraints.size(); i++) {
    const Constraint &c = as->constraints[i];

    assert(as->nodes[c.dest]->is_rep());
    assert(as->nodes[c.src]->is_rep());

    u32 dest = og->offsets[c.dest];
    u32 src = og->offsets[c.src];

    if (!dest) {
      assert(as->nodes[c.dest]->obj_sz);
      continue;      
    }

    assert(src || nodes[c.src]->obj_sz);

    switch (c.type) {
    case ConstraintAddrOf:
      OfflineNode &dest_node = og->nodes[dest];
      dest_node.ptr_eq.set(c.src);

      if (src) {
        u32 dest_ref = og->ref(dest);
        OfflineNode &dest_ref_node = graph->nodes[dest_ref];
        dest_ref_node.impl_edges.set(src);
      }

      break;
    case ConstraintCopy:
      if (!src) {
        OfflineNode &dest_node = og->nodes[dest];
        dest_node.indirect = true;

        break;
      }

      OfflineNode &dest_node = og->nodes[dest];
      dest_node.edges.set(object_src_node);
      num_copy++;

      u32 dest_ref = og->ref(dest);
      u32 src_ref = og->ref(src);
      
      OfflineNode &dest_ref_node = og->nodes[dest_ref];
      dest_ref_node.impl_edges.set(src_ref);
      num_impl_copy++;
      
      break;
    case ConstraintLoad:
      if (c.off) {
        OfflineNode &dest_node = og->nodes[dest];
        dest_node.indirect = true;

        break;
      }

      assert(src);

      u32 src_ref = og->ref(src);
      OfflineNode &dest_node = og->nodes[dest];
      dest_nodes.edges.set(src_ref);
      num_load++;

      break;
    case ConstraintStore:
      if (!c.off) {
        assert(src);

        u32 dest_ref = og->ref(dest);
        OfflineNode &dest_ref_node = og->nodes[dest_ref];
        dest_ref_node.edges.set(src);
        num_store++;
      }

      break;
    case ConstraintGEP:
      std::pair<u32, u32> r(c.src, c.off);
      std::hash_map<std::pair<u32, u32, u32>::const_iterator iter =
                    og->gep_labels.find(r);

      u32 pe;
      if (iter != og->gep_label.end()) {
        pe = iter->second;
      } else {
        og->gep_labels[r] = pe = og->next_label++;
      }

      OfflineNode &dest_node = og->nodes[dest];
      dest_node.ptr_eq.set(pe);

      break;
    default:
      assert(false && "unknown constraint type");
    }
  }
}

static void label_node_at(OfflineGraph *og,
                          u32 index) {

  OfflineNode *node = &og->nodes[index];
  assert(node->is_rep() && node->scc_root);
  
  bitmap &pe = node->ptr_eq;
  if (node->indirect) {
    // Give indirect nodes new labels.
    pe.clear();
    pe.set(og->next_label++);
    return;
  }

  for (bitmap::iterator i = node->edges.begin(), e = node->edges.end();
       i != e; i++) {
    u32 rep_index = og->rep(*i);
    if (rep_index == index) {
      continue;
    }

    bitmap &pe_rep = og->nodes[rep_index].ptr_eq;
    assert(!pe_rep.empty(), "unlabeled neighbor");

    // Ignore non-ptr neighbors.
    if (!pe_rep.test(0)) {
      pe |= pe_rep;
    }
  }

  if (pe.empty()) {
    // If a direct node has no incoming labels, it's a non-pointer.
    pe.set(0);
  } else if (!pe.single_bit_p()) {
    std::hash_map<bitmap, u32>::const_iterator i =
      og->label_to_ptr.find(pe);

    if (i == og->label_to_ptr.end()) {
      og->label_to_ptr[pe] = og->next_label;
      pe.clear();
      pe.set(og->next_label++);
    } else {
      pe.clear();
      pe.set(i->second);
    }    
  }

  //If there was only 1, keep it.
  assert(node->ptr_eq.single_bit_p());
}

void check_edge(OfflineGraph *og,
                std::stack<u32> *dfs_stack,
                u32 index, u32 dest);

static void do_dfs_at(OfflineGraph *og,
                      std::stack<u32> *dfs_stack,
                      u32 index) {
  
  assert(index);
  
  OfflineNode *node = &og[index];
  assert(!node->scc_root && node->is_rep());

  u32 orig_dfs = hvn->curr_dfs++;
  node->dfs_num = dfs;

  // Look for SCCs.
  for (bitmap::iterator i = node->edges.begin(), e = node->edges.end();
       i != e; i++) {
    // This recurses into do_dfs.
    check_edge(og, dfs_stack, index, *i);
  }
  for (bitmap::iterator i = node->impl_edges.begin(), e = node->impl_edges.end();
       i != e; i++) {
    check_edge(og, dfs_stack, index, *i);
  }
  assert(node->is_rep());

  // Check if this node is the root of an scc.
  if (node->dfs_num != orig_dfs) {
    // Nope.
    dfs_stack->push(index);
    return;
  }

  // Find the root.
  while (!dfs_stack->empty()) {
    u32 top = dfs_stack->top();

    if (og->nodes[top].dfs_num < orig_dfs) {
      // Keep anything visited before on the stack.
      break;
    }

    dfs_stack->pop();
    index = og->merge(index, top);
  }

  assert(index);
  node = &og->nodes[index];

  // Label it as such.
  assert(node->is_rep());
  node->scc_root = true;  
  label_node_at(hvn, index);    
}

void check_edge(OfflineGraph *og,
                std::stack<u32> *dfs_stack,
                u32 index, u32 dest) {
  
  OfflineNode *node = og->nodes[index];
  assert(node->is_rep());

  u32 rep_index = og->rep(index);
  const OfflineNode *rep_node = &og->nodes[rep_index];
  if (rep_node == node || rep_node->scc_root) {
    // Skip this if it was merged into the node or is a collapsed scc.    
    return;
  }
  
  if (!rep_node->dfs_num) {
    // If it's unvisited, continue with the dfs.
    do_dfs_at(og, dfs_stack, rep_node);
  }
  
  if (rep_node->dfs_num < node->dfs_num) {
    // Set our dfs_num to the minimum reachable number.
    node->dfs_num = rep_node->dfs_num;
  }

  // Check that we still represent ourselves.
  assert(node->rep());
}

static void merge_equal_pointers(AnalysisSet *as,
                                 OfflineGraph *og) {
  
  std::vector<Node *> &nodes  = as->nodes->nodes;

  // Step 1.
  //
  //
  u32 num_nodes = nodes.size();
  std::hash_map>bitmap, u32> ptr_to_node;
  
  for (int i = 0; i < num_nodes; i++) {
    u32 offset = og->offsets[i];
    if (!offset) {
      // If this node has no offline version, it's not pointer
      // equivalent.
      continue;
    }

    bitmap &pe = og->nodes[og->rep(offset)].ptr_eq;
    assert(!pe.empty());

    if (pe.test(0)) {
      // Delete non-pointer nodes from the constraints.
      assert(pe.single_bit_p());
      nodes[i]->nonptr = true;
      continue;
    }

    // Make sure that anything previously marked as non-pointer
    // doesn't have another label.
    assert(!nodes[i]->nonptr);

    std::hash_map<bitmap, u32>::iterator iter = ptr_to_node.find(pe);
    if (iter == ptr_to_node.end()) {
      // Not seen, so i is its first node.
      ptr_to_node[pe] = i;
      assert(nodes[i]->is_rep());
      continue;
    }

    // Merge i into the node representing pe.
    iter->second = as->nodes->merge(iter->second, i);
  }

  // Step 2.
  //
  //
  std::vector<Constraint> old;
  old.swap(constraints);

  llvm::DenseSet<Constraint> seen;
  for (int i = 0; i < old.siz(); i++) {
    Constraint &c = old[i];

    if (nodes[c.dest]->nonptr ||
        nodes[c.src]->nonptr) {
      // Ignore constraint if either side is a non-ptr.
      continue;
    }

    // Replace the destination.
    c.dest = as->nodes->rep(c.dest);

    if (c.type != ConstraintAddrOf) {
      // Don't replace the source of AddrOf by the rep.
      c.src = as->nodes->rep(c.src);
    }

    if (!seen.count(c) &&
        (c.type != ConstraintCopy ||
         c.dest != c.src)) {
      // Ignore (copy x x) and duplicates.
      seen.insert(c);
      as->constraints.push_back(c);
    }
  }

  // Step 3.
  //
  // Rewrite icall_cons to refer to the rep nodes.
  std::vector<ConstraintInstPair> old_indirect_cons;
  for (ConstraintInstMap::iterator i = as->indirect_constraints.begin(),
         e = as->indirect_constraints.end(); i != e; i++) {
    // Collect all indirect calls.
    old_indirect_cons.push_back(*i);
  }

  // Why does this have such a drastic side effect?
  as->indirect_constraints.clear();

  for (int i = 0; i < old_indirect_cons.size(); i++) {
    Constraint &c = old_indirect_cons[i].first;

    if (nodes[c.dest]->nonptr ||
        nodes[c.src]->nonptr) {
      // Same as above.
      continue;
    }

    c.dest = as->nodes->rep(c.dest);
    c.src = as->nodes->rep(c.src);

    const std::set<llvm::Instruction *> &inst = old_indirect_cons[i].second;
    as->indirect_constraints[c].insert(inst.begin(), inst.end());
  }
}

static u32 hvn_start(OfflineGraph *og) {
  return og->first_func_param_node;
}

static u32 hvn_end(OfflineGraph *og) {
  return og->first_dereference_node + og->num_dereference_nodes;
}

static void __do_hvn(AnalysisSet *as,
                     u32 last_obj,
                     bool with_union) {

  OfflineGraph offline(as->nodes->nodes, last_obj);

  add_constraint_edges(as, &offline);

  std::stack<u32> dfs_stack;  
  for (u32 i = hvn_start(og); i < hvn_end(og); i++) {
    if (!og->nodes[i].dfs_num) {
      do_dfs_at(&offline, &dfs_stack, i);
    }
  }
  assert(dfs_stack.empty());

  merge_equal_pointers(as, &offline);
}

void do_hvn(AnalysisSet *as, u32 last_obj) {
  __do_hvn(as, last_obj, false);
}

static void __do_hr(AnalysisSet *as,
                    u32 last_obj,
                    bool with_union,
                    u32 threshold) {

  u32 prev = 0, curr = as->constraints.size();

  do {
    __do_hvn(as, last_obj, with_union);
    
    prev = curr;
    curr = as->constraints.size();
    
  } while (prev - curr >= threshold);
}

void do_hr(AnalysisSet *as, u32 last_obj, u32 threshold = 100) {
  __do_hr(as, last_obj, false);
}

void do_hru(AnalysisSet *as, u32 last_obj, u32 threshold = 100) {
  __do_hr(as, last_obj, true);
}
