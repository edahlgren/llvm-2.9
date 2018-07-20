// Shift nodes that have their address taken to the front of the nodes set,
// which clumps them closely together. This is an optimization.
static u32 move_addr_taken_nodes() {
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

static OfflineGraph *make_offline_graph_1(u32 last_object_node) {  
}

static OfflineGraph *make_offline_graph_2(u32 last_object_node) {
  // Step 1.
  //
  // Intialize a new offline graph that fits all of our nodes.
  OfflineGraph *graph = new OfflineGraph(nodes.size());

  // Step 2.
  //
  // Mark address-taken variables as indirect.
  for (int i = 1, i < last_object_node + 1; i++) {
    graph->nodes[i].indirect = true;
  }

  // Step 3.
  //
  // Populate the graph from the constraints.
  for (int i = 0; i < constraints.size(); i++) {
    const Constraint &c = constraints[i];

    switch (c.type) {
    case ConstraintAddrOf: // D = &S
    case ConstraintLoad:   // D = *S
      graph->nodes[c.dest].indirect = true;
      break;
    case ConstraintStore:  // *D = S
      // Ignore store.
      break;
    case ConstraintCopy:   // D = S
      graph->nodes[c.dest].edges.set(c.src);
    case ConstraintGEP:   // D = S + off
      std::pair<u32, u32> a(c.src, c.off);
      gep_label_iterator j = graph->gep_labels.find(a);
      u32 l = graph->next_label;

      if (j != graph->gep_labels.end()) {
        l = j->second;
      } else {
        graph->gep_labels[a] = l;
        graph->next_label++;
      }

      graph[c.dest].labels.set(l);
      break;
    default:
      assert(false && "unknown constraint type");
    }
  }

  // Step 4.
  //
  // Clear the GEP labels.
  graph->gep_labels.clear();

  return graph;
}

void Constraints::optimize() {
  // Step 1.
  //
  // Clump nodes with address taken variables together at the front of
  // the nodes set so that they'll be easier to solve for.
  u32 last_object_node = move_addr_taken_nodes();

  // Step 2.
  //
  // Apply the HVN algorithm to the constraints.
  {
    HVN hvn(false, last_object_node);
    hvn.run();
  }

  // Step 3.
  //
  // Apply the HRU algorithm to the constraints.
  {
    HR hr();
    hr.run();
  }

  // Step 4.
  //
  // Apply the HCD algorithm to the constraints.
  {
    HCD hcd();
    hcd.run();
  }

  // Step 5.
  //
  // Reduce the load/store constraints
  refactor_load_store();

  // Step 6.
  //
  // Solve for the Anderson points-to sets.
  anderson_solve();
  
  // Step 7.
  //
  // Initialize an offline graph.
  OfflineGraph *offline = make_offline_graph(last_object_node);

  // Step 8.
  //
  // Perform the HU algorithm to detect equivalences.
  {
    HU hu(offline);
    hu.run();
  }

  std::hash_map<bitmap, u32> eq;
  for (int i = 1; i < offline.nodes.size(); i++) {
    Node *node = nodes[i];
    assert(node->is_rep());

    // Step 9.
    //
    // Handle nonptr nodes.
    OfflineNode *offline_node = offline->get_node(i);
    if (offline_node->labels.empty() ||
        offline_node->labels.test(0)) {
      nodes[i]->nonptr = true;
      continue;
    }

    // Step 10.
    //
    // Handle equivalent pointer nodes.
    std::hash_map<bitmap, u32>::iterator j = eq.find(offline_node.labels);
    if (j != eq.end()) {
      // Merge the equivalent nodes.
      merge_nodes(...);
    } else {
      // Save the equivalence relation.
      eq[offline_node.labels] = i;
    }
  }

  // Step 11.
  //
  // Free the temporary structures.
  delete offline;
  eq.clear();

  llvm::DenseSet<llvm::Constraint> seen;
  std::vector<llvm::Constraint> old_cons;
  old_cons.swap(constraints);

  std::vector<u32> redir;
  redir.assign(old_cons.size(), 0);

  for (int i = 0; i < old_cons.size(); i++) {
    const Constraint &oc = old_cons[i];

    if (nodes[oc.dest]->nonptr ||
        nodes[oc.src]->nonptr) {
      redir[i] = MAX_U32;
    }

    if (oc.type != ConstraintStore &&
        oc.type != ConstraintAddrOf // ...) {
      ///...
    }

    redir[i] = constraints.size();
    constraints.push_back(c);
  }

  std::vector<32> new_idr;

  // ...

  std::vector<u32> new_defs(defs.size());
  std::vector<u32> new_uses(uses.size());

  // ...  
  
  redir.clear();
  new_defs.clear();
  new_uses.clear();  
}
