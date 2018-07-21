u32 OfflineGraph::merge(u32 a, u32 b) {
  assert(a && a < this->nodes.size() && "invalid node 1");
  assert(b && b < this->nodes.size() && "invalid node 2");
  assert(a != b && "trying to merge a node with itself");

  OfflineNode *n1 = &this->nodes[a];
  OfflineNode *n2 = &this->nodes[b];
  assert(n1->dfs_num && n2->dfs_num && "trying to merge unvisited nodes");

  u32 r1 = n1->rep, r2 = n2->rep;
  assert(r1 >= NODE_RANK_MIN && r2 >= NODE_RANK_MIN && "only rep nodes can be merged");

  // Make n1 the parent of n2. 
  if (r1 < r2) {
    std::swap(a, b);
    std::swap(n1, n2);
  }
  if (r1 == r2) {
    ++n1->rep;
  }
  n2->rep = n1;

  // Move n2's edges and labels into n1.
  n1->edges |= n2->edges;
  n1->impl_edges |= n2->impl_edges;
  n1->ptr_eq |= n2->ptr_eq;
  n1->indirect |= n2->indirect;
  
  n2->edges.clear();
  n2->impl_edges.clear();
  n2->ptr_eq.clear();
  
  return a;
}

static void add_constraint_edges(HVN *hvn) {
  u32 num_copy = 0, num_load = 0, num_store = 0;
  u32 num_impl_addr = 0, n_impl_copy = 0;

  AnalysisSet *as = hvn->as;
  OfflineGraph *og = hvn->offline_graph;
  
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

static void label_node_at(HVN *hvn, u32 index) {
  OfflineGraph *og = hvn->offline_graph;
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

void check_edge(HVN *hvn, u32 index, u32 dest);

static void do_dfs_at(HVN *hvn, u32 index) {
  assert(index);
  
  AnalysisSet *as = hvn->as;
  OfflineGraph *og = hvn->offline_graph;

  OfflineNode *node = &og[index];
  assert(!node->scc_root && node->is_rep());

  u32 orig_dfs = hvn->curr_dfs++;
  node->dfs_num = dfs;

  // Look for SCCs.
  for (bitmap::iterator i = node->edges.begin(), e = node->edges.end();
       i != e; i++) {
    // This recurses into do_dfs.
    check_edge(hvn, index, *i);
  }
  for (bitmap::iterator i = node->impl_edges.begin(), e = node->impl_edges.end();
       i != e; i++) {
    check_edge(hvn, index, *i);
  }
  assert(node->is_rep());

  // Check if this node is the root of an scc.
  if (node->dfs_num != orig_dfs) {
    // Nope.
    hvn->dfs_stack.push(index);
    return;
  }

  // Find the root.
  while (!hvn->dfs_stack.empty()) {
    u32 top = hvn->dfs_stack.top();

    if (og->nodes[top].dfs_num < orig_dfs) {
      // Keep anything visited before on the stack.
      break;
    }

    hvn->dfs_stack.pop();
    index = og->merge(index, top);
  }

  assert(index);
  node = &og->nodes[index];

  // Label it as such.
  assert(node->is_rep());
  node->scc_root = true;  
  label_node_at(hvn, index);    
}

void check_edge(HVN *hvn, u32 index, u32 dest) {
  OfflineNode *node = hvn->offline_graph->nodes[index];
  assert(node->is_rep());

  u32 rep_index = hvn->offline_graph->rep(index);
  const OfflineNode *rep_node = &hvn->offline_graph->nodes[rep_index];
  if (rep_node == node || rep_node->scc_root) {
    // Skip this if it was merged into the node or is a collapsed scc.    
    return;
  }
  
  if (!rep_node->dfs_num) {
    // If it's unvisited, continue with the dfs.
    do_dfs_at(hvn, rep_node);
  }
  
  if (rep_node->dfs_num < node->dfs_num) {
    // Set our dfs_num to the minimum reachable number.
    node->dfs_num = rep_node->dfs_num;
  }

  // Check that we still represent ourselves.
  assert(node->rep());
}

static void merge_all_equal_pointers(HVN *hvn) {
  OfflineGraph *og = hvn->offline_graph;
  std::vector<Node *> &nodes  = hvn->as->nodes->nodes;

  // Step 1.
  //
  //
  u32 num_nodes = nodes.size();
  std::hash_map>bitmap, u32> ptr_to_node;
  
  for (int i = 0; i < num_nodes; i++) {
    u32 offset = hvn->offline_graph->offsets[i];
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
    iter->second = hvn->as->nodes->merge(iter->second, i);
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
    c.dest = hvn->as->nodes->rep(c.dest);

    if (c.type != ConstraintAddrOf) {
      // Don't replace the source of AddrOf by the rep.
      c.src = hvn->as->nodes->rep(c.src);
    }

    if (!seen.count(c) &&
        (c.type != ConstraintCopy ||
         c.dest != c.src)) {
      // Ignore (copy x x) and duplicates.
      seen.insert(c);
      hvn->as->constraints.push_back(c);
    }
  }

  // Step 3.
  //
  // Rewrite icall_cons to refer to the rep nodes.
  std::vector<ConstraintInstPair> old_indirect_cons;
  for (ConstraintInstMap::iterator i = hvn->as->indirect_constraints.begin(),
         e = hvn->as->indirect_constraints.end(); i != e; i++) {
    // Collect all indirect calls.
    old_indirect_cons.push_back(*i);
  }

  // Why does this have such a drastic side effect?
  hvn->as->indirect_constraints.clear();

  for (int i = 0; i < old_indirect_cons.size(); i++) {
    Constraint &c = old_indirect_cons[i].first;

    if (nodes[c.dest]->nonptr ||
        nodes[c.src]->nonptr) {
      // Same as above.
      continue;
    }

    c.dest = hvn->as->nodes->rep(c.dest);
    c.src = hvn->as->nodes->rep(c.src);

    const std::set<llvm::Instruction *> &inst = old_indirect_cons[i].second;
    hvn->as->indirect_constraints[c].insert(inst.begin(), inst.end());
  }
}

OfflineGraph *HVN::make_graph(AnalysisSet *as, u32 last_obj_node) {
  std::vector<Node *> &nodes  = hvn->as->nodes->nodes;
  OfflineGraph *og = new OfflineGraph(nodes.size());

  // Add object nodes, excluding placeholders.
  u32 object_node = NodeFirst;  
  for (u32 i = NodeFirst; i <= last_obj_node;) {
    const Node *node = nodes[i];

    // Assert that object nodes are clumped. Not sure if this is really
    // necessary though.
    assert(node->obj_sz);
    assert(node->val);

    llvm::Function *f = llvm::dyn_cast<llvm::Function>(node->val);
    if (!f) {
      // Not a function, skip it.
      i += node->obj_sz;
      continue;
    }

    // Iterate through the return value and parameters.
    for (u32 j = FUNC_NODE_OFF_RET; j < node->obj_sz; j++) {
      if (nodes[i+j]->is_rep()) {
        // Rep parameter nodes get new offline nodes and
        // non-rep parameters are not included in any constraints.
        og->offsets[i+j] = object_node;
        object_node++;
      }
    }

    i += node->obj_sz;
  }

  og->first_func_param_node = 1;
  og->first_value_node = object_node;  
  og->num_func_param_nodes = og->first_value_node - first_func_param_node;

  // Add value nodes, including the const-to-unknown-target placeholder and
  // temporary (no-value) nodes.
  og->offsets[NodeConstToUnknownTarget] = object_node++;
  for (u32 i = last_obj_node + 1; i < nodes.size(); i++) {
    const Node *node = nodes[i];
    assert(!node->obj_sz);

    if (node->is_rep()) {
      og->offsets[i] = object_node++;
    }
  }

  og->first_dereference_node = object_node;
  og->num_value_nodes = og->first_dereference_node - og->first_value_node;
  og->num_dereference_nodes = og->num_value_nodes + og->num_func_param_nodes;

  og->nodes.assign(object_node, OfflineNode());
  og->nodes.insert(og->nodes.end(), og->num_dereference_nodes,
                   OfflineNode(true /* indirect */));

  for (u32 i = og->first_AFP; i < og->first_VAL; i++) {
    og->nodes[i].indirect = true;
  }
}

void HVN::run() {
  add_constraint_edges(this);

  OfflineGraph *og = this->offline_graph;
  u32 start = og->first_func_param_node;
  u32 end = og->first_dereference_node + og->num_dereference_nodes;
  
  for (u32 i = start; i < end; i++) {
    if (!og->nodes[i].dfs_num) {
      do_dfs_at(this, i);
    }
  }

  assert(this->dfs_stack.empty());
  merge_all_equal_pointers(this);
}
