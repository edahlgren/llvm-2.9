void rm_unreachable_pnodes(SEG *graph, SEGIndexSet &torder) {
  SEGIndexSet destroy;

  for (SEGIndexSet::iter i = torder.begin(), e = torder.end();
       i != e; ++i) {
    SEGNode *node = graph->get_node(*i);
    assert(!node->non_preserving);

    for (SEGIndex::iter pi = node->pred.begin(), pe = node->pred.end(),
           pi != pe; pi++) {
      SEGNode *pred = graph->get_representative_node(*i);
      if (!pred->del && pred != node) {
        destroy.insert(*i);
      }
    }

    if (!destroy.empty()) {
      node->pred.destructive_copy(destroy);
      continue;
    }

    // This node isn't reachable anymore.
    node->rep = 0;
  }
}

SEGIndex unite(SEG *graph,
               SEGIndex a,
               SEGIndex b,
               Stage stage) {
  assert(stage == T2 || stage == T4 || stage == T5);
  
  SEGNode *node_a = graph->get_node(a);
  assert(node_a->non_preserving);

  SEGNode *node_b = graph->get_node(b);
  assert(node_b->non_preserving);

  if (a == b) {
    return a;
  }
  
  if (stage == T4) {
    if (node_a->rank() == node_b->rank()) {
      node_a->rep--;            
    }
    if (node_a->rank() < node_b->rank()) {
      // Flip the nodes.
      u32 node_t = node_a;
      node_a = node_b;
      node_b = node_a;
    }
  }
  
  if (stage == T2 || stage == T5) {
    if (node_a->rank() < node_b->rank()) {
      node_a->rep = MAX_U32 - node_b->rank();
    }
  }

  node_a->uses_relevant_def |= node_b->uses_relevant_def;
  node_a->has_const_transfer_func |= node_b->has_const_transfer_func;

  if (stage == T2) {
    node_a->pred |= node_b->pred;
  }

  node_b->rep = a;
  return a;  
}

// What the hell is this doing?
SEGIndex get_representative_node(SEG *graph, SEGIndex node_index) {
  graph->assert_valid_index(node_index);
  if (node->rep > graph->max_size) {
    return node_index;
  }
  
  node->rep = get_representative_node(graph, node->rep);
  return node->rep;
}

SEGNode *get_representative_node(SEG *graph, SEGIndex node_index) {
  SEGIndex index = get_representative_node(graph, node_index);
  return graph->get_node(index);
}

class T4Analysis {
  SEGIndexSet &defs;
  SEGIndexSet &torder;
};

int _do_t4(SEG* graph,
        SEGIndex node_index,
        int dfs_num,
        std::stack<SEGIndex> &merge_stack,
        T4Analysis *analysis) {
  int _dfs_num = dfs_num++;
  
  SEGNode *node = graph->nodes[node_index];
  node->dfs_num = _dfs_num;

  for (SEGIndexSet::iter i = node->pred.begin(), e = node->pred.end();
       i != e; i++) {
    SEGIndex pred_index = *i;    
    SEGNode *pred = get_representative_node(graph, pred_index);

    // Is this a non-preserving node?
    if (pred->non_preserving) {
      continue;
    }

    if (!pred->del) {
      if (!pred->dfs_num) {
        dfs_num = _do_t4(graph, pred_index, dfs_num, merge_stack, analysis);
      }
      if (node->dfs_num > pred->dfs_num) {
        node->dfs_num = pred->dfs_num;
      }
    }
  }

  if (node->dfs_num == _dfs_num) {
    while (!merge_stack.empty()) {
      SEGIndex top_index = merge_stack.top();

      graph->assert_valid_index(top_index);
      SEGNode *node = graph->nodes[top_index];
      
      if (top.dfs_num < _dfs_num) {
        break;
      }

      node_index = unite(graph, node_index, top_index, T4);
    }

    analysis->torder.insert(node_index);
    node->del = true;

    return dfs_num;
  }

  merge_stack.push(node);
  return dfs_num;
}         

// This is "transformation 4" from "On Sparse Evaluation Representations"
// by Ramalingam. It collapses each strongly connected component consisting
// only of preserving nodes (!non_preserving) into a single, preserving node.
T4Analysis do_t4(SEG *graph) {
  T4Analysis analysis;
  
  int dfs_num = 1;
  std::stack<SEGIndex> merge_stack;

  for (SEGIndex i = 1; i < graph->nodes.size(); ++i) {
    SEGNode *node = graph->get_node(i);

    // ??
    if (node->rep > graph->max_size) {
      if (!node->non_preserving && !node->dfs_num) {
        dfs_num = _do_t4(graph, i, dfs_num, merge_stack, &analysis);
      }

      // _t4 (via unite) can change the value of rep, so we need to recheck
      // if the condition above holds.

      if (node->rep > graph->max_size) {
        // T6 assumes that all rep nodes have del == true.
        // ??
        node->del = true;

        // The original code uses:
        //
        // G[i].r || (!G[i].c && G[i].np)
        //
        // Basically it allows for either relevant or non-const and
        // non-preserving. This doesn't really make sense to me at all.
        //
        if (node->uses_relevant_def ||
            (!node->has_const_transfer_func && node->non_preserving)) {
          analysis.defs.push_back(node);
        }
      }
    }    
  }

  assert(merge_stack.empty());
  rm_unreachable_pnodes(graph, analysis.torder);
  
  return std::move(analysis);
}

void do_t2(SEG *graph, SEGIndexSet &torder) {
  for (SEGIndexSet::iter i = torder.begin(), e = torder.end();
       i != e; ++i) {
    SEGNode *node = get_representative_node(graph, *i);
    
    // Was the node deleted?
    if (node->rep == 0) {
      continue;
    }
    
    // Unite nodes with singleton predecessors.
    if (node->pred.set.size() == 1) {
      SEGIndex::iter pi = node->pred.begin();
      SEGNodeIndex pred_index = graph->get_representative_node(*pi);

      unite(graph, pred_index, *i, T2);
    }
  }
}
  
void do_t6(SEG *graph, SEGIndexSet &defs) {
}

void SEG::reduce() {
  T4 t4(this);
  t4.run();

  T2 t2(this);
  t2.run(t4.torder);

  T6 t6(this);
  t6.run(t4.rdefs);

  T5 t5(this);
  t5.run(t6.pnode_rdefs);
}

SEG *build_constraint_based_seg(llvm::Module *m, u64 max_size = 1000000000) {
  // Make a new sparse evaluation graph.
  SEG *graph = new SEG(max_size);

  // Walk the module to process its instructions, filling in the graph and
  // collecting constraints.
  Constraints *cs = get_module_anders_constraints(m, graph);

  // Reduce the graph to make it more sparse.
  graph->reduce();

  // Build a data flow graph from the constraints.
  DataFlowGraph *dfg = build_dfg(cs);

  // Do we need the constraints anymore now that they've been
  // incorporated into the dfg?
  delete cs;

  // Partition variables in the dfg. Maybe this can be in dfg_from_constraints
  // above.
  dfg->partition();

  // Incorporate interprocedural edges from the dfg.
  graph->extend(dfg);

  // Get rid of the dfg, we don't need it anymore.
  delete dfg;
  
  // Return the final sparse representation.
  return graph;
}
