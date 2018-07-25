#include "offline.h"

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
      break;
    case ConstraintCopy:
      OfflineNode &dest_node = og->nodes[dest];
      dest_node.edges.set(object_src_node);
      num_copy++;
      
      break;
    case ConstraintLoad:
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
      break;
    default:
      assert(false && "unknown constraint type");
    }
  }
}

static void map_offline_nodes(OfflineGraph *og) {
  for (int i = 0; i < og->offsets.size(); i++) {
    u32 index = og->offsets[i];
    if (index) {
      og->nodes[index].main = index;
      
      u32 ref_index = offline.ref(index);
      og->nodes[ref_index].main = index;
    }
  }
}

static void do_dfs_at(OfflineGraph *og,
                      std::stack<u32> *dfs_stack,
                      u32 index) {

  assert(index);

  OfflineNode *node = &og->nodes[index];
  assert(!node->scc_root && node->is_rep());

  u32 orig_dfs = og->dfs_num++;
  node->dfs_num = orig_dfs;

  // Look for SCCs.
  for (bitmap::iterator i = node->edges.begin(), e = node->edges.end();
       i != e; i++) {
    const OfflineNode *edge_node = &og->nodes[*i];
    assert(edge_node->is_rep());

    if (edge_node->scc_root) {
      continue;
    }

    if (!edge_node->dfs_num) {
      do_dfs_at(og, dfs_stack, *i);
    }

    if (edge_node->dfs_num < node->dfs_num) {
      node->dfs_num = edge_node->dfs_num;
    }
  }

  assert(node->is_rep());


  // Check if this node is the root of an scc.
  if (node->dfs_num != orig_dfs) {
    // Nope.
    dfs_stack->push(index);
    return;
  }

  // Record all of the nodes in our SCC.
  std::vector<u32> scc(1, index);
  std::vector<u23> var_nodes;
  if (index < og->first_dereference_node) {
    var_nodes.push_back(index);
  }

  // Find the root.
  while (!dfs_stack->empty()) {
    u32 top = dfs_stack->top();
    assert(top != index);

    if (og->nodes[top].dfs_num < orig_dfs) {
      // Anything visited before us should remain on the stack.
      break;
    }

    dfs_stack->pop();
    succ.push_back(top);
    if (top < og->first_dereference_node) {
      var_nodes.push_back(top);
    }
  }

  if (scc.size() == 1) {
    node->scc_root = true;
    return;
  }

  assert(var_nodes.size());

  u32 var_rep = og->nodes[var_nodes[0]].main;
  for (u32 i = 1, e = var_nodes.size(); i != e; i++) {
    u32 main = og->nodes[var_nodes[i]].main;
    var_nodes = as->nodes->merge(var_rep, main, &as->deref_to_var_nodes);
  }

  for (int i = 0; i < scc.size(); i++) {
    u32 scc_index = scc[i];
    assert(scc_index);

    OfflineNode *scc_node = &og->nodes[scc_index];
    scc_node->scc_root = true;

    if (scc_index >= og->first_dereference_node) {
      as->deref_to_var_nodes[scc_node->main] = var_rep;
    }
  }
}

static void update_constraints_and_indirect_calls(AnalysisSet *as) {
  std::vector<Constraint> old;
  old.swap(as->constraints);
  
  llvm::DenseSet<Constraint> seen;
  for (int i = 0; i < old.size(); i++) {
    const Constraint &c = old[i];

    u32 dest = as->nodes->rep(c.dest);
    u32 src = c.src;

    if (c.type != ConstraintAddrOf) {
      src = as->nodes->rep(src);
    }

    if (!seen.count(c) &&
        (c.type != ConstraintCopy ||
         c.dest != c.src)) {
      seen.insert(c);
      as->constraints.push_back(c);
    }
  }

  std::vector<ConstraintInstPair> old_indirect_cons;
  for (ConstraintInstMap::iterator i = as->indirect_constraints.begin(),
         e = as->indirect_constraints.end(); i != e; i++) {
    // Collect all indirect calls.
    old_indirect_cons.push_back(*i);
  }

  as->indirect_constraints.clear();
  
  for (int i = 0; i < old_indirect_cons.size(); i++) {
    Constraint &c = old_indirect_cons[i].first;

    c.dest = as->nodes->rep(c.dest);
    c.src = as->nodes->rep(c.src);

    const std::set<llvm::Instruction *> &inst = old_indirect_cons[i].second;
    as->indirect_constraints[c].insert(inst.begin(), inst.end());
  }    
}

static u32 hcd_start(OfflineGraph *og) {
  return og->first_func_param_node;
}

static u32 hcd_end(OfflineGraph *og) {
  return og->first_dereference_node + og->num_dereference_nodes;
}

static void do_hcd(AnalysisSet *as,
                     u32 last_obj) {

  // Step 1.
  //
  //
  OfflineGraph offline(as->nodes->nodes, last_obj);

  // Step 2.
  //
  //
  add_constraint_edges(as, &offline);

  // Step 3.
  //
  //
  map_offline_nodes(&offline);

  // Step 4.
  //
  //
  std::stack<u32> dfs_stack;  
  for (u32 i = hcd_start(&offline); i < hcd_end(&offline); i++) {
    if (!offline.nodes[i].dfs_num) {
      do_dfs_at(&offline, &dfs_stack, i);
    }
  }
  assert(dfs_stack.empty());

  // Step 5.
  //
  //
  update_constraints_and_indirect_calls();
}
