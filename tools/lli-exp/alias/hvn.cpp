class HVN {
  OfflineGraph *graph;
  u32 next_ptr_eq;
  u32 curr_dfs;
  std::stack<u32> dfs_stack;
  bool do_union;
  
  HVN(bool do_union, u32 last_object_node) : do_union(do_union) : curr_dfs(1) {
    graph = make_offline_graph_1(last_object_node);
    next_ptr_eq = nodes.size();
  }
};

void HVN::run() {
  hvn_add_constraint_edges(this->graph);

  for (u32 i = this->graph->firstAFP; i < this->graph->firstREF; i++) {
    if (!graph->nodes[i].dfs_num) {
      this->dfs(i);
    }
  }

  assert(this->dfs_stack.empty());

  this->merge_equal_pointers();
}

void HVN::add_constraint_edges() {
  u32 num_copy = 0, num_load = 0, num_store = 0;
  u32 num_impl_addr = 0, n_impl_copy = 0;

  for (int i = 0; i < constraints.size(); i++) {
    const Constraint &c = constraints[i];

    assert(nodes[c.dest]->is_rep());
    assert(nodes[c.src]->is_rep());

    u32 object_dest_node = graph->main_offset[c.dest];
    u32 object_src_node = graph->main_offset[c.src];

    if (!object_dest_node) {
      assert(nodes[c.dest]->obj_sz);
      continue;      
    }

    assert(object_src_node || nodes[c.src]->obj_sz);

    switch (c.type) {
    case ConstraintAddrOf:
      graph->nodes[object_dest_node].ptr_eq.set(c.src);
      
      if (object_src_node) {
        u32 refd = graph->ref(object_dest_node);
        graph->nodes[refd].impl_edges.set(object_src_node);
      }

      break;
    case ConstraintCopy:
      if (object_src_node) {
        graph->nodes[object_dest_node].edges.set(object_src_node);
        num_copy++;

        u32 refd = graph->ref(object_dest_node);
        u32 refs = graph->ref(object_src_node);
        graph->nodes[refd].impl_edges.set(refs);
        num_impl_copy++;

        break;
      }

      graph->nodes[object_dest_node].indirect = true;
      
      break;
    case ConstraintLoad:
      if (c.off) {
        graph->nodes[object_dest_node].indirect = true;

        break;
      }

      assert(object_src_node);

      u32 refs = graph->ref(object_src_node);
      graph->nodes[object_dest_node].edges.set(refs);
      num_load++;

      break;
    case ConstraintStore:
      if (!c.off) {
        assert(object_src_node);

        u32 refd = graph->ref(object_dest_node);
        graph->nodes[refd].edges.set(object_src_node);
        num_store++;
      }

      break;
    case ConstraintGEP:
      u32 pe;
      std::pair<u32, u32> r(c.src, c.off);
      std::hash_map<std::pair<u32, u32, u32>::const_iterator iter =
                    graph->gep_labels.find(r);
      if (iter != graph->gep_label.end()) {
        pe = iter->second;
      } else {
        graph->gep_labels[r] = pe = next_ptr_eq++;
      }

      graph->nodes[object_dest_node].ptr_eq.set(pe);

      break;
    default:
      assert(false && "unknown constraint type");
    }
  }
}

void HVN::dfs(u32 i) {
}

void HVN::merge_equal_pointers() {
}
