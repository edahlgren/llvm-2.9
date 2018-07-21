const u32 NODE_RANK_MIN= 0xf0000000;

class OfflineNode {
  u32 dfs_num;
  u32 rep;
  u32 offset;

  bool del;
  bool indirect;
  bool scc_root;
  
  bitmap edges;
  bitmap impl_edges;
  bitmap labels;
  bitmap ptr_eq;

  OfflineNode() {
    OfflineNode(false);
  }
  
  OfflineNode(bool indirect) :
    del(false),
    rep(MAX_U32),
    dfs_num(0),
    indirect(indirect) {}
};

typedef std::hash_map<std::pair<u32, u32>, u32>::const_iterator gep_label_iterator;

class OfflineGraph {
  std::vector<OfflineNode> nodes;
  std::vector<u32> offsets;
  u32 next_label;
  u32 dfs_num;
  std::stack<u32> node_stack;
  
  std::hash_map<bitmap, u32> label_to_ptr;
  std::hash_map<std::pair<u32, u32>, u32> gep_to_ptr;

  u32 num_value_nodes;
  u32 num_func_param_nodes;
  u32 num_deference_nodes;

  u32 first_value_node;
  u32 first_func_param_node;
  u32 first_dereference_node;

 OfflineGraph(size) : next_label(1), dfs_num(1) {
    nodes.assign(size, OfflineNode());
  }

  u32 ref(u32 i) {
    return p - firstAFP + firstREF;
  }

  u32 rep(u32 i) {
    u32 &r0 = nodes[i].rep;
    if (r0 >= NODE_RANK_MIN) {
      return i;
    }
    u32 = get_rep_node(r0);
    r0 = r;
    return r;
  }
};

class HVN {
  AnalysisSet *as;
  OfflineGraph *offline_graph;
  u32 curr_dfs;
  std::stack<u32> dfs_stack;
  bool do_union;
  
  HVN(AnalysisSet *as, bool do_union, u32 last_object_node) : do_union(do_union) : curr_dfs(1) {
    graph = make_graph(as, last_object_node);
    graph->next_label = as->nodes.size();
  }
  
  HVN(AnalysisSet *as, u32 last_object_node) {
    HVN(as, false, last_object_node);
  }

  ~HVN() {
    delete graph;
    dfs_stack.clear();
  }

  OfflineGraph *make_graph(AnalysisSet *as, u32 last_object_node);
  void run();
};
