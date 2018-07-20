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

  OfflineNode() :
    del(false),
    rep(MAX_U32),
    dfs_num(0),
    idr(false) {}
};

typedef std::hash_map<std::pair<u32, u32>, u32>::const_iterator gep_label_iterator;

class OfflineGraph {
  std::vector<OfflineNode> nodes;
  std::vector<u32> offsets;
  u32 next_label;
  u32 dfs_num;
  std::stack<u32> node_stack;
  
  std::hash_map<std::pair<u32, u32>, u32> gep_labels;

  u32 nVAL, nAFP, nREF, firstVAL, firstAFP, firstREF;

 OfflineGraph(size) : next_label(1), dfs_num(1) {
    nodes.assign(size, OfflineNode());
  }

  u32 ref(u32 i) {
    return p - firstAFP + firstREF;
  }
};

class HVN {
  AnalysisSet *as;
  OfflineGraph *offline_graph;
  u32 next_ptr_eq;
  u32 curr_dfs;
  std::stack<u32> dfs_stack;
  bool do_union;
  
  HVN(AnalysisSet *as, bool do_union) : do_union(do_union) : curr_dfs(1) {
    graph = make_graph(as);
    next_ptr_eq = as->nodes.size();
  }
  
  HVN(AnalysisSet *as) {
    HVN(as, false);
  }

  ~HVN() {
    delete graph;
    dfs_stack.clear();
  }

  OfflineGraph *make_graph(AnalysisSet *as);
  void run();
};
