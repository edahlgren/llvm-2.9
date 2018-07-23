static const u32 FACTOR_MIN_SZ = 2;
static const u32 NODE_RANK_MIN = 0xf0000000;

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
  
  u32 num_value_nodes;
  u32 num_func_param_nodes;
  u32 num_deference_nodes;

  u32 first_value_node;
  u32 first_func_param_node;
  u32 first_dereference_node;

  u32 next_label;
  u32 dfs_num;

  std::stack<u32> node_stack;  
  std::hash_map<bitmap, u32> label_to_ptr;
  std::hash_map<std::pair<u32, u32>, u32> gep_to_ptr;

  OfflineGraph(std::vector<Node *> &ns, u32 last_obj) {
    nodes.assign(ns.size(), OfflineNode());

    u32 object_node = NodeFirst;  
    for (u32 i = NodeFirst; i <= last_obj;) {
      const Node *node = ns[i];

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
        if (ns[i+j]->is_rep()) {
          // Rep parameter nodes get new offline nodes and
          // non-rep parameters are not included in any constraints.
          offsets[i+j] = object_node;
          object_node++;
        }
      }

      i += node->obj_sz;
    }

    first_func_param_node = 1;
    first_value_node = object_node;  
    num_func_param_nodes = first_value_node - first_func_param_node;

    // Add value nodes, including the const-to-unknown-target placeholder
    // and temporary (no-value) nodes.
    offsets[NodeConstToUnknownTarget] = object_node++;
    for (u32 i = last_obj + 1; i < ns.size(); i++) {
      const Node *node = ns[i];
      assert(!node->obj_sz);

      if (node->is_rep()) {
        offsets[i] = object_node++;
      }
    }

    first_dereference_node = object_node;
    num_value_nodes = first_dereference_node - first_value_node;
    num_dereference_nodes = num_value_nodes + num_func_param_nodes;

    nodes.assign(object_node, OfflineNode());
    nodes.insert(nodes.end(), num_dereference_nodes,
                     OfflineNode(true /* indirect */));

    for (u32 i = first_func_param_node; i < first_value_node; i++) {
      nodes[i].indirect = true;
    }

    next_label = ns.size();
    dfs_num = 1;
  }

  ~OfflineGraph() {
    // What gets automatically deleted in here?
    nodes.clear();
    offsets.clear();
  }

  u32 ref(u32 i) {
    return p - first_func_param_node + first_dereference_node;
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

// last_obj should be cached in the AnalysisSet, simplifying this type signature.
void do_hvn(AnalysisSet *as, u32 last_obj);

void do_hr(AnalysisSet *as, u32 last_obj, u32 threshold);

void do_hru(AnalysisSet *as, u32 last_obj, u32 threshold);

class HCD {
  AnalysisSet *as;
  u32 curr_dfs;

  std::stack<u32> dfs_stack;
  OfflineGraph *offline_graph;

  HCD(AnalysisSet *as, u32 last_obj) :as(as), curr_dfs(1) {
    offline_graph = new OfflineGraph(as->nodes->nodes, last_obj);
  }

  ~HCD() {
    assert(dfs_stack.empty());
    delete offline_graph;
  }  
};
