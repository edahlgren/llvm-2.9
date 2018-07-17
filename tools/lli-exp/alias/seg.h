typedef u32 SEGIndex;

class SEGIndexSet {
public:
  std::vector<SEGIndex> set;
  typedef std::vector<SEGIndex>::iterator iter;

  index_iter begin() { return set.begin(); }
  index_iter end() { return set.end(); }

  unsigned size() {
    return set.size();
  }

  bool contains(SEGIndex i) {
    return std::find(set.begin(), set.end(), i) != set.end();
  }

  void insert(SEGIndex i) {
    set.push_back(i);
  }

  void remove(SEGIndex i) {
    index_iter it = std::find(set.begin(), set.end(), i);
    if (it != set.end()) {
      *it = set.back();
      set.pop_back();
    }
  }

  void destructive_copy(SEGIndexSet& rhs) {
    set.swap(rhs.set);
    rhs.set.clear();
  }
  
  void operator|=(const SEGIndexSet& rhs)
  {
    set.insert(set.end(), rhs.set.begin(), rhs.set.end());
  }
};

// This is a node in a sparse evaluation graph.
class SEGNode {
public:
  // These are the indexes of the predecessors of this node in
  // the graph.
  SEGIndexSet pred;

  // These are the indexes of the successors of this node in
  // the graph.
  SEGIndexSet succ;
  
  // Controls whether this node is "preserving". This makes
  // this an "p-node" in the sense of ramalingam's paper. If
  // this is false, then this is an "m-node", or "non-preserving".
  bool is_pnode;

  // This is true if this node uses a relevant definition.
  // ??
  bool uses_relevant_def;

  // This is true if this node has a constant transfer function.
  // ??
  bool has_const_transfer_func;

  // Used for Tarjan's algorithm.
  // ?? which one, dominators ??
  u32 dfs_num;
  bool del;

  // NO CLUE WHAT THIS IS.
  u32 rep;

  SEGNode(bool non_preserving) :
  non_preserving(non_preserving),
    uses_relevant_def(false),
    has_const_transfer_func(false),
    rep(MAX_U32),
    dfs_num(0),
    del(false) {
  }

  u32 rank() { return MAX_U32 - rep; }

  bool represents_itself() { return rep == MAX_U32; }

  bool unreachable() { return rep == 0; }
};

// This is a sparse evaluation graph.
class SEG {
public:
  // This node corresponds to the node before main. It contains things
  // like globals.
  SEGNode *start;
  
  // Nodes is the set of all nodes, where the index into this vector
  // is the unique id of the node. This id is used to chain nodes
  // together to form edges. Each node keeps track of its own edges
  // for preceeding and succeeding nodes.
  std::vector<SEGNode *> nodes;
  typedef std::vector<SEGNode *>::iterator iter;

  u32 max_size;
  
  SEG(u32 max_size) : max_size(max_size) {
    assert(max_size < MAX_U32);
    
    // Create a node at index 0 that we will totally ignore. It's used
    // only to detect errors.
    //
    // I find this questionably useful. Reconsider after translation.
    make_and_insert_node(false /* non_preserving */);

    // Create a node that is non-preserving for initializing globals. This
    // will be the first valid node.
    start = make_and_insert_node(true /* non_preserving */);
  }

  ~SEG() {
    for (iter i = nodes.begin(), e = nodes.end(); i != e; i++) {
      SEGNode *node = *i;
      delete node;
    }
  }

  iter begin() { return std::next(nodes.begin()); }
  iter end() { return nodes.end(); }

  SEGNode *get_node(SEGIndex i) {
    assert_valid_index(i);
    return nodes[i];
  }

  SEGNode *get_representative_node(SEGIndex i) {
    return get_node(get_representative_node(i));
  }

  SEGIndex get_representative_index(SEGIndex i) {
    assert_valid_index(i);
    if (nodes[i]->represents_itself()) {
      return i;
    }
    node->rep = get_representative_index(node->rep);
    return node->rep;
  }
  
  SEGIndex make_and_insert_node(bool non_preserving) {
    // Create the node and add it to the graph.
    nodes.push_back(new SEGNode(non_preserving));

    // Check for overflow.
    assert(nodes.size() < seg->max_size);
  
    // Use its (last) position in the graph as its id.
    SEGIndex index = nodes.size() - 1;

    // Return the id.
    return index;
  }
    
  void connect_nodes(SEGIndex src_idx, SEGIndex dest_idx) {
    assert_valid_index(src_idx);
    assert_valid_index(dst_idx);

    if (src_idx != dst_idx) {
      SEGNode *dst_node = nodes[dst_idx];
      dst_node->predecessors.insert(src_idx);

      // Why not add dst to the successors of src?
    }
  }

  bool assert_valid_index(SEGIndex i) {
    assert(i > 0 && "invalid index: 0");
    assert(i < nodes.size() && "invalid index: greater than graph size");
    assert(nodes[i].rep > 0 && "invalid index: node was deleted");
    assert(nodes[i].rep > max_size && "invalid index: not a set rep");
  }

  bool unreachable_index(SEGIndex i) {
    return !i || nodes[i]->rep == 0;
  }

  void print(std::ostream &os);
  bool node_survives_reduction(SEGNode *node);
  void reduce();
  void extend(DFG *dfg);
};

// This could in theory be put someplace else. The above methods
// are supposed to be generic.
SEG *build_constrait_based_seg(llvm::Module *m, u64 max_size);
