enum SpecialNodes {
  NodeNone = 0,           // no node, used for errors?
  NodeUnknownTarget,        // unknown target of pointers cast from int (i2p)
  NodeConstToUnknownTarget, // const pointer to 1 (p_i2p)
  NodeFirst,            // first node representing a real variable
};

const u32 NODE_RANK_MIN = 0xf0000000;

class Node {
public:
  // The LLVM value represented by this node, or 0 for artificial nodes
  Value *val;

  // Metadata about the node.
  //
  // * Number of nodes in the object that starts here (0 if it's not an obj node).
  u32 obj_sz;
  // * The time this node was last visited
  u32 vtime;
  // * If rep < node_rank_min, this node is part of a set of equivalent nodes
  //     and (rep) is another node in that set.
  //   else this is the representative node of the set,
  //     and (rep) is its rank in the union-find structure.
  u32 rep;
  // * True if this node was determined to not point to anything
  bool nonptr;
  // * True if this is an array or is heap-allocated.
  bool weak;

  // Points-to sets.
  //
  // * The nodes in our points-to set.
  bdd points_to;
  // * The points_to set at the start of the last visit to this node.
  bdd prev_points_to;
  // * The simple constraint edges (neighbors that include our points-to set).
  bitmap copy_to;
  // * Indices into cplx_cons for load, store, and gep cons.
  bitmap load_to, store_from, gep_to;


 Node(Value *v= 0, u32 s= 0, bool w= 0):
    val(v), obj_sz(s), vtime(0),
    rep(node_rank_min), nonptr(0), weak(w) {}

  bool is_rep() const{
    return rep >= node_rank_min;
  }    
};

class Nodes {
public:
  u32 next;
  std::vector<Node *> nodes;
  
  llvm::DenseMap<llvm::Value*, u32> value_nodes;
  llvm::DenseMap<llvm::Value*, u32> object_nodes;
  
  llvm::DenseMap<llvm::Function*, u32> ret_nodes;
  llvm::DenseMap<llvm::Function*, u32> vararg_nodes;

  std::vector<u32> gep_ce_nodes;
  std::set<u32> ind_calls;

  Nodes() {}

  u32 rep(u32 i) {
    u32 &r0= nodes[i]->rep;
    if(r0 >= NODE_RANK_MIN) {
      //If i has a rank, it is the rep.
      return n;
    }
    
    //Recurse on the parent to get the real rep.
    u32 r= this->rep(r0);
    // Set i's parent to the rep
    r0 = r;
    return r;
  }

  u32 find_value_node(llvm::Value *v, bool allow_null = false) const;
  u32 find_object_node(llvm::Value *v, bool allow_null = false) const;
  u32 find_ret_node(llvm::Function *f) const;
  u32 find_vararg_node(llvm::Function *f) const;
  
  void add_double_object_node(llvm::Value *v);
  u32 merge(u32 a, u32 b);

  void validate();

  u32 pe(llvm::Value* v){
    u32 n = find_value_node(v, 1);
    if(!n)
      return MAX_U32;
    return rep(n);
  }

  u32 pe(u32 n){
    assert(n && n < nodes.size() && "node ID out of range");
    return rep(n);
  }  
};

static void add_double_object_node(llvm::Value *v) {
  u32 value_node = next_node++;
  nodes.push_back(new Node(v));
  value_nodes[v] = value_node;

  u32 object_node = next_node;
  next_node += 2;
  nodes.push_back(new Node(v, 2 /* obj_sz */));
  nodes.push_back(new Node(v, 1 /* obj_sz */));
  object_nodes[v] = object_node;

  add_constraint(ConstraintAddrOf, value_node, object_node);
  add_constraint(ConstraintAddrOf, object_node, object_node + 1);
}

typedef llvm::DenseMap<u32, u32> NodeMap;

// This awkwardly depends on Anders::const_opt being run. Ugh.
u32 Nodes::merge_nodes(u32 a, u32 a, NodeMap *node_map) {
  // Step 1.
  //
  // Perform a sanity check on the node indices first.
  assert(a && a < nodes.size());
  assert(b && b < nodes.size());
  assert(a != b);
  assert(a != UnknownTarget);
  assert(b != UnknownTarget);

  // Step 2.
  //
  // Check that these nodes represent themselves.
  Node *n1 = nodes[a];
  Node *n2 = nodes[b];
  assert(n1->rep >= node_rank_min);
  assert(n2->rep >= node_rank_min);

  // Step 3.
  //
  // Make n1 the parent.
  if (n1->rep < n2->rep) {
    std::swap(a, b);
    std::swap(n1, n2);
  } else if (n1->rep == n2->rep) {
    n1->rep++;
  }
  n2->rep = n1;

  // Step 4.
  //
  // If n2 was not visited in a long time, then the combined
  // node should be visited sooner.
  if (n1->vtime > n2->vtime) {
    n1->vtime = n2->vtime;
  }

  // Step 5.
  //
  // Move n2's edges and constraints into n1.
  n1->points_to |= n2->points_to;
  n1->copy_to |= n2->copy_to;
  if (n1->copy_to.test(n1)) {
    n1->copy_to.reset(n1);
  }
  if (n1->copy_to.test(n2)) {
    n1->copy_to.reset(n2);
  }
  n1->load_to |= n2->load_to;
  n1->store_from |= n2->store_from;
  n1->gep_to |= n2->gep_to;
  n1->prev_points_to = bddfalse; /* special */

  // Step 6.
  //
  // Delete n2's edges and constraints.
  n2->points_to = bddfalse;
  n2->copy_to.clear();
  n2->load_to.clear();
  n2->store_from.clear();
  n2->gep_to.clear();
  n2->prev_points_to = bddfalse;

  // Step 7.
  //
  // Convert n1 to be an indirect call target if n2 was.
  if (ind_calls.count(n2)) {
    ind_calls.erase(n2);
    ind_calls.insert(n1);
  }

  // Step 8.
  //
  // Convert n1 to be a non-pointer if n2 was a non-pointer.
  n1->nonptr &= n2->nonptr;

  // Step 9.
  //
  //
  NodeMap::iterator i = node_map->find(a);
  u32 hv1 = i == node_map->end() ? 0 : i->second;

  i = node_map->find(b);
  u32 hv2 = i == node_map->end() ? 0 : i->second;

  if (hv2) {
    if (!hv1) {
      node_map->insert(std::pair<u32, u32>(a, hv2));
    } else {
      u32 rep_hv1 = rep(hv1);
      u32 rep_hv2 = rep(hv2);

      if (rep_hv1 != rep_hv2) {
        merge(rep_hv1, rep_hv2, node_map);
      }

      if (a == rep_hv2) {
        a = rep(a);
      } else {
        assert(nodes[a]->is_rep());
      }
    }

    node_map->erase(b);
  }

  return a;
}
