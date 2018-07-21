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

// This awkwardly depends on Anders::const_opt being run. Ugh.
static u32 merge_nodes(u32 a, u32 a) {
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
  llvm::DenseMap<u32, u32>::iterator ihv
}

class ExtInfo {
  // Each function name is mapped to its extf_t.
  llvm::StringMap<ExternalFunctionType> info;

  // Cache of is_ext results for all functions.
  std::hash_map<const llvm::Function *, bool> cache;

  ExtInfo() {
    std::set<ExternalFunctionType> seen;
    ExternalFunctionType prev = EFT_NOOP;
    seen.insert(prev);
    for (const ei_pair *p = external_function_database; p->n; ++p) {
      if (p->t != prev) {
        assert(!seen.count(p->t));
        seen.insert(p->t);
        prev = p->t;
      }
      
      assert(!info.count(p->n));
      info[p->n] = p->t;
    }
    
    cache.clear();
  }

  extf_t get_type(const Function *f) const {
    llvm::StringMap<ExternalFunctionType>::const_iterator i =
      info.find(f->getNameStart());
    if (i == info.end())
      return EFT_OTHER;
    else
      return i->second;
  }

  bool has_static(const Function *F) const;
  bool has_static2(const Function *F) const;
  bool is_alloc(const Function *F) const;
  bool no_struct_alloc(const Function *F) const;
  bool is_noop(const Function *F) const;
  bool is_ext(const Function *F) {
    //Check the cache first; everything below is slower.
    std::hash_map<const Function *, bool>::iterator i = this->cache.find(f);
    if (i != this->cache.end()) {
      return i->second;
    }
  
    bool res;
    if(f->isDeclaration() || f->isIntrinsic()) {
      res = true;
    } else {
      ExternalFunctionType t = get_type(f);
      res = t == EFT_ALLOC
        || t == EFT_REALLOC
        || t == EFT_NOSTRUCT_ALLOC
        || t == EFT_NOOP;
    }
    this->cache[f] = res;
    return res;
  }
};
