#include "external.h"

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
  bool is_ext(const Function *F);
};

bool ExtInfo::is_ext(const Function *f) {
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

static llvm::Argument *get_argv(llvm::Function *f) {
  // There should always be at least one arg, the name of the function.
  assert(f->arg_begin() != f->arg_end());
  
  llvm::Function::arg_iterator argv = f->arg_begin()++;
  if (argv != f->arg_end()) {
    return *argv;
  }
  return nullptr;
}

static Value *get_envp(llvm::Function *f) {
  // There should always be at least one arg, the name of the function.
  assert(f->arg_begin() != f->arg_end());

  llvm::Function::arg_iterator argv = f->arg_begin()++;
  if (argv != f->arg_end()) {
    llvm::Function::arg_iterator envp = argv++;
    if (envp != f->arg_end()) {
      return *envp;
    }
  }
  return nullptr;
}

enum SpecialNodes {
  NoNode = 0,           // no node, used for errors?
  UnknownTarget,        // unknown target of pointers cast from int (i2p)
  ConstToUnknownTarget, // const pointer to 1 (p_i2p)
  FirstNode,            // first node representing a real variable
};

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

// Types of constraints.
enum ConstraintType {
  ConstraintAddrOf = 0, // (Base) Address-of:
                        //   d = &s;
  ConstraintCopy,       // (Simple) Copy:
                        //   d = s;
  ConstraintLoad,       // (Complex 1) Load:
                        //   d = *s (+ off);
  ConstraintStore,      // (Complex 2) Store:
                        //  *d (+ off) = s;
  ConstraintGEP,        // (Copy + offset) GEP:
                        //   d = s + off
};

class Constraint {
public:
  ConstraintType type;
  u32 dest, src, off;

  Constraint(ConsType t, u32 d, u32 s, u32 o = 0) :
    type(t), dest(d), src(s), off(o) {}

  bool operator == (const Constraint &b) const{
    return type == b.type && dest == b.dest && src == b.src && off == b.off;
  }
  
  bool operator < (const Constraint &b) const{
    if(type != b.type)
      return type < b.type;
    if(dest != b.dest)
      return dest < b.dest;
    if(src != b.src)
      return src < b.src;
    return off < b.off;
  }
  
  bool operator > (const Constraint &b) const{
    if(type != b.type)
      return type > b.type;
    if(dest != b.dest)
      return dest > b.dest;
    if(src != b.src)
      return src > b.src;
    return off > b.off;
  }
  
  bool operator != (const Constraint &b) const{
    return !(operator==(b));
  }
  
  bool operator >= (const Constraint &b) const{
    return !(operator<(b));
  }
  
  bool operator <= (const Constraint &b) const{
    return !(operator>(b));
  }
};

static void assert_valid_constraint(Constraint &c) {
  switch (c.type) {
  case ConstraintAddrOf:
    assert(c.dest != UnknownTarget);
    assert(!off);
    break;
  case ConstraintCopy:
    assert(src != UnknownTarget
           && dest != UnknownTarget);
    assert(!off);
    break;
  case ConstraintLoad:
    assert(src != UnknownTarget
           && dest != UnknownTarget
           && src != ConstToUnknownTarget);
    break;
  case ConstraintStore:
    assert(src != UnknownTarget
           && dest != UnknownTarget
           && dest != ConstToUnknownTarget);
    break;
  case ConstraintGEP:
    assert(src != UnknownTarget
           && dest != UnknownTarget);
    break;
  default:
      assert(false && "unknown constraint type");
  }
}

std::vector<Constraint> constraints;

static bool add_constraint(ConsType t, u32 dest, u32 src, u32 off = 0) {
  assert(src && dest);
  
  if (t == ConstraintGEP && !off) {
    t = CopyCons;
  }
  if (t == ConstraintCopy && src == dest) {
    return false;
  }

  Constraint c(t, dest, src, off);
  assert_valid_constraint(c);
  constraints.push_back(c);
  return true;
}
  
// min_struct: 
//   When there are no structs, max_struct is assigned
//   the smallest type.
// max_struct:
//   The struct type with the most fields (or min_struct
//   if no structs are found).
// max_struct_sz:
//   The # of fields in max_struct (0 for min_struct).
const llvm::Type *const min_struct= llvm::Type::Int8Ty;
const llvm::Type* max_struct;
u32 max_struct_sz;

// Every struct type is mapped to vectors S (first) and O (second):
//
// * If field [i] in the expanded struct type begins an embedded struct,
//   then S[i] is the # of fields in the largest such struct, else S[i] = 1.
// * S[0] is always the size of the expanded struct T, since a pointer to
//   the first field of T can mean all of T.
// * If a field has index (j) in the original struct, it has index O[j] in
//   the expanded struct.
typedef llvm::DenseMap<const llvm::StructType*,
                       std::pair<std::vector<u32>, std::vector<u32> >> StructInfoMap;
StructInfoMap struct_info_map;

static void analyze_struct(const llvm::StructType *st) {
  assert(st);
  
  if (struct_info_map.count(st)) {
    // Skip if the struct type is already present.
    return;
  }
  
  u32 num_fields = 0;
  std::vector<u32> sz, off;
  
  for (llvm::StructType::element_iterator i = st->element_begin(),
         e = st->element_end(); i != e; ++i){
    const llvm::Type *et= *i;

    // Treat an array field as a single element of its type.
    while (const llvm::ArrayType *at = llvm::dyn_cast<llvm::ArrayType>(et))
      et = at->getElementType();

    // The offset is where this element will be placed in the exp. struct.
    off.push_back(num_fields);
    
    // Process nested struct.
    if (const llvm::StructType *nst = llvm::dyn_cast<llvm::StructType>(et)) {
      StructInfoMap::iterator nst_it = struct_info_map.find(nst);
      if (nst_it == struct_info_map.end()) {
        // Recurse.
        analyze_struct(nst);
        nst_it = struct_info_map.find(nst);
      }      
      
      const std::vector<u32> &sz_element = nst->second.first;
      num_fields += sz_element.size();
      
      // Copy the nested struct's info, whose element 0 is the size of the
      // nested struct itself.
      for (u32 j = 0; j < sz_element.size(); j++) {
        sz.push_back(sz_element[j]);
      }      
    } else{
      // Process a simple type.
      sz.push_back(1);
      num_fields++;
    }
  }

  // Record the size of the complete struct and update max_struct.
  sz[0]= num_fields;  
  if(num_fields > max_struct_sz){
    max_struct = st;
    max_struct_sz= num_fields;
  }
  
  struct_info_map[st] = std::make_pair(sz, off);
}

static bool is_address_taken(Value *v) const {
  assert(v);

  // Check the uses of this value.
  for (Value::use_iterator i = v->use_begin(), e = v->use_end();
       i != e; i++) {
    // Handle call instructions.
    llvm::CallInst *call_inst = llvm::dyn_cast<llvm::CallInst>(*i);
    if (call_inst) {
      for (u32 j = 1, je = call_inst->getNumOperands(); j < je; ++j) {
        if (call_inst->getOperand(j) == v)
          return true;
      }
    }

    // Handle invoke instructions.
    llvm::InvokeInst *invoke_isnt = llvm::dyn_cast<llvm::InvokeInst>(*i);
    if (invoke_inst) {
      for(u32 j = 3, je = invoke_inst->getNumOperands(); j < je; ++j) {
        if (invoke_inst->getOperand(j) == v)
          return true;
      }
    }

    // Handle constant expressions.
    llvm::ConstantExpr *const_expr = llvm::dyn_cast<llvm::ConstantExpr>(*i);
    if (const_expr) {
      // Recurse.
      if (escapes(const_expr))
        return true;
    }

    // Handle non-compare instructions.
    if (!llvm::isa<llvm::CmpInst>(*i))
      return true;
  }

  return false;
}

// A bunch of state:
//
// next_node:
// * The ID of the node to create next (should equal nodes.size()).
// nodes:
// * The nodes of the constraint and points-to graph
// value_nodes:
// * The node ID of each value
// object_nodes:
// * The first node of the value's object (if it has one),
// ret_nodes:
// * The nodes for the return value of a function
// vararg_nodes:
// * The nodes for the varargs of a function.
// at_args:
// * The args of addr-taken func (exception for the node-info check)
// ext_info:
// * Info about external functions.
u32 next_node;
std::vector<Node *> nodes;
llvm::DenseMap<llvm::Value*, u32> value_nodes, object_nodes;
llvm::DenseMap<llvm::Function*, u32> ret_nodes, vararg_nodes;
llvm::DenseSet<llvm::Value*> at_args;
ExtInfo *ext_info;

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

//The offsets from a function's obj node to the return value and first arg.
const u32 func_node_off_ret= 1, func_node_off_arg0= 2;

static void add_function_constraints(llvm::Function *f) {
  assert(f);

  // Check if this function's address is ever taken.
  bool address_taken = is_address_taken(f);

  // Add a constraint.
  u32 f_value_node = 0, f_object_node = 0;
  if (address_taken) {
    f_value_node = next_node++;
    nodes.push_back(new Node(f));
    value_nodes[f] = f_value_node;

    f_object_node = next_node++;
    nodes.push_back(new Node(f, 1 /* obj_sz */));
    object_nodes[f] = f_object_node;

    add_constraint(ConstraintAddrOf, f_value_node, f_object_node);
  }

  // Don't analyze external functions. They're handled at the call site.
  if (ext_info->is_ext(f)) {
    return;
  }

  bool is_vararg = f->isVarArg();

  // Treat the double-ptr args to main (argv and envp) as external vars.
  if (f->getNameStr() == "main") {
    if (llvm::Argument *argv = get_argv(f)) {
      add_double_object_node(argv);
    }
    if (llvm::Argument *envp = get_envp(f)) {
      add_double_object_node(envp);
    }
  } else if (!address_taken) {
    // Step 1.
    //
    // Make a value node for each pointer argument.
    for (llvm::Function::arg_iterator arg_it = f->arg_begin(),
           arg_end = f->arg_end(); arg_it != arg_end; arg_it++) {
      llvm::Argument *arg = *arg_it;
      
      if (llvm::isa<llvm::PointerType>(arg->getType())) {
        u32 value_node = next_node++;
        nodes.push_back(new Node(arg));
        value_nodes[arg] = value_node;
      }
    }

    // Step 2.
    //
    // Make a return node if needed.
    if (llvm::isa<llvm::PointerType>(f->getReturnType())) {
      u32 return_node = next_node++;
      nodes.push_back(new Node(f));
      ret_nodes[f] = return_node;
    }

    // Step 3.
    //
    // Make a vararg node if needed.
    if (f->isVarArg()) {
      u32 vararg_node = next_node++;
      nodes.push_back(new Node(f));
      vararg_nodes[f] = vararg_node;
    }
  } else {
    // If the address of the function *was* taken and this isn't main ...

    // Step 1.
    //
    // Map all args to the function's obj node.
    u32 last_ptr= ~0UL;
    std::vector<llvm::Value *> args;

    for (u32 i = 0, llvm::Function::arg_iterator arg_it = f->arg_begin(),
           arg_end = f->arg_end(); arg_it != arg_end; i++, arg_it++) {
      llvm::Argument *arg = *arg_it;
      args.push_back(arg);

      value_nodes[arg] = f_object_node + func_node_off_arg0 + i;
      at_args.insert(arg);

      if (llvm::isa<llvm::PointerType(arg->getType())) {
        last_ptr = i;
      }
    }

    // Step 2.
    //
    // Map the return node.
    assert(next_node == f_object_node + func_node_off_ret);
    nodes.push_back(new Node(f, 1));
    next_node++;

    // Step 3.
    //
    // Make object nodes for all args up to the last ptr.
    // Their values must be the args themselves (not the function).
    assert(next_node == f_object_node + func_node_off_arg0);
    if (last_ptr != ~0UL) {
      for (u32 i = 0; i <= last_ptr; ++i){
        nodes.push_back(new Node(args[i], 1));
      }
      next_node += last_ptr + 1;
    }

    // Step 4.
    //
    // Make a vararg node if needed.
    if (f->isVarArg()) {
      nodes.push_back(new Node(f, 1));
      ++next_node;
    }

    // Step 5.
    //
    // Extend the object of f to include the nodes above.
    nodes[f_object_node]->obj_sz = next_node - f_object_node;
  }
}

static void add_const_gep_constraints(llvm::Value *v) {
}

static void add_global_constraints(llvm::GlobalVariable *g) {
  assert(g);

  // Step 1.
  //
  // Make a node for the global pointer.
  u32 g_value_node = next_node++;
  nodes.push_back(new Node(g));
  value_nodes[g] = g_value_node;

  // Step 2.
  //
  // Get the type of object that this global points to.
  const llvm::Type *typ = g->getType()->getContainedType(0);
  bool is_array = false;
  while (const llvm::ArrayType *at = llvm::dyn_cast<llvm::ArrayType>(typ)) {
    typ = at->getElementType();
    is_array = true;
  }

  // Step 3.
  //
  // Create the global object.
  u32 g_object_node = next_node++;
  object_nodes[g] = g_object_node;

  const llvm::StructType *st = llvm::dyn_cast<llvm::StructType>(typ);
  if (st) {
    const std::vector<u32> &sz = get_struct_sz(st);
    for (int i = 0; i < sz.size(); i++) {
      nodes.push_back(new Node(g, sz[i], is_array));
    }
    next_node += sz.size();

    // A struct may be used in a constant GEP expression.
    add_const_gep_constraints(g);
  } else {
    // Not a struct, just add the object node above.
    nodes.push_back(new Node(g, 1, is_array));
    next_node++;

    // An array may be used in a constant GEP expression.
    if (is_array)
      add_const_gep_constraints(g);
  }

  add_constraint(ConstraintAddrOf, g_value_node, g_object_node);
}

AndersConstraints *build_anders_constraints(llvm::Module *m, const FunctionIterator &fi) {
  // Step 1.
  //
  // Add the placeholder nodes (the ones that come before the first
  // real node).
  for (u32 i = 0; i < FirstNode; i++) {
    // Initialize them with empty llvm::Values.
    Node *node = new Node();

    // If this is supposed to represent the unknown target of pointers
    // cast from int, then it's actually an object because its address
    // is taken. The special node ConstToUnknownNode is its initial pointer.
    if (i == UnknownTarget) {
      node->obj_sz = 1;
      add_constraint(ConstraintAddrOf, ConstToUnknownTarget, UnknownTarget);
    }
    nodes.push_back(new Node);
  }

  next_node = FirstNode;

  // Step 2.
  //
  // Find and map all struct types in the program.
  max_struct = min_struct;
  max_struct_sz = 0;

  llvm::TypeSymbolTable &tst = m->getTypeSymbolTable();
  for (llvm::TypeSymbolTable::iterator i = tst.begin(), e = tst.end();
       i != e; i++) {
    const llvm::StructType *st = llvm::dyn_cast<llvm::StructType>(i->second);
    if (st) {
      analyze_struct(st);
    }
  }

  // Step 3.
  //
  // Add constraints for functions pointers, function args, and function
  // return values;
  for (llvm::Module::iterator i = m->begin(); e = m->end(); i != e; i++) {
    add_function_constraints(i);
  }

  // Step 4.
  //
  // Add constraints for globals. This might include adding constraints
  // for const GEP expressions using the global.
  for (llvm::Module::global_iterator i = m->global_begin(), i = m->global_end();
       i != e; i++) {
    add_global_constraints(i);
  }
}
