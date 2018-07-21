#include "external.h"

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
  assert(v);

  for (llvm::Value::use_iterator i = v->use_begin(); e = v->use_end();
       i != e; i++) {
    llvm::ConstantExpr *exp = llvm::dyn_cast<llvm::ConstantExpr>(*i);
    if (expr) {
      if (expr->getOpcode() == llvm::Instruction::BitCast) {
        // Recurse.
        add_const_gep_constraints(expr);
      } else if (expr->getOpcode() == llvm::Instruction::GetElementPtr) {
        // A GEP can only use a pointer as its first op.
        assert(expr->getOperand(0) == v);

        // Save it.
        assert(!value_nodes.count(expr));
        u32 value_node = next_node++;
        nodes.push_back(new Node(expr));
        value_nodes[expr] = value_node;

        // Keep track of it, to initialize it after other globals.
        gep_ce_nodes.push_back(value_node);
      }
    }
  }

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

//The nodes that have already been visited by proc_global_init or proc_gep_ce.
//The value is the return of proc_global_init, or 1 for proc_gep_ce.
llvm::DenseMap<u32, u32> globals_initialized;

static llvm::Constant *strip_bitcasts(llvm::Constant *c) {
  for (llvm::ConstExpr *expr = llvm::dyn_cast<llvm::ConstantExpr>(c);
       expr; expr = llvm::dyn_cast_or_null<llvm::ConstantExpr>(c)) {
    switch (expr->getOpcode()) {
    case llvm::Instruction::BitCast:
      c = expr->getOperand(0);
      break;
    case llvm::Instruction::IntToPtr:
    case llvm::Instruction::PtrToInt:
    case llvm::Instruction::GetElementPtr:
      return c;
    default:
    }
  }
  
  return c;
}

static void initialize_gep(u32 value_node_expr) {
}

static void initialize_global(u32 object_node, llvm::Constant *c, bool first = false) {
  assert(object_node && c);

  // Step 1.
  //
  // Have we already initialized it? Then just return it.
  llvm::DenseMap<u32, u32>::iterator g_it = globals_initialized.find(object_node);
  if (g_it != globals_initialized.end()) {
    return g_it->second;
  }

  // Step 2.
  //
  // Strip bitcast expressions from c until we find a non-expr value,
  // a GEP, or one of the IntToPtr/PtrToInt instructions below.
  c = strip_bitcasts(c);

  // Step 3.
  //
  // Handle IntToPtr and PtrToInt instructions.  
  llvm::ConstExpr *const_expr = llvm::dyn_cast<llvm::ConstantExpr>(c);
  switch (const_expr->getOpcode()) {
  case llvm::Instruction::IntToPtr:
    // We don't trace int->ptr for globals.
    c = 0;
    break;
  case llvm::Instruction::PtrToInt:
    // Exit on a ptr->int instruction.
    if (first) {
      globals_initialized[object_node] = 1;
    }
    // Why don't we add a constraint here?
    return 1;
  default:
  }

  // Step 4.
  //
  // Handle an invalidated constant (see IntToPtr above).
  if (!c) {
    if (first) {
      globals_initialized[object_node] = 1;
    }
    add_constraint(ConstraintAddrOf, object_node, UnknownTarget);
    return 1;
  }

  // Step 5.
  //
  // Handle a null or undefined constant.
  if (c->isNullValue() || llvm::isa<llvm::UndefValue>(c)) {
    if (first) {
      globals_initialized[object_node] = 1;      
    }
    // Same as above, why don't we add a constraint here?
    return 1;
  }

  // Step 6.
  //
  // Handle a single value constant.
  if (c->getType()->isSingleValueType()) {
    // Not a pointer?
    bool pointer = llvm::isa<llvm::PointerType>(c->getType());
    if (!pointer) {
      if (first) {
        globals_initialized[object_node] = 1;
      }
      return 1;
    }

    // Not a const expression?
    llvm::ConstantExpr *expr = llvm::dyn_cast<llvm::ConstantExpr>(c);
    if (!expr) {
      u32 object_node_const = find_object_node(c);
      add_constraint(ConstraintAddrOf, object_node, object_node_const);

      if (first) {
        globals_initialized[object_node] = 1;
      }
      return 1;
    }

    // Not found?
    u32 value_node_expr = find_value_node(expr, 1);
    if (!value_node_expr) {
      value_node_expr = next_node++;
      nodes.push_back(new Node(expr));
      value_node[expr] = value_node_expr;
    }

    // Initialize and return.
    initialize_gep(value_node_expr);
    add_constraint(ConstraintCopy, object_node, value_node_expr);
    
    if (first) {
      globals_initialized[object_node] = 1;
    }
    return 1;
  }

  // Step 7.
  //
  // Handle constant structs.
  llvm::ConstantStruct *cs = llvm::dyn_cast<llvm::ConstantStruct>(c);
  if (cs) {
    u32 off = 0;
    for (int i = 0; i < cs->getNumOperands(); i++) {
      // Recurse.
      off += initialize_global(object_node + off, c->getOperand(i), false);
    }

    if (first) {
      globals_initialized[object_node] = off;
    }
    return off;
  }

  // Step 8.
  //
  // Handle constant arrays. We expect nothing else at this point.
  llvm::ConstantArray *ca = llvm::dyn_cast<llvm::ConstantArray>(c);
  assert(ca && "unexpected multi-value constant");

  u32 off = 0;
  for (int i = 0; i < ca->getNumOperands(); i++) {
    off = initialize_global(object_node, ca->getOperand(i), false);
  }

  if (first) {
    globals_initialized[object_node] = off;
  }
  return off;
}

static void sanity_check() {
  // Iterate through all of the nodes.
  for (int i = 0; i < nodes.size(); i++) {
    // Step 1.
    //
    // Check that nodes with real values always have a size > 0.
    const Node *node = nodes[i];
    if (!node->val) {
      assert(!node->obj_sz || i == UnknownTarget);
      continue;
    }

    u32 value_node = get_value_node(node->val, 1);
    u32 object_node = get_object_node(node->val, 1);
    u32 ret_node = 0, vararg_node = 0;

    // Step 2.
    //
    // Figure out what short of value the node has.
    if (llvm::Function *f = llvm::dyn_cast<llvm::Function>(node->val)) {
      llvm::DenseMap<llvm::Function *, u32>::iterator j = ret_nodes.find(f);
      if (j != ret_nodes.end()) {
        ret_node = j->second;
      }

      j = vararg_nodes.find(f);
      if (j != vararg_nodes.end()) {
        vararg_node = j->second;
      }
    }

    // Step 3.
    //
    // If the node isn't an object node, check that it doesn't have
    // an object size or that it's a special type (at_args).
    bool value_or_object = false;
    if (i == value_node ||
        i == ret_node ||
        i == vararg_node) {
      assert(!node->obj_sz || at_args.count(node->val));
      value_or_object = true;
    }

    // Step 4.
    //
    // If the node is an object node, the check that it's within
    // the object of its value.
    if (i < object_node + nodes[object_node]->obj_siz) {
      u32 object_offset = node->obj_sz + 1;
      u32 object_upper_bound = object_node + nodes[object_nodes]->obj_sz;
      assert(node->obj_sz && object_offset <= object_upper_bound);
      value_or_object = true;
    }

    // Step 5.
    //
    // Check that we did Step 3 or Step 4. If we didn't, then the
    // node isn't findable.
    assert(value_or_object);

    // Step 6.
    //
    // Check that the value map points to right objects in
    // the node set.
    for (llvm::DenseMap<llvm::Value *, u32>::iterator i = value_nodes.begin(),
           e = value_nodes.end(); i != e; i++) {
      llvm::Value *v = i->first;
      u32 node_id = i->second;

      // Skip the args of an addr-taken function, they're are mapped
      // to the obj_nodes instead.
      if (at_args.count(v)) {
        continue;
      }      
      assert(v == nodes[node_id]->val);
    }

    // Step 7.
    //
    // Check that the object map points to the right objects in
    // the node set.
    for (llvm::DenseMap<llvm::Value *, u32>::iterator i = object_nodes.begin(),
           e = object_nodes.end(); i != e; i++) {
      llvm::Value *v = i->first;
      u32 node_id = i->second;
      assert(v == nodes[node_id]->val);
    }

    // Step 8.
    //
    // Check that the return map points to the right objects in
    // the node set.
    for (llvm::DenseMap<llvm::Function *, u32>::iterator i = ret_nodes.begin(),
           e = ret_nodes.end(); i != e; i++) {
      llvm::Function *f = i->first;
      u32 node_id = i->second;
      assert(f == nodes[node_id]->val);
    }

    // Step 9.
    //
    // Check that the vararg map points to the right objects in
    // the nodes set.
    for (llvm::DenseMap<llvm::Function *, u32>::iterator i = vararg_nodes.begin(),
           e = vararg_nodes.end(); i != e; i++) {
      llvm::Function *f = i->first;
      u32 node_id = i->second;
      assert(f == nodes[node_id]->val);
    }
  }
}

Constraints *build_constraints(llvm::Module *m, const Processor *proc) {
  // Step 1.
  //
  // Add the placeholder nodes (the ones that come before the first
  // real node) and position the first node.
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
  for (llvm::Module::global_iterator i = m->global_begin(), e = m->global_end();
       i != e; i++) {
    add_global_constraints(i);
  }

  // Step 5.
  //
  // Initialize globals (separately from Step 4). This is because an
  // initializer may refer to a global below it.
  for (llvm::Module::global_iterator i = m->global_begin(), e = m->global_end();
       i != e; i++) {
    llvm::GlobalVariable *g = *i;
    if (g->hasInitializer()) {
      u32 object_node = find_object_node(g);
      initialize_global(object_node, g->getInitializer());
    }
  }

  // Step 6.
  //
  // Initialize the GEP constant expressions.
  for (int i = 0; i < gep_ce_nodes.size(); i++) {
    u32 value_node_expr = gep_ce_nodes[i];
    initialize_gep(value_node_expr);
  }
}
