//===- global_state.cpp ---------------------------------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#include "global_state.h"
#include "structs.h"

static void assert_ret_next(GlobalState *gs, u32 obj_node_id) {
  u32 ret_node_id = obj_node_id + FUNC_NODE_OFF_RET;
  assert(gs->nodes->next() == ret_node_id);  
}

static void assert_args_next(GlobalState *gs, u32 obj_node_id) {
  u32 next_arg_node_id = obj_node_id + FUNC_NODE_OFF_ARG0;
  assert(gs->nodes->next() == next_arg_node_id);
}

static void assert_obj_sz(GlobalState *gs, u32 obj_node_id) {
  u32 num_objects = gs->nodes->next() - obj_node_id;
  assert(gs->nodes->nodes[obj_node_id]->obj_sz == num_objects);
}

static void init_addr_taken_function_args(GlobalState *gs,
                                          const llvm::Function *f,
                                          u32 obj_node_id) {
  // Sanity check.
  assert_ret_next(gs, obj_node_id);

  // Add a placeholder node for the function's return value.
  // Note that this cannot be found by value. I'm not sure why.
  Node *node = gs->nodes->find_node(obj_node_id);
  node->obj_sz++;
  gs->nodes->add_unreachable(f);

  // Find the last pointer argument.
  u32 last_ptr = OVERFLOW_U32;
  u32 i = 0;
  for (llvm::Function::const_arg_iterator arg = f->arg_begin(),
         end = f->arg_end(); arg != end; i++, arg++) {
    if (is_pointer(arg)) {
      last_ptr = i;
    }
  }

  // Sanity check.
  assert_args_next(gs, obj_node_id);
  
  // Up until the last pointer argument, if there is one, map
  // each argument value to a node.
  if (last_ptr != OVERFLOW_U32) {
    u32 j = 0;
    for (llvm::Function::const_arg_iterator arg = f->arg_begin(),
           end = f->arg_end(); j != last_ptr && arg != end; j++, arg++) {
      
      // Map argument to a node that can be found by value.
      u32 arg_id = gs->nodes->add_value(arg);
      assert(arg_id == obj_node_id + FUNC_NODE_OFF_ARG0 + j);
      node->obj_sz++;

      // Keep track of arguments of address taken functions.
      gs->addr_taken_args.insert(arg);
    }
  }
  
  // Make a vararg node if needed. Note that like the return
  // value this also cannot be found by value. Again, not sure
  // why.
  if (f->isVarArg()) {
    node->obj_sz++;
    gs->nodes->add_unreachable(f);
  }

  // Sanity check.
  assert_obj_sz(gs, obj_node_id);
}

static void init_addr_taken_function_signature(GlobalState *gs,
                                               const llvm::Function *f,
                                               std::string entry_point) {

  assert(f && f->hasAddressTaken());

  // Create a node at node_id to represent the function
  // pointer.
  u32 node_id = gs->nodes->add_value(f);

  // Create a node at obj_node_id to represent the function
  // object backing the pointer.
  u32 obj_node_id = gs->nodes->add_object(f, 1);

  // Create a constraint where node_id represents the
  // function pointer and obj_node_id represents the
  // function object whose address is being taken:
  //
  //   node_id = &obj_node_id
  gs->constraints->add(ConstraintAddrOf, node_id, obj_node_id);

  // Skip special functions:
  //
  // 1. Treat external function args as external variables.
  //
  // 2. Treat entry point args as external variables. After all,
  //    if this is a C-style main with argv and envp, there's
  //    no object we can find in the program backing these
  //    pointer args anyway.
  if (f->getNameStr() == entry_point ||
      gs->module->ext_info.is_ext(f)) {
    return;
  }

  // Handle addr taken function args.
  init_addr_taken_function_args(gs, f, obj_node_id);
}

static void init_normal_function_args(GlobalState *gs,
                                      const llvm::Function *f) {  

  // Map each pointer argument value to a node.
  for (llvm::Function::const_arg_iterator arg = f->arg_begin(),
         end = f->arg_end(); arg != end; arg++) {

    llvm::outs() << "  arg: ";
    llvm::WriteAsOperand(llvm::outs(), arg, false);
    llvm::outs() << " is_pointer: " << is_pointer(arg) << "\n";
       
    if (is_pointer(arg->getType())) {
      gs->nodes->add_value(arg);
    }
  }
  
  // Map pointer returns to the function itself.
  if (is_pointer(f->getReturnType())) {
    gs->nodes->add_ret(f);
  }
  
  // Condense and map variable args to the function itself.
  if (f->isVarArg()) {
    gs->nodes->add_vararg(f);
  }
}

static void init_normal_function_signature(GlobalState *gs,
                                           const llvm::Function *f,
                                           std::string entry_point) {

  // Treat the entry point's args and external function
  // args as external variables. Effectively skip.
  if (f->getNameStr() == entry_point ||
      f->isDeclaration())
    return;

  init_normal_function_args(gs, f);
}

// Process function signatures. This means:
//
// + Add pointer and object nodes for functions with
//   their address taken.
//
// + Add pointer and object nodes for arguments with their
//   address taken.
//
// Ordering:
//
// While we could link all of the objects together lazily, it's
// much nicer to compute object nodes for all of the top-level
// declarations (functions and global variables) first, because
// then we don't risk creating unreachable objects.
//
// To do this, we can also strongly assert that all initialized
// global variables have values that are either free-standing
// constants or pointers to things that are already cached.
void GlobalState::process_function_signature(const llvm::Function *f) {
  llvm::outs() << "** Processing function signature: " << f->getName()
               << " addr taken: " << f->hasAddressTaken() << "\n";
  
  if (f->hasAddressTaken()) {
    init_addr_taken_function_signature(this, f, this->module->entry_point);
    return;
  }

  init_normal_function_signature(this, f, this->module->entry_point);
}

void GlobalState::process_global_signature(const llvm::GlobalVariable *g) {
  assert(g);

  // N.B. Not sure this works for empty structs.
  //      Also not sure this syncs well with array
  //      intializers.

  // Find the type of the global.
  GlobalType gt(g);

  // Allocate pointer node.
  u32 node_id = this->nodes->add_value(g);

  // Allocate object node(s).
  u32 obj_id = NodeNone;
  
  if (const llvm::StructType *st = struct_type(gt.type)) {
    // Handle structs.
    const std::vector<u32> sz = this->module->structs.get_sz(st);
    for (std::vector<u32>::const_iterator i = sz.begin(), e = sz.end();
         i != e; i++) {
      u32 _obj_id = this->nodes->add_object(g, *i, gt.is_array);
      // Use the first element to represent the whole thing.
      if (obj_id == NodeNone) {
        obj_id = _obj_id;
      }
    }
  } else {
    // Treat everything else like a single-value object,
    // including arrays.
    obj_id = this->nodes->add_object(g, 1, gt.is_array);
  }

  // Assert that the obj_id is valid.
  assert(obj_id != NodeNone);

  // Map the global pointer to the global object id.
  this->constraints->add(ConstraintAddrOf, node_id, obj_id);  
}

static u32 init_gep(GlobalState *gs,
                    llvm::ConstantExpr *expr) {

  assert(is_gep(expr));

  // Lookup or add a value node for the const expr.
  u32 node_id = gs->nodes->find_value_node(expr, true);
  if (gs->done_set.lookup(node_id))
    // Already processed.
    return node_id;

  if (!node_id) {
    node_id = gs->nodes->add_value(expr);
  }
    
  // Prematurely mark processed for cleaner returns.
  gs->done_set.add(node_id, 1);
  
  llvm::Value *ptr = expr->getOperand(0);
  llvm::ConstantExpr *sub_expr = const_expr(&ptr);

  assert(!is_const_null_ptr(ptr) && "const null ptrs not supported for globals");
  
  if (is_int_to_ptr(sub_expr)) {
    gs->constraints->add(ConstraintAddrOf, node_id, NodeUnknownTarget);
    return node_id;
  }

  u32 ptr_node_id = gs->nodes->find_value_node(ptr, true);
  if (!ptr_node_id) {
    ptr_node_id = gs->nodes->add_value(ptr);
  }

  gs->constraints->add(ConstraintGEP, node_id, ptr_node_id,
                       gep_struct_off(gs->module->structs, expr));
         
  if (is_gep(sub_expr)) {
    init_gep(gs, sub_expr);
  }

  return node_id;
}

u32 init_global_value(GlobalState *gs, llvm::Constant *c, u32 obj_id);

static u32 __init_global_value(GlobalState *gs,
                               llvm::Constant *c,
                               u32 obj_id) {  
  assert(c);

  // Find underlying expression, if one exists.
  llvm::ConstantExpr *expr = const_expr(&c);

  if (is_ptr_to_int(expr) ||
      is_null(c) ||
      is_undefined(c) ||
      (is_single_value_type(c) && !is_pointer(c))) {
    // Skip over, nothing interesting here.
    return 1;
  }
  
  if (is_int_to_ptr(expr)) {
    // Add pointer to unknown target.
    gs->constraints->add(ConstraintAddrOf, obj_id, NodeUnknownTarget);
    return 1;
  }

  // Other const expr types (Bitcast, PtrToInt, IntToPtr)
  // are handled above (const_expr, skip, unknown target).
  if (expr) assert(is_gep(expr));
  
  if (llvm::ConstantStruct *cs = const_struct(c)) {
    // Handle structs.
    u32 off = 0;
    for (u32 i = 0; i < cs->getNumOperands(); i++) {
      off += init_global_value(gs, cs->getOperand(i),
                                obj_id + off);
    }
    return off;
  }

  if (llvm::ConstantArray *ca = const_array(c)) {
    // Handle arrays.
    u32 off = 0;
    for (u32 i = 0; i < ca->getNumOperands(); i++) {
      off += init_global_value(gs, ca->getOperand(i),
                                obj_id + off);
    }
    return off;
  }

  // Handle single value types.
  assert(is_single_value_type(c));

  if (is_pointer(c) && is_gep(expr)) {
    // Handle pointers to GEPs.
    u32 expr_id = init_gep(gs, expr);
    gs->constraints->add(ConstraintCopy, obj_id, expr_id);
    return 1;
  }

  if (is_pointer(c)) {
    // Handle pointers to everything else.
    u32 const_obj_id = gs->nodes->find_object_node(c);
    gs->constraints->add(ConstraintAddrOf, obj_id, const_obj_id);
    return 1;
  }

  // Nothing to do for non-pointers.
  return 1;
}

u32 init_global_value(GlobalState *gs,
                      llvm::Constant *c,
                      u32 obj_id) {
  assert(c);

  u32 num_fields;
  if (!gs->done_set.lookup(obj_id, &num_fields)) {
    return num_fields;
  }
  
  num_fields = __init_global_value(gs, c, obj_id);
  gs->done_set.add(obj_id, num_fields);

  return num_fields;
}

void GlobalState::process_global_value(const llvm::GlobalVariable *g) {
  assert(g);

  if (!g->hasInitializer()) {
    // No value to initialize.
    return;
  }

  // The id of the global object.
  u32 obj_id = this->nodes->find_object_node(g);
  
  // The global value to associate with the object.
  llvm::Constant *c = g->getInitializer();

  init_global_value(this, c, obj_id);
}
