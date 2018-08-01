//===- analysis_set.cpp -- ------------------------------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//
#include "llvm/Argument.h"        // for llvm::Argument
#include "llvm/Constants.h"       // for llvm::ConstantExpr
#include "llvm/DerivedTypes.h"    // for llvm::StructType, llvm::PointerType
#include "llvm/Instructions.h"    // for llvm::CallInst, llvm::InvokeInst,
                                  //     llvm::CmpInst
#include "llvm/GlobalVariable.h"  // for llvm::Type
#include "llvm/Module.h"          // for llvm::Function
#include "llvm/Type.h"            // for llvm::Type
#include "llvm/TypeSymbolTable.h" // for llvm::TypeSymbolTable
#include "llvm/Value.h"           // for llvm::Value
#include "llvm/Support/Casting.h" // for llvm::dyn_cast, llvm::isa
#include "llvm/Support/GetElementPtrTypeIterator.h"

#include "analysis_set.h"
#include "constraints.h"
#include "nodes.h"

static bool is_address_taken(llvm::Value *v) {
  assert(v);

  // Check the uses of this value.
  for (llvm::Value::use_iterator i = v->use_begin(), e = v->use_end();
       i != e; i++) {

    // Handle call instructions.
    llvm::CallInst *call_inst = llvm::dyn_cast<llvm::CallInst>(*i);
    if (call_inst) {
      for (u32 j = 1, je = call_inst->getNumOperands(); j < je; ++j) {
        if (call_inst->getOperand(j) == v)
          return true;
      }
      continue;
    }

    // Handle invoke instructions.
    llvm::InvokeInst *invoke_inst = llvm::dyn_cast<llvm::InvokeInst>(*i);
    if (invoke_inst) {
      for(u32 j = 3, je = invoke_inst->getNumOperands(); j < je; ++j) {
        if (invoke_inst->getOperand(j) == v)
          return true;
      }
      continue;
    }

    // Handle constant expressions.
    llvm::ConstantExpr *const_expr = llvm::dyn_cast<llvm::ConstantExpr>(*i);
    if (const_expr) {
      // Recurse.
      if (is_address_taken(const_expr)) {        
        return true;
      }
      continue;
    }

    // Handle non-compare instructions.
    if (!llvm::isa<llvm::CmpInst>(*i))
      return true;
  }

  return false;
}

static llvm::Argument *get_argv(llvm::Function *f) {
  u32 i = 0;
  for (llvm::Function::arg_iterator arg_it = f->arg_begin(),
         arg_end = f->arg_end(); arg_it != arg_end; arg_it++) {    
    if (i == 1) {
      return arg_it;
    }
    i++;
  }
  return nullptr;
}

static llvm::Argument *get_envp(llvm::Function *f) {
  u32 i = 0;
  for (llvm::Function::arg_iterator arg_it = f->arg_begin(),
         arg_end = f->arg_end(); arg_it != arg_end; arg_it++) {    
    if (i == 2) {
      return arg_it;
    }
    i++;
  }
  return nullptr;
}  

static void add_double_object_node(AnalysisSet *as,
                                   llvm::Value *v) {

  u32 value_node = as->nodes.next++;
  as->nodes.nodes.push_back(new Node(v));
  as->nodes.value_nodes[v] = value_node;

  u32 object_node = as->nodes.next;
  as->nodes.next += 2;
  as->nodes.nodes.push_back(new Node(v, 2 /* obj_sz */));
  as->nodes.nodes.push_back(new Node(v, 1 /* obj_sz */));
  as->nodes.object_nodes[v] = object_node;

  as->constraints.add(ConstraintAddrOf, value_node, object_node);
  as->constraints.add(ConstraintAddrOf, object_node, object_node + 1);
}

static void add_entry_point_function_args(AnalysisSet *as,
                                          llvm::Function *f) {
  
  if (llvm::Argument *argv = get_argv(f)) {
    add_double_object_node(as, argv);
  }
  
  if (llvm::Argument *envp = get_envp(f)) {
    add_double_object_node(as, envp);
  }
}

static void add_nonaddr_taken_function_args(AnalysisSet *as,
                                            llvm::Function *f) {
  
  // Step 1.
  //
  // Make a value node for each pointer argument.
  for (llvm::Function::arg_iterator arg_it = f->arg_begin(),
         arg_end = f->arg_end(); arg_it != arg_end; arg_it++) {
    llvm::Argument *arg = arg_it;
    
    if (llvm::isa<llvm::PointerType>(arg->getType())) {
      u32 value_node = as->nodes.next++;
      as->nodes.nodes.push_back(new Node(arg));
      as->nodes.value_nodes[arg] = value_node;
    }
  }
  
  // Step 2.
  //
  // Make a return node if needed.
  if (llvm::isa<llvm::PointerType>(f->getReturnType())) {
    u32 return_node = as->nodes.next++;
    as->nodes.nodes.push_back(new Node(f));
    as->nodes.ret_nodes[f] = return_node;
  }
  
  // Step 3.
  //
  // Make a vararg node if needed.
  if (f->isVarArg()) {
    u32 vararg_node = as->nodes.next++;
    as->nodes.nodes.push_back(new Node(f));
    as->nodes.vararg_nodes[f] = vararg_node;
  }
}

static void add_addr_taken_function_args(AnalysisSet *as,
                                         llvm::Function *f,
                                         u32 func_object_node) {
  // Step 1.
  //
  // Map all args to the function's obj node.
  u32 last_ptr = OVERFLOW_U32;
  std::vector<llvm::Value *> args;

  u32 i = 0;
  for (llvm::Function::arg_iterator arg_it = f->arg_begin(),
         arg_end = f->arg_end(); arg_it != arg_end; i++, arg_it++) {
    llvm::Argument *arg = arg_it;
    args.push_back(arg);
    
    as->nodes.value_nodes[arg] =
      func_object_node + FUNC_NODE_OFF_ARG0 + i;
    as->at_args.insert(arg);
    
    if (llvm::isa<llvm::PointerType>(arg->getType())) {
      last_ptr = i;
    }
  }
  
  // Step 2.
  //
  // Map the return node.
  assert(as->nodes.next == func_object_node + FUNC_NODE_OFF_RET);
  as->nodes.nodes.push_back(new Node(f, 1));
  as->nodes.next++;

  // Step 3.
  //
  // Make object nodes for all args up to the last ptr.
  // Their values must be the args themselves (not the function).
  assert(as->nodes.next == func_object_node + FUNC_NODE_OFF_ARG0);
  if (last_ptr != OVERFLOW_U32) {
    for (u32 i = 0; i <= last_ptr; ++i){
      as->nodes.nodes.push_back(new Node(args[i], 1));
    }
    as->nodes.next += last_ptr + 1;
  }

  // Step 4.
  //
  // Make a vararg node if needed.
  if (f->isVarArg()) {
    as->nodes.nodes.push_back(new Node(f, 1));
    ++as->nodes.next;
  }

  // Step 5.
  //
  // Extend the object of f to include the nodes above.
  as->nodes.nodes[func_object_node]->obj_sz =
    as->nodes.next - func_object_node;
}

static void handle_function(AnalysisSet *as,
                            llvm::Function *f) {
  assert(f);

  // Check if this function's address is ever taken.
  bool address_taken = is_address_taken(f);

  // Add a constraint.
  u32 f_value_node = 0;
  u32 f_object_node = 0;
  
  if (address_taken) {
    f_value_node = as->nodes.next++;
    as->nodes.nodes.push_back(new Node(f));
    as->nodes.value_nodes[f] = f_value_node;

    f_object_node = as->nodes.next++;
    as->nodes.nodes.push_back(new Node(f, 1 /* obj_sz */));
    as->nodes.object_nodes[f] = f_object_node;

    as->constraints.add(ConstraintAddrOf, f_value_node, f_object_node);
  }

  // Don't analyze external functions. They're handled at the call site.
  if (as->ext_info.is_ext(f)) {
    return;
  }

  // Treat the double-ptr args to main (argv and envp) as external vars.
  if (f->getNameStr() == "main") {
    add_entry_point_function_args(as, f);
    return;
  }

  if (!address_taken) {
    add_nonaddr_taken_function_args(as, f);
    return;
  }

  // If the address of the function *was* taken and this isn't main ...
  add_addr_taken_function_args(as, f, f_object_node);
}

static void handle_const_gep_using_value(AnalysisSet *as,
                                         llvm::Value *v) {

  assert(v);

  for (llvm::Value::use_iterator i = v->use_begin(), e = v->use_end();
       i != e; i++) {
    llvm::ConstantExpr *expr = llvm::dyn_cast<llvm::ConstantExpr>(*i);
    if (expr) {
      if (expr->getOpcode() == llvm::Instruction::BitCast) {
        // Recurse.
        handle_const_gep_using_value(as, expr);
        continue;
      }

      if (expr->getOpcode() == llvm::Instruction::GetElementPtr) {
        // A GEP can only use a pointer as its first op.
        assert(expr->getOperand(0) == v);

        // Save it.
        assert(!as->nodes.value_nodes.count(expr));

        u32 value_node = as->nodes.next++;
        as->nodes.nodes.push_back(new Node(expr));
        as->nodes.value_nodes[expr] = value_node;

        // Keep track of it, to initialize it after other globals.
        as->nodes.const_gep_nodes.push_back(value_node);
      }
    }
  }  
}

u32 compute_gep_off(AnalysisSet *as, llvm::User *u){
  assert(u);

  u32 off = 0;
  for (llvm::gep_type_iterator i = llvm::gep_type_begin(*u),
         e = llvm::gep_type_end(*u); i != e; i++) {

    const llvm::StructType *st = llvm::dyn_cast<llvm::StructType>(*i);
    if (!st) {
      continue;
    }

    llvm::ConstantInt *op = llvm::dyn_cast<llvm::ConstantInt>(i.getOperand());
    assert(op && "non-const struct index in GEP");

    const std::vector<u32> &so = as->structs.get_off(st);
    u32 index = op ? op->getZExtValue() : 0;
    if (index >= so.size()) {
      assert(false && "struct index out of bounds");
    }

    off += so[index];
  }

  return off;
}

u32 handle_global_initializer(AnalysisSet *as,
                              llvm::Constant *c,
                              u32 object_node,
                              llvm::DenseMap<u32, u32> &glb_init,
                              bool first);

static void handle_const_gep_at(AnalysisSet *as,
                                u32 value_node,
                                llvm::DenseMap<u32, u32> &glb_init) {

  llvm::Value *v = as->nodes.nodes[value_node]->val;
  llvm::ConstantExpr *expr = llvm::dyn_cast_or_null<llvm::ConstantExpr>(v);

  assert(expr);
  assert(expr->getOpcode() == llvm::Instruction::GetElementPtr);

  if (glb_init.count(value_node)) {
    return;
  }
  
  glb_init[value_node] = 1;

  bool nested = 0;
  llvm::Value *r = expr->getOperand(0);

  for (llvm::ConstantExpr *er = llvm::dyn_cast<llvm::ConstantExpr>(r);
       er && !nested; er = llvm::dyn_cast_or_null<llvm::ConstantExpr>(r)) {
    switch (er->getOpcode()) {
    case llvm::Instruction::BitCast:
      r = er->getOperand(0);
      break;

    case llvm::Instruction::IntToPtr:
      as->constraints.add(ConstraintAddrOf, value_node,
                          NodeUnknownTarget);
      break;

    case llvm::Instruction::GetElementPtr:
      nested = 1;
      break;

    default:
      assert(false && "unexpected constant expression type");
    }
  }

  assert(!llvm::isa<llvm::ConstantPointerNull>(r) &&
      "GEP of null not supported for global init");

  u32 value_node_r = as->nodes.find_value_node(r, true);
  if (!value_node_r) {
    value_node_r = as->nodes.next++;
    as->nodes.nodes.push_back(new Node(r));
    as->nodes.value_nodes[r] = value_node_r;
  }

  u32 off = compute_gep_off(as, expr);
  as->constraints.add(ConstraintGEP, value_node, value_node_r, off);

  if (nested) {
    handle_const_gep_at(as, value_node_r, glb_init);
  }

  if (llvm::GlobalVariable *rg = llvm::dyn_cast<llvm::GlobalVariable>(r)) {
    if (rg->hasInitializer()) {
      u32 object_node_r = as->nodes.find_object_node(r);
      handle_global_initializer(as, rg->getInitializer(),
                                object_node_r, glb_init, true);
    }
  }
}

static void handle_global(AnalysisSet *as,
                          llvm::GlobalVariable *g) {
  
  assert(g);

  // Step 1.
  //
  // Make a node for the global pointer.
  u32 g_value_node = as->nodes.next++;
  as->nodes.nodes.push_back(new Node(g));
  as->nodes.value_nodes[g] = g_value_node;

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
  u32 g_object_node = as->nodes.next++;
  as->nodes.object_nodes[g] = g_object_node;

  const llvm::StructType *st = llvm::dyn_cast<llvm::StructType>(typ);
  if (st) {
    const std::vector<u32> &sz = as->structs.get_sz(st);
    for (unsigned int i = 0; i < sz.size(); i++) {
      as->nodes.nodes.push_back(new Node(g, sz[i], is_array));
    }
    as->nodes.next += sz.size();

    // A struct may be used in a constant GEP expression.
    handle_const_gep_using_value(as, g);

    as->constraints.add(ConstraintAddrOf,
                        g_value_node,
                        g_object_node);
    return;
  }

  // Not a struct, just add the object node above.
  as->nodes.nodes.push_back(new Node(g, 1, is_array));
  as->nodes.next++;

  // An array may be used in a constant GEP expression.
  if (is_array)
    handle_const_gep_using_value(as, g);

  as->constraints.add(ConstraintAddrOf,
                     g_value_node,
                     g_object_node);
}

static llvm::Constant *strip_bitcasts(llvm::Constant *c) {
  for (llvm::ConstantExpr *expr = llvm::dyn_cast<llvm::ConstantExpr>(c);
       expr; expr = llvm::dyn_cast_or_null<llvm::ConstantExpr>(c)) {
    switch (expr->getOpcode()) {
    case llvm::Instruction::BitCast:
      c = expr->getOperand(0);
      break;

    case llvm::Instruction::IntToPtr:
    case llvm::Instruction::PtrToInt:
    case llvm::Instruction::GetElementPtr:
      return c;
    }
  }
  
  return c;
}

u32 handle_global_initializer(AnalysisSet *as,
                              llvm::Constant *c,
                              u32 object_node,
                              llvm::DenseMap<u32, u32> &glb_init,
                              bool first) {
  assert(object_node && c);

  // Step 1.
  //
  // Have we already initialized it? Then just return it.
  llvm::DenseMap<u32, u32>::iterator g_it = glb_init.find(object_node);
  if (g_it != glb_init.end()) {
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
  llvm::ConstantExpr *const_expr = llvm::dyn_cast<llvm::ConstantExpr>(c);
  if (const_expr) {
    switch (const_expr->getOpcode()) {
    case llvm::Instruction::IntToPtr:
      // We don't trace int->ptr for globals.
      c = 0;
      break;
    
    case llvm::Instruction::PtrToInt:
      // Exit on a ptr->int instruction.
      if (first) {
        glb_init[object_node] = 1;
      }
      
      // Why don't we add a constraint here?
      return 1;
    }
  }

  // Step 4.
  //
  // Handle an invalidated constant (see IntToPtr above).
  if (!c) {
    if (first) {
      glb_init[object_node] = 1;
    }

    as->constraints.add(ConstraintAddrOf, object_node, NodeUnknownTarget);
    return 1;
  }

  // Step 5.
  //
  // Handle a null or undefined constant.
  if (c->isNullValue() || llvm::isa<llvm::UndefValue>(c)) {
    if (first) {
      glb_init[object_node] = 1;      
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
        glb_init[object_node] = 1;
      }
      
      return 1;
    }

    // Not a const expression?
    llvm::ConstantExpr *expr = llvm::dyn_cast<llvm::ConstantExpr>(c);
    if (!expr) {
      u32 object_node_const = as->nodes.find_object_node(c);
      as->constraints.add(ConstraintAddrOf, object_node,
                          object_node_const);

      if (first) {
        glb_init[object_node] = 1;
      }
      
      return 1;
    }

    // Not found?
    u32 value_node_expr = as->nodes.find_value_node(expr, 1);
    if (!value_node_expr) {
      value_node_expr = as->nodes.next++;
      as->nodes.nodes.push_back(new Node(expr));
      as->nodes.value_nodes[expr] = value_node_expr;
    }

    // Initialize and return.
    handle_const_gep_at(as, value_node_expr, glb_init);
    as->constraints.add(ConstraintCopy, object_node, value_node_expr);
    
    if (first) {
      glb_init[object_node] = 1;
    }
    
    return 1;
  }

  // Step 7.
  //
  // Handle constant structs.
  llvm::ConstantStruct *cs = llvm::dyn_cast<llvm::ConstantStruct>(c);
  if (cs) {
    u32 off = 0;
    for (unsigned int i = 0; i < cs->getNumOperands(); i++) {
      // Recurse.
      off += handle_global_initializer(as, cs->getOperand(i),
                                       object_node + off,
                                       glb_init, false);
    }

    if (first) {
      glb_init[object_node] = off;
    }

    return off;
  }

  // Step 8.
  //
  // Handle constant arrays. We expect nothing else at this point.
  llvm::ConstantArray *ca = llvm::dyn_cast<llvm::ConstantArray>(c);
  assert(ca && "unexpected multi-value constant");

  u32 off = 0;
  for (unsigned int i = 0; i < ca->getNumOperands(); i++) {
    off = handle_global_initializer(as, ca->getOperand(i),
                                    object_node,
                                    glb_init, false);
  }

  if (first) {
    glb_init[object_node] = off;
  }
  
  return off;  
}

void AnalysisSet::init(llvm::Module *m) {
  // Step 1.
  //
  // Add the placeholder nodes (the ones that come before the first
  // real node) and position the first node.
  for (u32 i = 0; i < NodeFirst; i++) {
    // Initialize them with empty llvm::Values.
    Node *node = new Node();

    // If this is supposed to represent the unknown target of pointers
    // cast from int, then it's actually an object because its address
    // is taken. The special node ConstToUnknownNode is its initial pointer.
    if (i == NodeUnknownTarget) {
      node->obj_sz = 1;
      constraints.add(ConstraintAddrOf,
                      NodeConstToUnknownTarget,
                      NodeUnknownTarget);
    }
    nodes.nodes.push_back(new Node);
  }
  nodes.next = NodeFirst;

  // Step 2.
  //
  // Find and map all struct types in the program.
  llvm::TypeSymbolTable &tst = m->getTypeSymbolTable();
  for (llvm::TypeSymbolTable::iterator i = tst.begin(), e = tst.end();
       i != e; i++) {
    const llvm::StructType *st = llvm::dyn_cast<llvm::StructType>(i->second);
    if (st) {
      structs.analyze(st);
    }
  }

  // Step 3.
  //
  // Add constraints for functions pointers, function args, and function
  // return values;
  for (llvm::Module::iterator i = m->begin(), e = m->end(); i != e; i++) {
    handle_function(this, (llvm::Function *)i);
  }

  // Step 4.
  //
  // Add constraints for globals. This might include adding constraints
  // for const GEP expressions using the global.
  for (llvm::Module::global_iterator i = m->global_begin(), e = m->global_end();
       i != e; i++) {
    handle_global(this, (llvm::GlobalVariable *)i);
  }
  
  llvm::DenseMap<u32, u32> glb_init;
  
  // Step 5.
  //
  // Initialize globals (separately from Step 4). This is because an
  // initializer may refer to a global below it.
  for (llvm::Module::global_iterator i = m->global_begin(), e = m->global_end();
       i != e; i++) {
    llvm::GlobalVariable *g = i;
    if (g->hasInitializer()) {
      u32 object_node = nodes.object_nodes[g];
      handle_global_initializer(this, g, object_node,
                                glb_init, true);
    }
  }

  // Step 6.
  //
  // Initialize the GEP constant expressions.
  for (unsigned int i = 0; i < nodes.const_gep_nodes.size(); i++) {
    u32 value_node = nodes.const_gep_nodes[i];
    handle_const_gep_at(this, value_node, glb_init);
  }
}
