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

void print_named_constraints(AnalysisSet *as, int l = 0) {
  llvm::raw_ostream &os = llvm::outs();
  
  os.indent(l) << "Constraints {" << "\n";
  for (Constraints::iterator i = as->constraints.begin(),
         e = as->constraints.end(); i != e; ++i) {

    Constraint c = *i;      
    os.indent(l + 1) << "constraint {" << "\n";
    os.indent(l + 2) << "type:\t\t" << c.type_string() << "\n";

    Node *dest_node = as->nodes.find_node(c.dest);
    os.indent(l + 2) << "(d)est:\t" << c.dest
                     << dest_node->value_to_string(l + 2) << "\n";

    Node *src_node = as->nodes.find_node(c.src);
    os.indent(l + 2) << "(s)rc:\t" << c.src
                     << src_node->value_to_string(l + 2) << "\n";
    
    os.indent(l + 2) << "offset:\t" << c.off << "\n";
    os.indent(l + 1) << "}" << "\n";
  }
  os.indent(l) << "}" << "\n";  
}

static void assert_ret_next(AnalysisSet *as, u32 obj_node_id) {
  u32 ret_node_id = obj_node_id + FUNC_NODE_OFF_RET;
  assert(as->nodes.next == ret_node_id);  
}

static void assert_args_next(AnalysisSet *as, u32 obj_node_id) {
  u32 next_arg_node_id = obj_node_id + FUNC_NODE_OFF_ARG0;
  assert(as->nodes.next == next_arg_node_id);
}

static void assert_obj_sz(AnalysisSet *as, u32 obj_node_id) {
  u32 num_objects = as->nodes.next - obj_node_id;
  assert(as->nodes.nodes[obj_node_id]->obj_sz == num_objects);
}

static void init_addr_taken_function_args(AnalysisSet *as,
                                          llvm::Function *f,
                                          u32 obj_node_id) {
  // Sanity check.
  assert_ret_next(as, obj_node_id);

  // Add a placeholder node for the function's return value.
  // Note that this cannot be found by value. I'm not sure why.
  Node *node = as->nodes.find_node(obj_node_id);
  node->obj_sz++;
  as->nodes.add_unreachable(f);

  // Find the last pointer argument.
  u32 last_ptr = OVERFLOW_U32;
  u32 i = 0;
  for (llvm::Function::arg_iterator arg = f->arg_begin(),
         end = f->arg_end(); arg != end; i++, arg++) {
    if (llvm::isa<llvm::PointerType>(arg->getType())) {
      last_ptr = i;
    }
  }

  // Sanity check.
  assert_args_next(as, obj_node_id);
  
  // Up until the last pointer argument, if there is one, map
  // each argument value to a node.
  if (last_ptr != OVERFLOW_U32) {
    u32 j = 0;
    for (llvm::Function::arg_iterator arg = f->arg_begin(),
           end = f->arg_end(); j != last_ptr && arg != end; j++, arg++) {

      // Map argument to a node that can be found by value.
      u32 arg_id = as->nodes.add_value(arg);
      assert(arg_id == obj_node_id + FUNC_NODE_OFF_ARG0 + j);
      node->obj_sz++;

      // Keep track of arguments of address taken functions.
      as->addr_taken_args.insert(arg);
    }
  }
  
  // Make a vararg node if needed. Note that like the return
  // value this also cannot be found by value. Again, not sure
  // why.
  if (f->isVarArg()) {
    node->obj_sz++;
    as->nodes.add_unreachable(f);
  }

  // Sanity check.
  assert_obj_sz(as, obj_node_id);
}

static void init_addr_taken_function_signature(AnalysisSet *as,
                                               llvm::Function *f,
                                               std::string entry_point) {

  assert(f && f->hasAddressTaken());

  // Create a node at node_id to represent the function
  // pointer.
  u32 node_id = as->nodes.add_value(f);

  // Create a node at obj_node_id to represent the function
  // object backing the pointer.
  u32 obj_node_id = as->nodes.add_object(f, 1);

  // Create a constraint where node_id represents the
  // function pointer and obj_node_id represents the
  // function object whose address is being taken:
  //
  //   node_id = &obj_node_id
  as->constraints.add(ConstraintAddrOf, node_id, obj_node_id);

  // Skip special functions:
  //
  // 1. Treat external function args as external variables.
  //
  // 2. Treat entry point args as external variables. After all,
  //    if this is a C-style main with argv and envp, there's
  //    no object we can find in the program backing these
  //    pointer args anyway.
  if (f->getNameStr() == entry_point ||
      as->ext_info.is_ext(f)) {
    return;
  }

  // Handle addr taken function args.
  init_addr_taken_function_args(as, f, obj_node_id);
}

static void init_normal_function_args(AnalysisSet *as,
                                      llvm::Function *f) {  

  // Map each pointer argument value to a node.
  for (llvm::Function::arg_iterator arg = f->arg_begin(),
         end = f->arg_end(); arg != end; arg++) {
    if (llvm::isa<llvm::PointerType>(arg->getType())) {
      as->nodes.add_value(arg);
    }
  }
  
  // Map pointer returns to the function itself.
  if (llvm::isa<llvm::PointerType>(f->getReturnType())) {
    as->nodes.add_ret(f);
  }
  
  // Condense and map variable args to the function itself.
  if (f->isVarArg()) {
    as->nodes.add_vararg(f);
  }
}

static void init_normal_function_signature(AnalysisSet *as,
                                           llvm::Function *f,
                                           std::string entry_point) {

  // Treat the entry point's args and external function
  // args as external variables. Effectively skip.
  if (f->getNameStr() == entry_point ||
      as->ext_info.is_ext(f)) {
    return;
  }

  init_normal_function_args(as, f);
}

// Process function signatures. This means:
//
// + Add pointer and object nodes for functions with
//   their address taken.
//
// + Add pointer and object nodes for arguments with their
//   address taken.
static void init_function_signatures(llvm::Module *m,
                                     AnalysisSet *as,
                                     std::string entry_point = "main") {
  
  for (llvm::Module::iterator i = m->begin(), e = m->end();
       i != e; i++) {
    if (i->hasAddressTaken()) {
      init_addr_taken_function_signature(as, i, entry_point);
    } else {
      init_normal_function_signature(as, i, entry_point);
    }
  }
}

static bool is_null(llvm::Constant *c) {
  return c->isNullValue();
}

static bool is_const_null_ptr(llvm::Value *v) {
  return llvm::isa<llvm::ConstantPointerNull>(v);
}

static bool is_undefined(llvm::Value *v) {
  return llvm::isa<llvm::UndefValue>(v);
}

static bool is_single_value_type(llvm::Value *v) {
  return v->getType()->isSingleValueType();
}

static bool is_pointer(llvm::Value *v) {
  return llvm::isa<llvm::PointerType>(v->getType());    
}

static bool is_ptr_to_int(llvm::ConstantExpr *expr) {
  return expr && expr->getOpcode() == llvm::Instruction::PtrToInt;
}

static bool is_int_to_ptr(llvm::ConstantExpr *expr) {
  return expr && expr->getOpcode() == llvm::Instruction::IntToPtr;
}

static bool is_gep(llvm::ConstantExpr *expr) {
  return expr && expr->getOpcode() == llvm::Instruction::GetElementPtr;
}

static void assert_valid_const_expr(llvm::ConstantExpr *expr) {  
  assert(is_ptr_to_int(expr) ||
         is_int_to_ptr(expr) ||
         is_gep(expr));
}

static llvm::ConstantExpr *const_expr(llvm::Value **c) {
  for (llvm::ConstantExpr *expr = llvm::dyn_cast<llvm::ConstantExpr>(*c);
       expr; expr = llvm::dyn_cast_or_null<llvm::ConstantExpr>(*c)) {
      if (expr->getOpcode() == llvm::Instruction::BitCast) {
        *c = expr->getOperand(0);
      } else {
        assert_valid_const_expr(expr);
        return expr;
      }
  }
  
  return nullptr;
}

static llvm::ConstantExpr *const_expr(llvm::Constant **c) {
  for (llvm::ConstantExpr *expr = llvm::dyn_cast<llvm::ConstantExpr>(*c);
       expr; expr = llvm::dyn_cast_or_null<llvm::ConstantExpr>(*c)) {
      if (expr->getOpcode() == llvm::Instruction::BitCast) {
        *c = expr->getOperand(0);
      } else {
        assert_valid_const_expr(expr);
        return expr;
      }
  }
  
  return nullptr;
}

static llvm::ConstantStruct *const_struct(llvm::Constant *c) {
  return llvm::dyn_cast<llvm::ConstantStruct>(c);
}

static llvm::ConstantArray *const_array(llvm::Constant *c) {
  return llvm::dyn_cast<llvm::ConstantArray>(c);
}

struct GlobalType {
  const llvm::Type *type;
  bool is_array;

  GlobalType(llvm::GlobalVariable *g) {
    type = g->getType()->getContainedType(0);
    while (const llvm::ArrayType *at = llvm::dyn_cast<llvm::ArrayType>(type)) {
      type = at->getElementType();
      is_array = true;
    }
  }
};

static const llvm::StructType *struct_type(const llvm::Type *typ) {
  return llvm::dyn_cast<llvm::StructType>(typ);
}

static void init_global_signature(AnalysisSet *as, llvm::GlobalVariable *g) {
  assert(g);

  // N.B. Not sure this works for empty structs.
  //      Also not sure this syncs well with array
  //      intializers.

  // Find the type of the global.
  GlobalType gt(g);

  // Allocate pointer node.
  u32 node_id = as->nodes.add_value(g);

  // Allocate object node(s).
  u32 obj_id = NodeNone;
  
  if (const llvm::StructType *st = struct_type(gt.type)) {
    // Handle structs.
    const std::vector<u32> sz = as->structs.get_sz(st);
    for (std::vector<u32>::const_iterator i = sz.begin(), e = sz.end();
         i != e; i++) {
      u32 _obj_id = as->nodes.add_object(g, *i, gt.is_array);
      // Use the first element to represent the whole thing.
      if (obj_id == NodeNone) {
        obj_id = _obj_id;
      }
    }
  } else {
    // Treat everything else like a single-value object,
    // including arrays.
    obj_id = as->nodes.add_object(g, 1, gt.is_array);
  }

  // Assert that the obj_id is valid.
  assert(obj_id != NodeNone);

  // Map the global pointer to the global object id.
  as->constraints.add(ConstraintAddrOf, node_id, obj_id);  
}

// Process global signatures. This means:
//
// + Add a pointer node for each global variable. We treat
//   globals as if their address is always taken, because they
//   are constant pointers in LLVM.
//
// + Add an object node for each field in the global type,
//   for example, one for each struct field.
static void init_global_variable_signatures(llvm::Module *m,
                                            AnalysisSet *as) {
  for (llvm::Module::global_iterator i = m->global_begin(),
         e = m->global_end(); i != e; i++) {
    init_global_signature(as, i);
  }  
}

class IDSet {
 public:
  llvm::DenseMap<u32, u32> cache;

  void add(u32 id, u32 num_fields) {
    cache[id] = num_fields;
  }

  bool lookup(u32 id, u32 *num_fields = 0) {
    llvm::DenseMap<u32, u32>::iterator i = cache.find(id);
    if (i == cache.end()) {
      return false;
    }
    if (num_fields)  {
      *num_fields = i->second;
    }
    return true;
  }
};

static const llvm::ConstantInt *const_int(llvm::Value *v) {
  return llvm::dyn_cast<llvm::ConstantInt>(v);
}

static u32 gep_off(AnalysisSet *as, llvm::User *u) {
  assert(u);

  u32 off = 0;
  for (llvm::gep_type_iterator i = llvm::gep_type_begin(*u),
         e = llvm::gep_type_end(*u); i != e; i++) {

    const llvm::StructType *st = struct_type(*i);
    if (!st) {
      continue;
    }

    const llvm::ConstantInt *op = const_int(i.getOperand());
    u32 index = op ? op->getZExtValue() : 0;
      
    const std::vector<u32> offsets = as->structs.get_off(st);
    assert(index < offsets.size());

    off += offsets[index];
  }

  return off;
}

static u32 init_gep(AnalysisSet *as,
                    llvm::ConstantExpr *expr,
                    IDSet *done_set) {

  assert(is_gep(expr));

  // Lookup or add a value node for the const expr.
  u32 node_id = as->nodes.find_value_node(expr, true);
  if (!node_id) {
    node_id = as->nodes.add_value(expr);
  }

  if (done_set->lookup(node_id)) {
    // Already processed.
    return node_id;
  }

  // Prematurely mark processed for cleaner returns.
  done_set->add(node_id, 1);

  llvm::Value *ptr = expr->getOperand(0);
  llvm::ConstantExpr *sub_expr = const_expr(&ptr);
  assert(!is_const_null_ptr(ptr));

  if (is_int_to_ptr(sub_expr)) {
    as->constraints.add(ConstraintAddrOf, node_id, NodeUnknownTarget);
    return node_id;
  }

  u32 ptr_node_id = as->nodes.find_value_node(ptr);
  if (!ptr_node_id) {
    ptr_node_id = as->nodes.add_value(ptr);
  }

  as->constraints.add(ConstraintGEP, node_id, ptr_node_id,
                      gep_off(as, expr));
         
  if (is_gep(sub_expr)) {
    init_gep(as, sub_expr, done_set);
  }

  return node_id;
}

u32 _init_global_value(AnalysisSet *as, llvm::Constant *c,
                       u32 obj_id, IDSet *done_set);

static u32 __init_global_value(AnalysisSet *as,
                               llvm::Constant *c,
                               u32 obj_id,
                               IDSet *done_set) {
  
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
    as->constraints.add(ConstraintAddrOf, obj_id, NodeUnknownTarget);
    return 1;
  }

  // Other const expr types (Bitcast, PtrToInt, IntToPtr)
  // are handled above (const_expr, skip, unknown target).
  if (expr) assert(is_gep(expr));
  
  if (llvm::ConstantStruct *cs = const_struct(c)) {
    // Handle structs.
    u32 off = 0;
    for (u32 i = 0; i < cs->getNumOperands(); i++) {
      off += _init_global_value(as, cs->getOperand(i),
                                obj_id + off, done_set);
    }
    return off;
  }

  if (llvm::ConstantArray *ca = const_array(c)) {
    // Handle arrays.
    u32 off = 0;
    for (u32 i = 0; i < ca->getNumOperands(); i++) {
      off += _init_global_value(as, ca->getOperand(i),
                                obj_id + off, done_set);
    }
    return off;
  }

  // Handle single value types.
  assert(is_single_value_type(c));

  if (is_pointer(c) && is_gep(expr)) {
    // Handle pointers to GEPs.
    u32 expr_id = init_gep(as, expr, done_set);
    as->constraints.add(ConstraintCopy, obj_id, expr_id);
    return 1;
  }

  if (is_pointer(c)) {
    // Handle pointers to everything else.
    u32 const_obj_id = as->nodes.find_object_node(c);
    as->constraints.add(ConstraintAddrOf, obj_id, const_obj_id);
    return 1;
  }

  // Nothing to do for non-pointers.
  return 1;
}

u32 _init_global_value(AnalysisSet *as, llvm::Constant *c,
                       u32 obj_id, IDSet *done_set) {
  assert(c);

  u32 num_fields;
  if (!done_set->lookup(obj_id, &num_fields)) {
    return num_fields;
  }
  
  num_fields = __init_global_value(as, c, obj_id, done_set);
  done_set->add(obj_id, num_fields);
  return num_fields;
}

static void init_global_value(AnalysisSet *as,
                              llvm::GlobalVariable *g,
                              IDSet *done_set) {
  assert(g);

  if (!g->hasInitializer()) {
    // No value to initialize.
    return;
  }

  // The id of the global object.
  u32 obj_id = as->nodes.find_object_node(g);
  
  // The global value to associate with the object.
  llvm::Constant *c = g->getInitializer();

  _init_global_value(as, c, obj_id, done_set);
  return;
}

// Process global variable values.
static void init_global_variable_values(llvm::Module *m, AnalysisSet *as) {
  IDSet done_set;
  for (llvm::Module::global_iterator i = m->global_begin(),
         e = m->global_end(); i != e; i++) {
    init_global_value(as, i, &done_set);
  }  
}

void AnalysisSet::init(llvm::Module *m) {
  // Create 3 placeholder nodes for each of the SpecialNodes,
  // excluding the NodeFirst. These are used to describe exceptional
  // states, like when we can't know the target of a pointer.
  for (u32 i = 0; i < NodeFirst; i++) {
    u32 obj_sz = 0;
    if (i == NodeUnknownTarget) {
      // Treat the unknown target as an object whose address has
      // been taken.
      obj_sz = 1;
      constraints.add(ConstraintAddrOf,
                      NodeConstToUnknownTarget,
                      NodeUnknownTarget);
    }
    nodes.add_unreachable(nullptr, obj_sz);
  }

  llvm::raw_ostream &os = llvm::outs();

  os << "Step 1" << "\n";
  os << "===========================================================" << "\n";
  nodes.print();
  print_named_constraints(this);
  os << "\n";

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
  // much nicer to have object nodes for all of the top-level
  // declarations (functions and global variables) first, because
  // then we don't risk creating unreachable objects.
  //
  // To do this, we can also strongly assert that all initialized
  // global variables have values that are either free-standing
  // constants or pointers to things that are already cached.
  init_function_signatures(m, this);

  os << "Step 2" << "\n";
  os << "===========================================================" << "\n";
  nodes.print();
  print_named_constraints(this);
  os << "\n";

  // Process global signatures. This means:
  //
  // + Add a pointer node for each global variable. We treat
  //   globals as if their address is always taken, because they
  //   are constant pointers in LLVM.
  //
  // + Add an object node for each field in the global type,
  //   for example, one for each struct field.
  //
  // Ordering:
  //
  // Same as init_function_signatures.
  init_global_variable_signatures(m, this);

  os << "Step 3" << "\n";
  os << "===========================================================" << "\n";
  nodes.print();
  print_named_constraints(this);
  os << "\n";

  // Process global variable values.
  //
  // This handles GEP expressions
  // that are *part* of global variable values. If a GEP expression
  // isn't part of a global variable value but still *references*
  // a global, it is handled later as the module's blocks are walked.
  init_global_variable_values(m, this);

  // N.B.
  //
  // What happens if the program is totally global? Meaning, what if
  // there are runtime values assigned to global variables? How is
  // that handled? Maybe that's a special case, but I don't see how
  // that is handled here.  

  os << "Step 4" << "\n";
  os << "===========================================================" << "\n";
  nodes.print();
  print_named_constraints(this);
  structs.print();
}

// TODO:
//
// Finish adding comments.
// X Fill in the trivial node lookup code.
// X Go through and make sure all little helper functions are defined.
// X Convert function code to simpler add_value and add_object helpers.
