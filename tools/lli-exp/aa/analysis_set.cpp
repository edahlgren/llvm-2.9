//===- analysis_set.cpp -- ------------------------------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//
#include "llvm/Argument.h"         // for llvm::Argument
#include "llvm/Constants.h"        // for llvm::ConstantExpr
#include "llvm/DerivedTypes.h"     // for llvm::StructType, llvm::PointerType
#include "llvm/Instructions.h"     // for llvm::CallInst, llvm::InvokeInst,
                                   //     llvm::CmpInst
#include "llvm/GlobalVariable.h"   // for llvm::Type
#include "llvm/LLVMContext.h"      // for llvm::getGlobalContext
#include "llvm/Module.h"           // for llvm::Function
#include "llvm/Type.h"             // for llvm::Type
#include "llvm/TypeSymbolTable.h"  // for llvm::TypeSymbolTable
#include "llvm/Value.h"            // for llvm::Value
#include "llvm/Support/CallSite.h" // for llvm::CallSite
#include "llvm/Support/Casting.h"  // for llvm::dyn_cast, llvm::isa
#include "llvm/Support/CFG.h"      // for llvm::succ_begin, llvm::succ_end
#include "llvm/Support/GetElementPtrTypeIterator.h"
#include "llvm/Support/InstIterator.h"

#include <algorithm> // for std::min
#include <utility>   // for std::pair, std::make_pair

#include "analysis_set.h"
#include "constraints.h"
#include "nodes.h"

static bool is_pointer(llvm::Value *v) {
  return llvm::isa<llvm::PointerType>(v->getType());    
}

static bool is_pointer(const llvm::Type *t) {
  return llvm::isa<llvm::PointerType>(t);
}

static bool is_const_null_ptr(llvm::Value *v) {
  return llvm::isa<llvm::ConstantPointerNull>(v);
}

void print_named_constraints(AnalysisSet *as, int l = 0) {
  llvm::raw_ostream &os = llvm::outs();
  
  os.indent(l) << "Constraints {" << "\n";
  for (Constraints::iterator i = as->constraints->begin(),
         e = as->constraints->end(); i != e; ++i) {

    Constraint c = *i;      
    os.indent(l + 1) << "constraint {" << "\n";
    os.indent(l + 2) << "type:\t\t" << c.type_string() << "\n";

    Node *dest_node = as->nodes->find_node(c.dest);
    os.indent(l + 2) << "(d)est:\t" << c.dest
                     << dest_node->value_to_string(l + 2) << "\n";

    Node *src_node = as->nodes->find_node(c.src);
    os.indent(l + 2) << "(s)rc:\t" << c.src
                     << src_node->value_to_string(l + 2) << "\n";
    
    os.indent(l + 2) << "offset:\t" << c.off << "\n";
    os.indent(l + 1) << "}" << "\n";
  }
  os.indent(l) << "}" << "\n";  
}

static void assert_ret_next(AnalysisSet *as, u32 obj_node_id) {
  u32 ret_node_id = obj_node_id + FUNC_NODE_OFF_RET;
  assert(as->nodes->next == ret_node_id);  
}

static void assert_args_next(AnalysisSet *as, u32 obj_node_id) {
  u32 next_arg_node_id = obj_node_id + FUNC_NODE_OFF_ARG0;
  assert(as->nodes->next == next_arg_node_id);
}

static void assert_obj_sz(AnalysisSet *as, u32 obj_node_id) {
  u32 num_objects = as->nodes->next - obj_node_id;
  assert(as->nodes->nodes[obj_node_id]->obj_sz == num_objects);
}

static void init_addr_taken_function_args(AnalysisSet *as,
                                          llvm::Function *f,
                                          u32 obj_node_id) {
  // Sanity check.
  assert_ret_next(as, obj_node_id);

  // Add a placeholder node for the function's return value.
  // Note that this cannot be found by value. I'm not sure why.
  Node *node = as->nodes->find_node(obj_node_id);
  node->obj_sz++;
  as->nodes->add_unreachable(f);

  // Find the last pointer argument.
  u32 last_ptr = OVERFLOW_U32;
  u32 i = 0;
  for (llvm::Function::arg_iterator arg = f->arg_begin(),
         end = f->arg_end(); arg != end; i++, arg++) {
    if (is_pointer(arg->getType())) {
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
      u32 arg_id = as->nodes->add_value(arg);
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
    as->nodes->add_unreachable(f);
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
  u32 node_id = as->nodes->add_value(f);

  // Create a node at obj_node_id to represent the function
  // object backing the pointer.
  u32 obj_node_id = as->nodes->add_object(f, 1);

  // Create a constraint where node_id represents the
  // function pointer and obj_node_id represents the
  // function object whose address is being taken:
  //
  //   node_id = &obj_node_id
  as->constraints->add(ConstraintAddrOf, node_id, obj_node_id);

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
    if (is_pointer(arg->getType())) {
      as->nodes->add_value(arg);
    }
  }
  
  // Map pointer returns to the function itself.
  if (is_pointer(f->getReturnType())) {
    as->nodes->add_ret(f);
  }
  
  // Condense and map variable args to the function itself.
  if (f->isVarArg()) {
    as->nodes->add_vararg(f);
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

static bool is_undefined(llvm::Value *v) {
  return llvm::isa<llvm::UndefValue>(v);
}

static bool is_single_value_type(llvm::Value *v) {
  return v->getType()->isSingleValueType();
}

static bool is_ptr_to_int(llvm::ConstantExpr *expr) {
  return expr && expr->getOpcode() == llvm::Instruction::PtrToInt;
}

static bool is_int_to_ptr(llvm::ConstantExpr *expr) {
  return expr && expr->getOpcode() == llvm::Instruction::IntToPtr;
}

static llvm::IntToPtrInst *get_int_to_ptr(llvm::Value *v) {
  return llvm::dyn_cast<llvm::IntToPtrInst>(v);
}

static bool is_gep(llvm::ConstantExpr *expr) {
  return expr && expr->getOpcode() == llvm::Instruction::GetElementPtr;
}

static llvm::GetElementPtrInst *get_gep(llvm::Value *v) {
  return llvm::dyn_cast<llvm::GetElementPtrInst>(v);
}

static llvm::ConstantExpr *get_const_expr(llvm::Value *v) {
  return llvm::dyn_cast<llvm::ConstantExpr>(v);
}

static llvm::Instruction *get_inst(llvm::Value *v) {
  return llvm::dyn_cast<llvm::Instruction>(v);
}

static llvm::GlobalVariable *get_global(llvm::Value *v) {
  return llvm::dyn_cast<llvm::GlobalVariable>(v);
}

static llvm::BitCastInst *get_bitcast(llvm::Value *v) {
  return llvm::dyn_cast<llvm::BitCastInst>(v);
}

static llvm::User *get_user(llvm::Value *v) {
  return llvm::dyn_cast<llvm::User>(v);
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
  u32 node_id = as->nodes->add_value(g);

  // Allocate object node(s).
  u32 obj_id = NodeNone;
  
  if (const llvm::StructType *st = struct_type(gt.type)) {
    // Handle structs.
    const std::vector<u32> sz = as->structs.get_sz(st);
    for (std::vector<u32>::const_iterator i = sz.begin(), e = sz.end();
         i != e; i++) {
      u32 _obj_id = as->nodes->add_object(g, *i, gt.is_array);
      // Use the first element to represent the whole thing.
      if (obj_id == NodeNone) {
        obj_id = _obj_id;
      }
    }
  } else {
    // Treat everything else like a single-value object,
    // including arrays.
    obj_id = as->nodes->add_object(g, 1, gt.is_array);
  }

  // Assert that the obj_id is valid.
  assert(obj_id != NodeNone);

  // Map the global pointer to the global object id.
  as->constraints->add(ConstraintAddrOf, node_id, obj_id);  
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
  u32 node_id = as->nodes->find_value_node(expr, true);
  if (!node_id) {
    node_id = as->nodes->add_value(expr);
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
    as->constraints->add(ConstraintAddrOf, node_id, NodeUnknownTarget);
    return node_id;
  }

  u32 ptr_node_id = as->nodes->find_value_node(ptr);
  if (!ptr_node_id) {
    ptr_node_id = as->nodes->add_value(ptr);
  }

  as->constraints->add(ConstraintGEP, node_id, ptr_node_id,
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
    as->constraints->add(ConstraintAddrOf, obj_id, NodeUnknownTarget);
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
    as->constraints->add(ConstraintCopy, obj_id, expr_id);
    return 1;
  }

  if (is_pointer(c)) {
    // Handle pointers to everything else.
    u32 const_obj_id = as->nodes->find_object_node(c);
    as->constraints->add(ConstraintAddrOf, obj_id, const_obj_id);
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
  u32 obj_id = as->nodes->find_object_node(g);
  
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

class BlockSet {
public:
  std::map<llvm::BasicBlock *, u32> cache;
};

static void init_pointer_instruction_nodes(AnalysisSet *as,
                                           llvm::Function *f) {
  
  for (llvm::inst_iterator ii = llvm::inst_begin(f),
         ei = llvm::inst_end(f); ii != ei; ii++) {
    llvm::Instruction *inst = &*ii;
    if (is_pointer(inst)) {
      as->nodes->add_value(inst);
    }
  }    
}

class BlockState {
public:
  llvm::BasicBlock *block;
  u32 position;
  bool contains_call;
  u32 constraints_sz;

  BlockState(llvm::BasicBlock *bb, u32 position) :
    block(bb),
    position(position),
    contains_call(false),
    constraints_sz(0) {}
};

u32 find_value_node_const_ptr(AnalysisSet *as, llvm::Value *v);

static void process_load(AnalysisSet *as,
                         llvm::Instruction *inst) {
  
  assert(inst);
  llvm::LoadInst *li = llvm::cast<llvm::LoadInst>(inst);

  u32 node_id = as->nodes->find_value_node(li);
  u32 sub_node_id = find_value_node_const_ptr(as, li->getOperand(0));
  if (!sub_node_id) {
    return;
  }

  as->constraints->add(ConstraintLoad, node_id, sub_node_id);
}

static void process_store(AnalysisSet *as,
                          llvm::Instruction *inst) {

  assert(inst);
  llvm::StoreInst *si = llvm::cast<llvm::StoreInst>(inst);

  llvm::Value *src = si->getOperand(0);
  llvm::Value *dest = si->getOperand(1);

  if (!is_pointer(src->getType()))
    return;

  u32 sub_dest_id = find_value_node_const_ptr(as, dest);
  u32 sub_src_id = find_value_node_const_ptr(as, src);

  if (sub_src_id && sub_dest_id)
    as->constraints->add(ConstraintStore, sub_dest_id, sub_src_id);
}

static const llvm::Type *trace_alloc_type(AnalysisSet *as,
                                    llvm::Instruction *inst) {
  assert(inst);

  const llvm::Type *mt = inst->getType()->getContainedType(0);
  while (const llvm::ArrayType *at= llvm::dyn_cast<llvm::ArrayType>(mt))
    mt = at->getElementType();
  
  u32 msz = 0;
  if (const llvm::StructType *st= llvm::dyn_cast<llvm::StructType>(mt))
    msz = as->structs.get_sz(st).size();

  bool found = 0;
  for (llvm::Value::use_iterator it= inst->use_begin(),
         ie = inst->use_end(); it != ie; ++it) {
    
    llvm::CastInst *ci= llvm::dyn_cast<llvm::CastInst>(*it);
    if (!ci || !llvm::isa<llvm::PointerType>(ci->getType()))
      continue;
    
    found = true;
    const llvm::Type *t = ci->getType()->getContainedType(0);

    u32 sz = 0;
    while (const llvm::ArrayType *at = llvm::dyn_cast<llvm::ArrayType>(t))
      t = at->getElementType();
    
    if (const llvm::StructType *st = llvm::dyn_cast<llvm::StructType>(t))
      sz= as->structs.get_sz(st).size();
    
    if (sz > msz) {
      msz = sz;
      mt = t;
    }
  }
  
  if (!found && !msz)
    return as->structs.max_struct;
  
  return mt;
}

static void process_callee(AnalysisSet *as, u32 node_id,
                           llvm::Function *f) {

  llvm::Value *v = as->nodes->find_node(node_id)->val;
  if (f && as->ext_info.no_struct_alloc(f)) {
    u32 obj_id = as->nodes->add_object(v, 1, true);
    as->constraints->add(ConstraintAddrOf, node_id, obj_id);
    return;
  }

  llvm::Instruction *inst = llvm::dyn_cast_or_null<llvm::Instruction>(v);
  assert(inst);
  llvm::CallSite cs(inst);
  
  if (!f || as->ext_info.is_alloc(f) ||
      (as->ext_info.get_type(f) == EFT_REALLOC &&
       llvm::isa<llvm::ConstantPointerNull>(cs.getArgument(0)))) {

    const llvm::Type *t = trace_alloc_type(as, inst);

    u32 obj_id = NodeNone;
    if (const llvm::StructType *st = llvm::dyn_cast<llvm::StructType>(t)) {
      const std::vector<u32> &sz= as->structs.get_sz(st);

      for (u32 i = 0; i < sz.size(); i++) {
        u32 _obj_id = as->nodes->add_object(inst, sz[i], true);         
        if (obj_id != NodeNone)
          obj_id = _obj_id;
      }
    } else {
      obj_id = as->nodes->add_object(inst, 1, true);
    }

    if (f)
      as->constraints->add(ConstraintAddrOf, node_id, obj_id);    
    return;
  }

  if (as->ext_info.has_static(f)) {
    bool _static = as->ext_info.has_static2(f);
    std::string func_name = f->getNameStr();
    
    std::map<std::string, u32>::const_iterator i_srn =
      as->static_returns.find(func_name);

    u32 obj_id = NodeNone;
    if (i_srn != as->static_returns.end()) {
      obj_id = i_srn->second;
      as->constraints->add(ConstraintAddrOf, node_id, obj_id);
      return;
    }

    if (_static) {
      obj_id = as->nodes->add_object(inst, 2, true);
      as->nodes->add_object(inst, 1, true);
      as->constraints->add(ConstraintAddrOf, node_id, obj_id);
    } else {
      const llvm::Type *t = inst->getType()->getContainedType(0);
      if (const llvm::StructType *st = llvm::dyn_cast<llvm::StructType>(t)) {
        const std::vector<u32> &sz= as->structs.get_sz(st);
        
        for (u32 i = 0; i < sz.size(); i++) {
          u32 _obj_id = as->nodes->add_object(inst, sz[i], true);         
          if (obj_id != NodeNone)
            obj_id = _obj_id;
        }
      } else {
        obj_id = as->nodes->add_object(inst, 1, true);
      }
    }
    
    as->static_returns[func_name] = obj_id;
    as->constraints->add(ConstraintAddrOf, node_id, obj_id);
  }
}

static u32 eft_la_num_args(AnalysisSet *as, llvm::Function *f) {
  switch(as->ext_info.get_type(f)) {
  case EFT_L_A0:
    return 0;
  case EFT_L_A1:
    return 1;
  case EFT_L_A2:
    return 2;
  case EFT_L_A8:
    return 8;
  default:
    assert(false && "unexpected external function type");
  }
}

u32 get_max_offset(AnalysisSet *as, llvm::Value *v) {
  
  const llvm::Type *t = v->getType();

  const llvm::IntegerType *min_int =
    llvm::Type::getInt8Ty(llvm::getGlobalContext());
  assert(is_pointer(t) && t->getContainedType(0) == min_int);

  if (llvm::ConstantExpr *expr = get_const_expr(v)) {
    t = expr->getOperand(0)->getType();
  } else if (llvm::BitCastInst *bi = get_bitcast(v)) {
    t = bi->getOperand(0)->getType();
  } else if (llvm::User *u = get_user(v)) {
    u32 msz = 1;
    for(llvm::User::op_iterator it= u->op_begin(), ie= u->op_end();
        it != ie; ++it){
      
      llvm::Value *v= it->get();
      t = v->getType();
      
      if (!is_pointer(t))
        continue;
      
      t = t->getContainedType(0);
      while (const llvm::ArrayType *at = llvm::dyn_cast<llvm::ArrayType>(t))
        t = at->getElementType();
      
      if (const llvm::StructType *st = llvm::dyn_cast<llvm::StructType>(t)){
        u32 sz = as->structs.get_sz(st).size();
        if (msz < sz)
          msz = sz;
      }
    }
    return msz;
  }

  return 1;
}

static void add_load_store_cons(AnalysisSet *as,
                                llvm::Value *dest,
                                llvm::Value *src,
                                u32 size = 0) {
    assert(dest && src);
    u32 dest_index = find_value_node_const_ptr(as, dest);
    u32 src_index = find_value_node_const_ptr(as, src);

    if (!dest_index || !src_index)
      return;      

    if (!size) {
      size = std::min(get_max_offset(as, dest),
                      get_max_offset(as, src));
    }
    
    for (u32 i = 0; i < size; i++) {
      u32 tnode_id = as->nodes->add_unreachable(0);
      as->constraints->add(ConstraintLoad, tnode_id, src_index, i);
      as->constraints->add(ConstraintStore, dest_index, tnode_id, i);
    }

    return;
}

static void process_EFT_L_A(AnalysisSet *as,
                            llvm::CallSite &cs,
                            llvm::Function *f) {

  llvm::Instruction *inst = cs.getInstruction();
  if (!is_pointer(inst))
    return;

  u32 node_id = as->nodes->find_value_node(inst);
  u32 i_arg = eft_la_num_args(as, f);

  llvm::Value *src= cs.getArgument(i_arg);
  if (!is_pointer(src->getType())){
    as->constraints->add(ConstraintAddrOf, node_id, NodeUnknownTarget);
    return;
  }

  u32 sub_node_id = find_value_node_const_ptr(as, src);
  if (sub_node_id)
    as->constraints->add(ConstraintCopy, node_id, sub_node_id);
}

static void process_EFT_L_A0__A0R_A1R(AnalysisSet *as,
                                      llvm::CallSite &cs,
                                      llvm::Function *f) {

  llvm::Value *dest = cs.getArgument(0);
  llvm::Value *src = cs.getArgument(1);
  add_load_store_cons(as, dest, src);
    
  llvm::Instruction *inst = cs.getInstruction();
  if (is_pointer(inst->getType()))
    as->constraints->add(ConstraintCopy,
                         as->nodes->find_value_node(inst),
                         as->nodes->find_value_node(dest));
}

static void process_EFT_A1R_A0R(AnalysisSet *as,
                                llvm::CallSite &cs,
                                llvm::Function *f) {
  
  llvm::Value *dest = cs.getArgument(0);
  llvm::Value *src = cs.getArgument(1);
  add_load_store_cons(as, dest, src);
}

static void process_EFT_A3R_A1R_NS(AnalysisSet *as,
                                   llvm::CallSite &cs,
                                   llvm::Function *f) {

  llvm::Value *dest = cs.getArgument(3);
  llvm::Value *src = cs.getArgument(1);
  add_load_store_cons(as, dest, src, 1);
}

static void process_EFT_A1R_A0(AnalysisSet *as,
                               llvm::CallSite &cs,
                               llvm::Function *f) {
  
  llvm::Value *dest = cs.getArgument(1);
  u32 dest_index = find_value_node_const_ptr(as, dest);

  llvm::Value *src = cs.getArgument(0);
  u32 src_index = find_value_node_const_ptr(as, src);
  
  if (dest_index && src_index)
    as->constraints->add(ConstraintStore, dest_index, src_index);
}

static void process_EFT_A2R_A1(AnalysisSet *as,
                               llvm::CallSite &cs,
                               llvm::Function *f) {
  
  llvm::Value *dest = cs.getArgument(2);
  u32 dest_index = find_value_node_const_ptr(as, dest);

  llvm::Value *src = cs.getArgument(1);
  u32 src_index = find_value_node_const_ptr(as, src);
  
  if (dest_index && src_index)
    as->constraints->add(ConstraintStore, dest_index, src_index);
}

static void process_EFT_A4R_A1(AnalysisSet *as,
                               llvm::CallSite &cs,
                               llvm::Function *f) {

  llvm::Value *dest = cs.getArgument(4);
  u32 dest_index = find_value_node_const_ptr(as, dest);

  llvm::Value *src = cs.getArgument(1);
  u32 src_index = find_value_node_const_ptr(as, src);
  
  if (dest_index && src_index)
    as->constraints->add(ConstraintStore, dest_index, src_index);
}

static void process_EFT_L_A0__A2R_A0(AnalysisSet *as,
                                     llvm::CallSite &cs,
                                     llvm::Function *f) {

  llvm::Instruction *inst = cs.getInstruction();
  if (is_pointer(inst)) {
    u32 node_id = as->nodes->find_value_node(inst);
    llvm::Value *src = cs.getArgument(0);

    if (!is_pointer(src->getType())) {
      as->constraints->add(ConstraintAddrOf, node_id,
                           NodeUnknownTarget);
    } else {
      u32 sub_node_id = find_value_node_const_ptr(as, src);
      if (sub_node_id)
        as->constraints->add(ConstraintCopy, node_id, sub_node_id);
    }
  }

  llvm::Value *dest = cs.getArgument(2);
  u32 node_id = find_value_node_const_ptr(as, dest);

  llvm::Value *src = cs.getArgument(0);
  u32 sub_node_id = find_value_node_const_ptr(as, src);
  if (node_id && sub_node_id)
    as->constraints->add(ConstraintStore, node_id, sub_node_id);
}

static u32 eft_a_new_num_args(AnalysisSet *as, llvm::Function *f) {
  switch(as->ext_info.get_type(f)) {
  case EFT_A1R_NEW:
    return 1;
  case EFT_A2R_NEW:
    return 2;
  case EFT_A4R_NEW:
    return 4;
  case EFT_A11R_NEW:
    return 11;
  default:
    return 0;
  }
}

static void process_EFT_A_NEW(AnalysisSet *as,
                              llvm::CallSite &cs,
                              llvm::Function *f) {
  
  u32 i_arg = eft_a_new_num_args(as, f);

  llvm::Value *dest = cs.getArgument(i_arg);
  u32 dest_node_id = find_value_node_const_ptr(as, dest);
  if (!dest_node_id)
    return;

  const llvm::Type *t = dest->getType()->getContainedType(0);
  assert(is_pointer(t));

  t = t->getContainedType(0);
  while (const llvm::ArrayType *at = llvm::dyn_cast<llvm::ArrayType>(t))
    t = at->getElementType();

  u32 obj_id = NodeNone;
  if (const llvm::StructType *st = llvm::dyn_cast<llvm::StructType>(t)) {
    const std::vector<u32> &sz= as->structs.get_sz(st);
    //  FIXME: X/0 shouldn't really have a value because it's not
    //  pointed to by any program variable, but for now we require
    //  all obj_nodes to have one.
    for (u32 i = 0; i < sz.size(); i++) {
      u32 _obj_id = as->nodes->add_object(dest, sz[i], true);
      if (obj_id == NodeNone)
        obj_id = _obj_id;
    }
  } else {
    obj_id = as->nodes->add_object(dest, 1, true);
  }
  assert(obj_id != NodeNone);
  
  u32 node_id = as->nodes->add_unreachable(0);
  as->constraints->add(ConstraintAddrOf, node_id, obj_id);
  as->constraints->add(ConstraintStore, dest_node_id, node_id);
}

static void process_external_call(AnalysisSet *as,
                                  llvm::CallSite &cs,
                                  llvm::Function *f) {
  
  assert(f && as->ext_info.is_ext(f));  
  if (as->ext_info.is_alloc(f) || as->ext_info.has_static(f))
    return;

  ExternalFunctionType tF = as->ext_info.get_type(f);
  switch (tF){
  case EFT_REALLOC:
    if (is_const_null_ptr(cs.getArgument(0)))
      break;

  case EFT_L_A0:
  case EFT_L_A1:
  case EFT_L_A2:
  case EFT_L_A8:
    process_EFT_L_A(as, cs, f);
    break;
    
  case EFT_L_A0__A0R_A1R:
    process_EFT_L_A0__A0R_A1R(as, cs, f);
    break;

  case EFT_A1R_A0R:
    process_EFT_A1R_A0R(as, cs, f);
    break;
    
  case EFT_A3R_A1R_NS:
    process_EFT_A3R_A1R_NS(as, cs, f);
    break;

  case EFT_A1R_A0:
    process_EFT_A1R_A0(as, cs, f);
    break;

  case EFT_A2R_A1:
    process_EFT_A2R_A1(as, cs, f);
    break;

  case EFT_A4R_A1:
    process_EFT_A4R_A1(as, cs, f);
    break;

  case EFT_L_A0__A2R_A0:
    process_EFT_L_A0__A2R_A0(as, cs, f);
    break;

  case EFT_A0R_NEW:
  case EFT_A1R_NEW:
  case EFT_A2R_NEW:
  case EFT_A4R_NEW:
  case EFT_A11R_NEW:
    process_EFT_A_NEW(as, cs, f);
    break;

  case EFT_ALLOC:
  case EFT_NOSTRUCT_ALLOC:
  case EFT_STAT:
  case EFT_STAT2:
    assert(false && "alloc type func. are not handled here");
    break;
    
  case EFT_NOOP:
  case EFT_OTHER:
    break;

  default:
    assert(false && "unknown ext.func type");
  }  
}

static void process_direct_call(AnalysisSet *as,
                                llvm::CallSite &cs,
                                llvm::Function *f) {
  assert(f);

  if (is_pointer(cs.getType())) {
    u32 node_id = as->nodes->find_value_node(cs.getInstruction());
    if (!is_pointer(f->getReturnType())) {
      as->constraints->add(ConstraintAddrOf, node_id, NodeUnknownTarget);
    } else {
      u32 ret_node_id = as->nodes->find_ret_node(f);
      assert(ret_node_id);
      as->constraints->add(ConstraintCopy, node_id, ret_node_id);
    }
  }

  llvm::CallSite::arg_iterator arg_it = cs.arg_begin(),
    arg_end = cs.arg_end();
  
  llvm::Function::arg_iterator func_it= f->arg_begin(),
    func_end= f->arg_end();
  
  for(; func_it != func_end; ++arg_it, ++func_it) {
    if (arg_it == arg_end)
      break;

    llvm::Value *aa = *arg_it, *fa = func_it;
    if (!is_pointer(fa->getType()))
      continue;

    u32 fa_node_id = as->nodes->find_value_node(fa);
    if (!is_pointer(aa)) {
      as->constraints->add(ConstraintAddrOf, fa_node_id,
                           NodeUnknownTarget);
    } else {
      u32 aa_node_id = as->nodes->find_value_node(aa);
      if (aa_node_id)
        as->constraints->add(ConstraintCopy, fa_node_id, aa_node_id);
    }
  }

  if (!f->isVarArg()) {
    assert(arg_it == arg_end && "too many args to non-vararg func");
  } else {
    u32 va_node_id = as->nodes->find_vararg_node(f);
    assert(va_node_id);

    for (; arg_it != arg_end; arg_it++) {
      llvm::Value *aa = *arg_it;

      if (!is_pointer(aa)) {
        as->constraints->add(ConstraintAddrOf, va_node_id,
                             NodeUnknownTarget);
      } else {
        u32 aa_node_id = find_value_node_const_ptr(as, aa);
        if (aa_node_id)
          as->constraints->add(ConstraintCopy, va_node_id, aa_node_id);
      }
    }
  }
}

static void process_indirect_call(AnalysisSet *as,
                                llvm::CallSite &cs,
                                llvm::Function *f) {

  llvm::Value *called = cs.getCalledValue();
  assert(called);

  if (llvm::isa<llvm::InlineAsm>(called))
    return;

  llvm::Instruction * inst = cs.getInstruction();
  u32 called_id = find_value_node_const_ptr(as, called);
  assert(called_id && "null callee");
  
  ConstraintGraphMetadata *meta = as->cgraph->meta;
  meta->indirect_call_func_nodes.insert(called_id);

  if (is_pointer(cs.getType())) {
    u32 node_id = as->nodes->find_value_node(inst);
    as->constraints->add(ConstraintLoad, node_id, called_id,
                         FUNC_NODE_OFF_RET);

    Constraint c(ConstraintLoad, node_id, called_id, FUNC_NODE_OFF_RET);
    meta->ret_arg_call_cons[c].insert(inst);
  }

  u32 arg_offset = FUNC_NODE_OFF_ARG0;
  for (llvm::CallSite::arg_iterator arg_it = cs.arg_begin(),
         arg_end = cs.arg_end(); arg_it != arg_end; ++arg_it, ++arg_offset) {

    llvm::Value *aa= *arg_it;
    //FIXME: don't add these cons. if the current formal arg in the
    //  function ptr type is of non-ptr type
    if (!is_pointer(aa)) {
      as->constraints->add(ConstraintStore, called_id,
                           NodeConstToUnknownTarget, arg_offset);

      Constraint c(ConstraintStore, called_id,
                   NodeConstToUnknownTarget, arg_offset);
      meta->ret_arg_call_cons[c].insert(inst);
    } else {
      u32 aa_node_id = find_value_node_const_ptr(as, aa);
      if (aa_node_id) {      
        as->constraints->add(ConstraintStore, called_id,
                             aa_node_id, arg_offset);

        Constraint c(ConstraintStore, called_id, aa_node_id, arg_offset);
        meta->ret_arg_call_cons[c].insert(inst);
      }
    }
  }  
}

static void process_call(AnalysisSet *as,
                         llvm::Instruction *inst) {
  assert(inst);
  llvm::CallSite cs(inst);

  llvm::Value *callee = inst->getOperand(0);
  llvm::Function *f = llvm::dyn_cast<llvm::Function>(callee);
  if (!f) {
    llvm::ConstantExpr *e = llvm::dyn_cast<llvm::ConstantExpr>(callee);
    if (e && e->getOpcode() == llvm::Instruction::BitCast) {
      f = llvm::dyn_cast<llvm::Function>(e->getOperand(0));
    }
  }

  u32 node_id = as->nodes->find_value_node(inst, true);
  if (node_id) {
    process_callee(as, node_id, f);
  }

  if (f) {
    if (as->ext_info.is_ext(f))
      process_external_call(as, cs, f);
    else
      process_direct_call(as, cs, f);
  } else {
    process_indirect_call(as, cs, f);
  }
}

static void process_return(AnalysisSet *as, BlockState *bs,
                           llvm::Instruction *inst) {

  assert(inst);
  llvm::ReturnInst *ri = llvm::cast<llvm::ReturnInst>(inst);

  if (!ri->getNumOperands())
    return;

  llvm::Value *src = ri->getOperand(0);
  if (!is_pointer(src))
    return;

  llvm::Function *f = ri->getParent()->getParent();

  u32 ret_node_id = as->nodes->find_ret_node(f),
    src_node_id = find_value_node_const_ptr(as, src);

  assert(ret_node_id);
  if (src_node_id)
    as->constraints->add(ConstraintCopy, ret_node_id, src_node_id);

  if (bs->contains_call) {
    u32 node_id = as->cgraph->create_node(PNODE);
    as->cgraph->add_edge(bs->position, node_id);
  }

  ConstraintGraphMetadata *meta = as->cgraph->meta;  
  assert(!meta->func_ret_nodes.count(bs->block->getParent()));
  meta->func_ret_nodes[bs->block->getParent()] = bs->position;
}

static void process_alloc(AnalysisSet *as,
                          llvm::Instruction *inst) {

  assert(inst);
  
  llvm::AllocaInst *ai = llvm::cast<llvm::AllocaInst>(inst);
  u32 node_id = as->nodes->find_value_node(ai);

  // Find out which type of data was allocated.
  bool weak =  false;
  const llvm::Type *t = ai->getAllocatedType();

  // An array is considered the same as 1 element.
  while (const llvm::ArrayType *at = llvm::dyn_cast<llvm::ArrayType>(t)) {
    weak = true;
    t = at->getElementType();
  }

  u32 obj_id = NodeNone;
  if (const llvm::StructType *st = llvm::dyn_cast<llvm::StructType>(t)) {
    const std::vector<u32> &sz= as->structs.get_sz(st);
    
    for (u32 i = 0; i < sz.size(); i++) {
      u32 _obj_id = as->nodes->add_object(ai, sz[i], weak);         
      if (obj_id != NodeNone)
        obj_id = _obj_id;
    }
  } else {
    obj_id = as->nodes->add_object(inst, 1, weak);
  }

  as->constraints->add(ConstraintAddrOf, node_id, obj_id);
}

static bool trace_int(llvm::Value *v,
                      llvm::DenseSet<llvm::Value *> &src,
                      llvm::DenseMap<llvm::Value *, bool> &seen,
                      u32 depth = 0) {

  llvm::DenseMap<llvm::Value *, bool>::iterator i_seen = seen.find(v);
  if(i_seen != seen.end())
    return i_seen->second;

  const llvm::Type *tl = v->getType();
  assert(v && llvm::isa<llvm::IntegerType>(tl) &&
         "trying to trace non-int value");
  seen[v]= 0;

  u32 opcode = 0;
  std::vector<llvm::Value *> ops;

  //Arguments and numbers provide unknown addresses.
  if (llvm::isa<llvm::Argument>(v) ||
      llvm::isa<llvm::ConstantInt>(v)) {
    seen[v]= 1;
    return true;
  } else if (llvm::ConstantExpr *ce = get_const_expr(v)) {
    opcode = ce->getOpcode();
    for (u32 i = 0; i < ce->getNumOperands(); i++) {
      ops.push_back(ce->getOperand(i));
    }
  } else if (llvm::Instruction *inst = get_inst(v)) {
    opcode = inst->getOpcode();
    for (u32 i = 0; i < inst->getNumOperands(); i++) {
      ops.push_back(inst->getOperand(i));
    }
  } else {
    assert(false && "unknown type of int value");
  }

  bool r = false;
  
  assert(opcode);
  switch (opcode){
  case llvm::Instruction::Invoke:
  case llvm::Instruction::FPToUI:
  case llvm::Instruction::FPToSI:
  case llvm::Instruction::ICmp:
  case llvm::Instruction::FCmp:
  case llvm::Instruction::Call:
  case llvm::Instruction::VAArg:
  case llvm::Instruction::ExtractElement:
    seen[v] = true;
    return true;
    
  case llvm::Instruction::PtrToInt:
    src.insert(ops[0]);
    return false;
    
  case llvm::Instruction::Load: {
    if (llvm::GlobalVariable *g = get_global(ops[0])) {
      if (g->hasInitializer() && g->isConstant()) {
        llvm::Value *gi = g->getInitializer();
        r = trace_int(gi, src, seen, depth + 1);
        if (r)
          seen[v] = true;

        return r;
      }
    }

    llvm::LoadInst *li0 = llvm::cast<llvm::LoadInst>(v);
    llvm::Value *addr = ops[0];
    llvm::Value *s = 0;

    bool found = false;
    llvm::BasicBlock *bb = li0->getParent();
    for (llvm::BasicBlock::iterator it = bb->begin(), ie = bb->end();
         !found && it != ie; ++it) {
      if (llvm::StoreInst *si = llvm::dyn_cast<llvm::StoreInst>(it)) {
        if (si->getPointerOperand() == addr)
          s = si->getOperand(0);
      } else if (llvm::LoadInst *li = llvm::dyn_cast<llvm::LoadInst>(it)) {
        found = li == li0;
      }
    }
    assert(found);

    if (s) {
      r = trace_int(s, src, seen, depth + 1);
      if (r)
        seen[v] = true;

      return r;
    }

    seen[v] = true;
    return true;
  }
    
  case llvm::Instruction::Shl:
  case llvm::Instruction::LShr:
  case llvm::Instruction::AShr:
  case llvm::Instruction::Trunc:
  case llvm::Instruction::ZExt:
  case llvm::Instruction::SExt:
  case llvm::Instruction::BitCast: {
    const llvm::Type *tr = ops[0]->getType();
    if (llvm::isa<llvm::IntegerType>(tr)) {
      r = trace_int(ops[0], src, seen, depth + 1);
      if (r)
        seen[v] = true;
      
      return r;
    }

    assert(opcode == llvm::Instruction::BitCast &&
           "invalid operand for int insn");

    llvm::Type::TypeID t = tr->getTypeID();
    assert((t == llvm::Type::FloatTyID ||
            t == llvm::Type::DoubleTyID) &&
           "invalid cast to int");

    seen[v] = true;
    return true;
  }

  case llvm::Instruction::Add:
  case llvm::Instruction::Sub:
  case llvm::Instruction::Mul:
  case llvm::Instruction::UDiv:
  case llvm::Instruction::SDiv:
  case llvm::Instruction::URem:
  case llvm::Instruction::SRem:
  case llvm::Instruction::And:
  case llvm::Instruction::Or:
  case llvm::Instruction::Xor:
    r = trace_int(ops[0], src, seen, depth + 1) &
      trace_int(ops[1], src, seen, depth + 1);
    if (r)
      seen[v] = true;
    
    return r;

  case llvm::Instruction::PHI:
    r = false;
    for (u32 i = 0; i < ops.size(); i++) {
      //Sometimes a pointer or other value can come into an
      // int-type phi node.
      const llvm::Type *t = ops[i]->getType();
      if (llvm::isa<llvm::IntegerType>(t)) {
        r |= trace_int(ops[i], src, seen, depth + 1);
      } else if(llvm::isa<llvm::PointerType>(t)) {
        src.insert(ops[i]);
      } else {
        r = true;
      }
    }
    
    if (r)
      seen[v] = true;
    
    return r;
    
  case llvm::Instruction::Select:
    r = trace_int(ops[0], src, seen, depth + 1) |
      trace_int(ops[1], src, seen, depth + 1);
    
    if (r)
      seen[v] = true;
    
    return r;
    
  default:
    assert(false && "this insn should not produce an int value");
  }

  assert(false && "unreachable");
  return false;
}

static void process_int2ptr(AnalysisSet *as,
                            llvm::Value *v) {

  assert(v);

  u32 dest_id = 0;
  llvm::Value *op = 0;

  if (llvm::IntToPtrInst *ii = get_int_to_ptr(v)) {
    dest_id = as->nodes->find_value_node(ii);
    op = ii->getOperand(0);    
  } else if (llvm::GetElementPtrInst *gi = get_gep(v)) {
    assert(is_const_null_ptr(gi->getOperand(0)) &&
           gi->getNumOperands() == 2 &&
           "only single-index GEP of null is used for i20");
    dest_id = as->nodes->find_value_node(gi);
    op = ii->getOperand(1);
  } else if (llvm::ConstantExpr *expr = get_const_expr(v)) {
    assert(!as->nodes->contains_value(expr));
    as->nodes->add_value(expr);
    
    if (expr->getOpcode() == llvm::Instruction::IntToPtr) {
      op = expr->getOperand(0);
    } else if (expr->getOpcode() == llvm::Instruction::GetElementPtr) {
      assert(is_const_null_ptr(expr->getOperand(0)) &&
             expr->getNumOperands() == 2 &&
             "only single-index GEP of null is used for i2p");
      op = expr->getOperand(1);
    } else {
      assert(false && "const expr is not i2p or gep");
    }
  } else {
    assert(false && "value is not i2p, gep, or const expr");
  }

  llvm::DenseSet<llvm::Value *> src;
  llvm::DenseMap<llvm::Value *, bool> seen;
  
  bool has_i2p = trace_int(op, src, seen);

  for (llvm::DenseSet<llvm::Value *>::iterator it = src.begin(),
         ie= src.end(); it != ie; ++it) {
    llvm::Value *s = *it;
    u32 sub_node_id = find_value_node_const_ptr(as, s);
    if (sub_node_id)
      as->constraints->add(ConstraintCopy, dest_id, sub_node_id);
  }

  if (has_i2p) {
    as->constraints->add(ConstraintAddrOf, dest_id, NodeUnknownTarget);
  }
}

u32 find_value_node_const_ptr(AnalysisSet *as, llvm::Value *v) {
  assert(v);
    
  u32 node_id = as->nodes->find_value_node(v, true);
  if (node_id) {
    return node_id;
  }

  llvm::Constant *c = llvm::dyn_cast<llvm::Constant>(v);
  assert(c && llvm::isa<llvm::PointerType>(c->getType()) &&
         "value without node is not a const pointer");
  assert(!llvm::isa<llvm::GlobalValue>(c) &&
         "global const pointer has no node");

  if (llvm::isa<llvm::ConstantPointerNull>(c) ||
      llvm::isa<llvm::UndefValue>(c)) {
    return 0;
  }

  llvm::ConstantExpr *e = llvm::dyn_cast<llvm::ConstantExpr>(c);
  assert(e && "unknown const pointer type");
  
  switch (e->getOpcode()) {
  case llvm::Instruction::BitCast:
    return find_value_node_const_ptr(as, e->getOperand(0));
      
  case llvm::Instruction::IntToPtr:
    process_int2ptr(as, e);
    return as->nodes->find_value_node(e);

  case llvm::Instruction::GetElementPtr:
    if (is_const_null_ptr(e->getOperand(0))) {
      if(e->getNumOperands() > 2)
        return 0;

      process_int2ptr(as, e);
      return as->nodes->find_value_node(e);
    }
    assert(false && "unexpected getelementptr const expression");
      
  default:
    assert(false && "unknown opcode in const pointer expression");
  }

  return 0;
}

static void process_gep(AnalysisSet *as,
                        llvm::Instruction *inst) {

  assert(inst);

  llvm::GetElementPtrInst *gi = llvm::cast<llvm::GetElementPtrInst>(inst);
  u32 node_id = as->nodes->find_value_node(gi);

  llvm::Value *s = gi->getOperand(0);
  if (llvm::isa<llvm::ConstantPointerNull>(s)) {
    if (gi->getNumOperands() == 2) {
      process_int2ptr(as, inst);
    }
    return;
  }

  u32 sub_node_id = find_value_node_const_ptr(as, s);
  assert(sub_node_id && "non-null GEP operand has no node");

  u32 off = gep_off(as, gi);
  as->constraints->add(ConstraintGEP, node_id, sub_node_id, off);
}

static void process_bitcast(AnalysisSet *as,
                            llvm::Instruction *inst) {

  assert(inst);
  
  llvm::BitCastInst *bi = llvm::cast<llvm::BitCastInst>(inst);
  u32 node_id = as->nodes->find_value_node(bi);

  llvm::Value *op = bi->getOperand(0);
  assert(is_pointer(op));

  u32 sub_node_id = find_value_node_const_ptr(as, op);
  if (sub_node_id)
    as->constraints->add(ConstraintCopy, node_id, sub_node_id);
}

static void process_phi(AnalysisSet *as,
                        llvm::Instruction *inst) {

  assert(inst);
  
  llvm::PHINode *pn = llvm::cast<llvm::PHINode>(inst);
  u32 node_id = as->nodes->find_value_node(pn);

  for (u32 i = 0; i < pn->getNumIncomingValues(); i++) {
    llvm::Value *v = pn->getIncomingValue(i);
    u32 sub_node_id = find_value_node_const_ptr(as, v);
    if (sub_node_id)
      as->constraints->add(ConstraintCopy, node_id, sub_node_id);
  }
}

static void process_select(AnalysisSet *as,
                           llvm::Instruction *inst) {

  assert(inst);
  
  llvm::SelectInst *si = llvm::cast<llvm::SelectInst>(inst);
  u32 node_id = as->nodes->find_value_node(si);

  llvm::Value *op1 = si->getOperand(1);
  u32 sub_node_id1 = find_value_node_const_ptr(as, op1);

  if (sub_node_id1)
    as->constraints->add(ConstraintCopy, node_id, sub_node_id1);
  
  llvm::Value *op2 = si->getOperand(2);
  u32 sub_node_id2 = find_value_node_const_ptr(as, op2);

  if (sub_node_id2)
    as->constraints->add(ConstraintCopy, node_id, sub_node_id2);
}

static void process_vararg(AnalysisSet *as,
                           llvm::Instruction *inst) {

  assert(inst);

  llvm::VAArgInst *vi = llvm::cast<llvm::VAArgInst>(inst);
  u32 node_id = as->nodes->find_value_node(vi);

  llvm::Function *f = inst->getParent()->getParent();
  u32 vararg_id = as->nodes->find_vararg_node(f);
  assert(vararg_id && "va_list args not handled yet");

  as->constraints->add(ConstraintCopy, node_id, vararg_id);
}

static llvm::Function* calledFunction(llvm::CallInst *ci) {
  if (llvm::Function *f = ci->getCalledFunction()) {
    return f;
  }

  llvm::Value *v = ci->getCalledValue();
  if (llvm::ConstantExpr *c = get_const_expr(v)) {
    if (c->getOpcode() == llvm::Instruction::BitCast) {
      if (llvm::Function *f = llvm::dyn_cast<llvm::Function>(c->getOperand(0))) {
        return f;
      }
    }
  }

  return 0;
}

static void add_call_edges(AnalysisSet *as, BlockState *bs,
                           llvm::Instruction *inst) {

  ConstraintGraph *constraint_graph = as->cgraph;
  ConstraintGraphMetadata *meta = as->cgraph->meta;
  llvm::CallInst *ci = llvm::cast<llvm::CallInst>(inst);
  
  // Handle direct calls.
  llvm::Function *f = calledFunction(ci);
  if (f) {
    // Process non-external calls first.
    if (!as->ext_info.is_ext(f)) {
      // Has a call.
      bs->contains_call = true;
      meta->func_callsites[bs->position].push_back(f);

      assert(!meta->callsite_succ.count(bs->position));
      u32 node_id = constraint_graph->create_node(PNODE);
      meta->callsite_succ[bs->position] = node_id;

      constraint_graph->add_edge(bs->position, node_id);
      bs->position = node_id;

      return;
    }

    // Count how many store constraints were added.
    u32 num_stores = 0;
    for (u32 i = bs->constraints_sz; i < bs->constraints_sz; i++) {
      Constraint *c = as->constraints->find(i);
      if (c->type == ConstraintStore) {
        num_stores++;
      }
    }

    // Process multiple stores. This can be caused by memcpy/memmove.
    // Create a diamond-shape.
    if (num_stores > 1) {
      // Bottom of the diamond.
      u32 bottom = constraint_graph->create_node(PNODE);

      for (u32 i = 0; i < num_stores * 2; i += 2) {
        u32 i0 = bs->constraints_sz + i;
        assert(as->constraints->find(i0)->type == ConstraintLoad);

        u32 i1 = bs->constraints_sz + i + 1;
        assert(as->constraints->find(i1)->type == ConstraintStore);

        // Create a non-preserving node.
        u32 node_id = constraint_graph->create_node(MNODE);
        constraint_graph->uses_relevant_def[node_id] = true;

        constraint_graph->add_edge(bs->position, node_id);
        constraint_graph->add_edge(node_id, bottom);

        constraint_graph->uses[i0] = node_id;
        constraint_graph->defs[i1] = node_id;
      }

      bs->position = bottom;
      return;
    }

    // Process single stores.
    if (num_stores == 1) {
      SEGNode *node = constraint_graph->get_node(bs->position);
      if (node->type == PNODE) {
        // If there's a store, it can longer preserve state.
        node->type = MNODE;
      } else {
        // If it's already non-preserving, add a new mnode
        // and connect it to the graph.
        u32 node_id = constraint_graph->create_node(MNODE);
        constraint_graph->add_edge(bs->position, node_id);
        bs->position = node_id;
      }
    }
    
    // Go through the new constraints.
    for (u32 i = bs->constraints_sz; i < as->constraints->size(); i++) {
      Constraint *c = as->constraints->find(i);

      if (c->type == ConstraintStore) {
        constraint_graph->defs[i] = bs->position;
      }
      
      if (c->type == ConstraintLoad) {
        constraint_graph->uses_relevant_def[bs->position] = true;
        constraint_graph->uses[i] = bs->position;
      }
    }

    return;
  }

  // Handle indirect calls.
  if (llvm::isa<llvm::InlineAsm>(ci->getCalledValue())) {
    // Skip inline assembly.
    return;
  }

  // Is there no value at the called value?
  u32 fp = as->nodes->find_value_node(ci->getCalledValue(), true);
  if (!fp) {
    return;
  }

  // Has a call.
  bs->contains_call = true;

  // Go through the new constraints and save them to process later.
  for (u32 i = bs->constraints_sz; i < as->constraints->size(); i++) {
    meta->indirect_call_cons.push_back(i);
  }
  
  // Also save the indirect call inst and current node so
  // we can add the interprocedural control-flow edges later,
  // as well as process indirect external calls.
  std::pair<llvm::CallInst *, u32> call_pair =
    std::make_pair(ci, bs->position);
  meta->indirect_call_pairs.push_back(call_pair);

  // Ensure the call inst has an associated object node.
  assert(!as->nodes->find_value_node(ci, true) ||
         as->nodes->find_object_node(ci));
  
  assert(!meta->callsite_succ.count(bs->position));
  u32 node_id = constraint_graph->create_node(PNODE);

  meta->callsite_succ[bs->position] = node_id;
  constraint_graph->add_edge(bs->position, node_id);

  bs->position = node_id;
  return;
}

static void add_load_edges(AnalysisSet *as, BlockState *bs,
                           llvm::Instruction *inst) {

  ConstraintGraph *constraint_graph = as->cgraph;
  constraint_graph->uses_relevant_def[bs->position] = true;
    
  for (u32 i = bs->constraints_sz; i < as->constraints->size(); i++) {
    Constraint *c = as->constraints->find(i);
    if (c->type == ConstraintLoad) {
      constraint_graph->uses[i] = bs->position;
      break;
    }
  }
}

static void add_store_edges(AnalysisSet *as, BlockState *bs,
                            llvm::Instruction *inst) {

  ConstraintGraph *constraint_graph = as->cgraph;
  SEGNode *node = constraint_graph->get_node(bs->position);
  if (node->type == PNODE) {
    node->type = MNODE;
  } else {
    u32 node_id = constraint_graph->create_node(MNODE);
    constraint_graph->add_edge(bs->position, node_id);
    bs->position = node_id;
  }

  // There may have been multiple constraints added, but there
  // will be exactly one store constraint
  for (u32 i = bs->constraints_sz; i < as->constraints->size(); i++) {
    Constraint *c = as->constraints->find(i);
    if (c->type == ConstraintStore) {
      constraint_graph->defs[i] = bs->position;
      break;
    }
  }  
}

enum InstType {
  INST_NONE = 0,
  INST_LOAD,
  INST_STORE,
  INST_CALL,
};

static void init_instruction(AnalysisSet *as, BlockState *bs,
                             llvm::Instruction *inst) {

  ConstraintGraph *constraint_graph = as->cgraph;  
  bool ptr = is_pointer(inst->getType());

  bs->constraints_sz = as->constraints->size();

  InstType type = INST_NONE;
  switch (inst->getOpcode()) {
  case llvm::Instruction::Load:
    if (ptr) {
      type = INST_LOAD;
      process_load(as, inst);
    }
    break;
    
  case llvm::Instruction::Store:
    assert(!ptr);
    type = INST_STORE;
    process_store(as, inst);
    break;
    
  case llvm::Instruction::Invoke:
  case llvm::Instruction::Call:
    type = INST_CALL;
    process_call(as, inst);
    break;
    
  case llvm::Instruction::Ret:
    assert(!ptr);
    process_return(as, bs, inst);
    break;
    
  case llvm::Instruction::Alloca:
    assert(ptr);
    process_alloc(as, inst);
    break;

  case llvm::Instruction::GetElementPtr:
    assert(ptr);
    process_gep(as, inst);
    break;
      
  case llvm::Instruction::IntToPtr:
    assert(ptr);
    process_int2ptr(as, inst);
    break;
    
  case llvm::Instruction::BitCast:
    if (ptr) {
      process_bitcast(as, inst);
    }
    break;
      
  case llvm::Instruction::PHI:
    if (ptr) {
      process_phi(as, inst);
    }
    break;
      
  case llvm::Instruction::Select:
    if (ptr) {
      process_select(as, inst);
    }
    break;
      
  case llvm::Instruction::VAArg:
    if (ptr) {
      process_vararg(as, inst);
    }
    break;
  }

  int diff = as->constraints->size() - bs->constraints_sz;
  if (!diff && type != INST_CALL) {
    return;
  }
  constraint_graph->defs.insert(constraint_graph->defs.end(), diff, 0);
  constraint_graph->uses.insert(constraint_graph->uses.end(), diff, 0);  
  
  switch (type) {
  case INST_CALL:
    add_call_edges(as, bs, inst);
    break;

  case INST_LOAD:
    add_load_edges(as, bs, inst);
    break;
    
  case INST_STORE:
    add_store_edges(as, bs, inst);
    break;
    
  default:
    assert(!ptr && "unknown instruction is pointer");
  }

  u32 sz = as->constraints->size();
  assert(constraint_graph->defs.size() == sz);
  assert(constraint_graph->uses.size() == sz);
}

static void init_blocks(AnalysisSet *as,
                        llvm::BasicBlock *bb,
                        u32 parent) {
  
  ConstraintGraph *constraint_graph = as->cgraph;
  ConstraintGraphMetadata *meta = as->cgraph->meta;

  // Process blocks we've seen before.
  ConstraintGraphMetadata::bb_iterator bb_it = meta->bb_start.find(bb);
  if (bb_it != meta->bb_start.end()) {
    u32 index = bb_it->second;
    assert(parent != MAX_U32);
    constraint_graph->add_edge(parent, index);

    return;
  }

  u32 index = constraint_graph->create_node(PNODE);
  meta->bb_start[bb] = index;

  if (parent == MAX_U32) {
    llvm::Function *f = bb->getParent();
    assert(!meta->func_start_nodes.count(f));
    meta->func_start_nodes[f] = index;
  } else {
    constraint_graph->add_edge(parent, index);
  }

  BlockState bs(bb, index);
  for (llvm::BasicBlock::iterator i = bb->begin(), e = bb->end();
       i != e; i++) {
    llvm::Instruction *inst = &*i;
    init_instruction(as, &bs, inst);
  }

  for (llvm::succ_iterator i = llvm::succ_begin(bb),
         e = llvm::succ_end(bb); i != e; i++) {
    init_blocks(as, *i, bs.position);
  }   
}

static void init_function_contents(llvm::Module *m, AnalysisSet *as) {
  assert(as->cgraph);
  
  BlockSet block_cache;  
  for (llvm::Module::iterator i = m->begin(), e = m->end();
       i != e; i++) {
    llvm::Function *f = i;
    init_pointer_instruction_nodes(as, f);

    init_blocks(as, &f->getEntryBlock(), MAX_U32);
  }
}

void AnalysisSet::init(llvm::Module *m) {
  nodes = new Nodes();
  constraints = new Constraints();
  
  // Create 3 placeholder nodes for each of the SpecialNodes,
  // excluding the NodeFirst. These are used to describe exceptional
  // states, like when we can't know the target of a pointer.
  for (u32 i = 0; i < NodeFirst; i++) {
    u32 obj_sz = 0;
    if (i == NodeUnknownTarget) {
      // Treat the unknown target as an object whose address has
      // been taken.
      obj_sz = 1;
      constraints->add(ConstraintAddrOf,
                      NodeConstToUnknownTarget,
                      NodeUnknownTarget);
    }
    nodes->add_unreachable(nullptr, obj_sz);
  }

  llvm::raw_ostream &os = llvm::outs();

  os << "Step 1" << "\n";
  os << "===========================================================" << "\n";
  nodes->print();
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
  // much nicer to compute object nodes for all of the top-level
  // declarations (functions and global variables) first, because
  // then we don't risk creating unreachable objects.
  //
  // To do this, we can also strongly assert that all initialized
  // global variables have values that are either free-standing
  // constants or pointers to things that are already cached.
  init_function_signatures(m, this);

  os << "Step 2" << "\n";
  os << "===========================================================" << "\n";
  nodes->print();
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
  nodes->print();
  print_named_constraints(this);
  os << "\n";

  // Process global variable values.
  //
  // This also handles GEP expressions that are *part* of global
  // variable values. If a GEP expression isn't part of a global
  // variable value but still *references* a global, it is handled
  // later as the module's blocks are walked.
  init_global_variable_values(m, this);

  // N.B.
  //
  // What happens if the program is totally global? Meaning, what if
  // there are runtime values assigned to global variables? How is
  // that handled? Maybe that's a special case, but I don't see how
  // that is handled here.  

  os << "Step 4" << "\n";
  os << "===========================================================" << "\n";
  nodes->print();
  print_named_constraints(this);
  structs.print();

  cgraph = new ConstraintGraph(constraints->size(), 1000000000);

  // Process the instructions inside functions.
  //
  init_function_contents(m, this);
}

// TODO:
//
// Finish adding comments.
// X Fill in the trivial node lookup code.
// X Go through and make sure all little helper functions are defined.
// X Convert function code to simpler add_value and add_object helpers.
