//===- blocks.cpp ---------------------------------------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#include "analysis_set.h"
#include "constraints.h"
#include "nodes.h"

#include "llvm/Assembly/Writer.h"  // for llvm::WriteAsOperand
#include "llvm/Instructions.h"     // for llvm::CallInst, llvm::InvokeInst,
                                   //     llvm::CmpInst, etc.
#include "llvm/Module.h"           // for llvm::Function, llvm::BasicBlock
#include "llvm/GlobalVariable.h"   // for llvm::Type
#include "llvm/Support/CallSite.h" // for llvm::CallSite
#include "llvm/Support/CFG.h"      // for llvm::succ_begin, llvm::succ_end
#include "llvm/Support/InstIterator.h"
#include "llvm/Type.h"             // for llvm::Type
#include "llvm/Value.h"            // for llvm::Value

#include <map>    // for std::map

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

// Cache of blocks and the SEGNode index that they're
// associated with.
class BlockSet {
public:
  std::map<llvm::BasicBlock *, u32> cache;
  
  u32 lookup(llvm::BasicBlock *block) {
    std::map<llvm::BasicBlock *, u32>::iterator it =
      cache.find(block);
    if (it != cache.end()) {
      assert(it->second);
      return it->second;
    }
    return 0;
  }

  void insert(llvm::BasicBlock *block, u32 index) {
    cache[block] = index;
  }
};

static bool is_gep(llvm::ConstantExpr *expr) {
  return expr && expr->getOpcode() == llvm::Instruction::GetElementPtr;
}

static bool is_const(llvm::Value *v) {
  return llvm::isa<llvm::Constant>(v);
}

static llvm::IntToPtrInst *get_int_to_ptr(llvm::Value *v) {
  return llvm::dyn_cast<llvm::IntToPtrInst>(v);
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

static bool is_global_value(llvm::Value *v) {
  return llvm::isa<llvm::GlobalValue>(v);
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

u32 find_value_node_const_ptr(AnalysisSet *as, llvm::Value *v);

static void process_load(AnalysisSet *as,
                         llvm::Instruction *inst) {
  
  assert(inst);
  llvm::LoadInst *li = llvm::cast<llvm::LoadInst>(inst);

  u32 node_id = as->nodes->find_value_node(li);
  u32 sub_node_id = find_value_node_const_ptr(as, li->getPointerOperand());
  if (!sub_node_id) {
    return;
  }

  as->constraints->add(ConstraintLoad, node_id, sub_node_id);
}

static bool is_pointer(llvm::Value *v) {
  return llvm::isa<llvm::PointerType>(v->getType());    
}

static bool is_pointer(const llvm::Type *t) {
  return llvm::isa<llvm::PointerType>(t);
}

static bool is_const_null_ptr(llvm::Value *v) {
  return llvm::isa<llvm::ConstantPointerNull>(v);
}

static void process_store(AnalysisSet *as,
                          llvm::Instruction *inst) {

  assert(inst);
  llvm::StoreInst *si = llvm::cast<llvm::StoreInst>(inst);

  llvm::Value *src = si->getValueOperand();
  llvm::outs() << "      src is_pointer: " << is_pointer(src)
               << " is_const: " << is_const(src) << "\n";

  llvm::Value *dest = si->getPointerOperand();
  llvm::outs() << "      dest is_pointer: " << is_pointer(dest)
               << " is_const: " << is_const(dest) << "\n";
  
  if (!is_pointer(src))
    return;

  //as->nodes->print(6);
  
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
    }

    if (obj_id == NodeNone)
      obj_id = as->nodes->add_object(inst, 1, true);

    if (f) {
      as->constraints->add(ConstraintAddrOf, node_id, obj_id);
    }
    
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
      }
      if (obj_id == NodeNone)
        obj_id = as->nodes->add_object(inst, 1, true);
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
    llvm::outs() << "process_EFT_L_A: " << f->getName() << "\n";
    process_EFT_L_A(as, cs, f);
    break;
    
  case EFT_L_A0__A0R_A1R:
    llvm::outs() << "process_EFT_L_A0__A0R_A1R: " << f->getName() << "\n";
    process_EFT_L_A0__A0R_A1R(as, cs, f);
    break;

  case EFT_A1R_A0R:
    llvm::outs() << "process_EFT_A1R_A0R: " << f->getName() << "\n";
    process_EFT_A1R_A0R(as, cs, f);
    break;
    
  case EFT_A3R_A1R_NS:
    llvm::outs() << "process_EFT_A3R_A1R_NS: " << f->getName() << "\n";
    process_EFT_A3R_A1R_NS(as, cs, f);
    break;

  case EFT_A1R_A0:
    llvm::outs() << "process_EFT_A1R_A0: " << f->getName() << "\n";
    process_EFT_A1R_A0(as, cs, f);
    break;

  case EFT_A2R_A1:
    llvm::outs() << "process_EFT_A2R_A1: " << f->getName() << "\n";
    process_EFT_A2R_A1(as, cs, f);
    break;

  case EFT_A4R_A1:
    llvm::outs() << "process_EFT_A4R_A1: " << f->getName() << "\n";
    process_EFT_A4R_A1(as, cs, f);
    break;

  case EFT_L_A0__A2R_A0:
    llvm::outs() << "process_EFT_A0__A2R_A0: " << f->getName() << "\n";
    process_EFT_L_A0__A2R_A0(as, cs, f);
    break;

  case EFT_A0R_NEW:
  case EFT_A1R_NEW:
  case EFT_A2R_NEW:
  case EFT_A4R_NEW:
  case EFT_A11R_NEW:
    llvm::outs() << "process_EFT_A_NEW: " << f->getName() << "\n";
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
    llvm::outs() << "EFT skipped: " << f->getName() << "\n";
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

  llvm::Value *callee = cs.getCalledValue();
  llvm::outs() << "      callee: ";
  llvm::WriteAsOperand(llvm::outs(), callee, false);
  
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
    if (f->isDeclaration() && as->ext_info.is_ext(f)) {
      llvm::outs() << " external" << "\n";
      process_external_call(as, cs, f);
    } else {
      llvm::outs() << " direct" << "\n";
      process_direct_call(as, cs, f);
    }
  } else {
    llvm::outs() << " indirect" << "\n";
    process_indirect_call(as, cs, f);
  }
}

static void process_return(AnalysisSet *as, BlockState *bs,
                           llvm::Instruction *inst) {

  assert(inst);
  llvm::ReturnInst *ri = llvm::cast<llvm::ReturnInst>(inst);

  llvm::Value *ret_value = ri->getReturnValue();
  if (!ret_value || !is_pointer(ret_value))
    return;

  llvm::Function *f = ri->getParent()->getParent();
  u32 ret_node_id = as->nodes->find_ret_node(f);
  assert(ret_node_id);

  u32 src_node_id = find_value_node_const_ptr(as, ret_value);
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
  }

  if (obj_id == NodeNone)
    obj_id = as->nodes->add_object(inst, 1, weak);

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

static void process_int2ptr(AnalysisSet *as, llvm::Value *v) {

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

static u32 process_gep(AnalysisSet *as,
                        llvm::ConstantExpr *expr) {

  assert(is_gep(expr));

  llvm::Value *s = expr->getOperand(0);
  if (llvm::isa<llvm::ConstantPointerNull>(s)) {
    if (expr->getNumOperands() == 2) {
      process_int2ptr(as, expr);
      return as->nodes->find_value_node(expr);
    }
    
    return 0;
  }

  // Lookup or add a value node for the const expr.
  u32 node_id = as->nodes->find_value_node(expr, true);
  if (!node_id) {
    node_id = as->nodes->add_value(expr);
  }

  u32 sub_node_id = find_value_node_const_ptr(as, s);
  assert(sub_node_id && "non-null GEP operand has no node");

  as->constraints->add(ConstraintGEP, node_id,
                       sub_node_id, gep_off(as, expr));  

  return node_id;
}

u32 find_value_node_const_ptr(AnalysisSet *as, llvm::Value *v) {
  assert(v);

  llvm::outs() << "      value: ";
  llvm::WriteAsOperand(llvm::outs(), v, false);
  llvm::outs() << "\n";
  
  u32 node_id = as->nodes->find_value_node(v, true);
  if (node_id) {
    return node_id;
  }

  llvm::Constant *c = llvm::dyn_cast<llvm::Constant>(v);
  assert(c && "value without node is not const");  
  assert(is_pointer(c) &&
         "value without node is not a pointer");
  assert(!is_global_value(c) &&
         "global const pointer should have a node");
  
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
    process_gep(as, e);
    return as->nodes->find_value_node(e);

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

  llvm::Value *s = gi->getPointerOperand();  
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

  llvm::Value *src = bi->getOperand(0);
  assert(is_pointer(src));

  u32 sub_node_id = find_value_node_const_ptr(as, src);
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
    if (!f->isDeclaration() && !as->ext_info.is_ext(f)) {
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

static std::string inst_type_str(llvm::Instruction *inst) {
  const llvm::Type *t = inst->getType();
  switch (t->getTypeID()) {
  case llvm::Type::VoidTyID:
    return "void";
  case llvm::Type::FloatTyID:
    return "float";
  case llvm::Type::DoubleTyID:
    return "double";
  case llvm::Type::X86_FP80TyID:
    return "x86 float";
  case llvm::Type::FP128TyID:
    return "128-bit float";
  case llvm::Type::PPC_FP128TyID:
    return "128-bit float";
  case llvm::Type::LabelTyID:
    return "label";
  case llvm::Type::MetadataTyID:
    return "metadata";
  case llvm::Type::X86_MMXTyID:
    return "mmx vectors";
  case llvm::Type::IntegerTyID:
    return "integer";
  case llvm::Type::FunctionTyID:
    return "function";
  case llvm::Type::StructTyID:
    return "struct";
  case llvm::Type::ArrayTyID:
    return "array";
  case llvm::Type::PointerTyID:
    return "pointer";
  case llvm::Type::OpaqueTyID:
    return "opaque";
  case llvm::Type::VectorTyID:
    return "vector";
  default:
    return "unknown";
  }
}

static std::string inst_str(llvm::Instruction *inst) {
  std::string str;
  llvm::raw_string_ostream os(str);
  
  os << "op: " << inst->getOpcodeName(inst->getOpcode())
     << " type: " << inst_type_str(inst);

  return os.str();
}

static void init_instruction(AnalysisSet *as, BlockState *bs,
                             llvm::Instruction *inst) {

  llvm::outs() << "    ** " << inst_str(inst) << "\n";

  ConstraintGraph *constraint_graph = as->cgraph;
  bool ptr = is_pointer(inst->getType());

  bs->constraints_sz = as->constraints->size();

  InstType type = INST_NONE;
  switch (inst->getOpcode()) {
  case llvm::Instruction::Load:
    // Process only *pointer* load instructions.
    if (ptr) {
      type = INST_LOAD;
      process_load(as, inst);
    }
    break;
    
  case llvm::Instruction::Store:
    // Freak out on *pointer* store instructions.
    assert(!ptr);
    type = INST_STORE;
    process_store(as, inst);
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
    
  case llvm::Instruction::Invoke:
  case llvm::Instruction::Call:
    type = INST_CALL;
    process_call(as, inst);
    break;
    
  default:
    assert(!ptr && "unknown instruction is a pointer");
  }

  int diff = as->constraints->size() - bs->constraints_sz;
  if (!diff && type != INST_CALL) {
    return;
  }
  constraint_graph->defs.insert(constraint_graph->defs.end(), diff, 0);
  constraint_graph->uses.insert(constraint_graph->uses.end(), diff, 0);  
  
  switch (type) {
  case INST_LOAD:
    add_load_edges(as, bs, inst);
    break;
    
  case INST_STORE:
    add_store_edges(as, bs, inst);
    break;
    
  case INST_CALL:
    add_call_edges(as, bs, inst);
    break;

  default:
    // Skip INST_NONE
    break;
  }

  u32 sz = as->constraints->size();
  assert(constraint_graph->defs.size() == sz);
  assert(constraint_graph->uses.size() == sz);
}

static void init_blocks(AnalysisSet *as,
                        BlockSet *block_cache,
                        llvm::BasicBlock *bb,
                        u32 parent) {
  
  ConstraintGraph *constraint_graph = as->cgraph;
  ConstraintGraphMetadata *meta = as->cgraph->meta;

  // Deal with processed blocks.
  if (u32 index = block_cache->lookup(bb)) {
    assert(parent != MAX_U32);
    // Just connect the parent block to the processed block,
    // this is likely a new parent. If it's not, add_edge
    // won't count it twice.
    constraint_graph->add_edge(parent, index);
    return;
  }

  // Create a node and mark the block processed.
  u32 index = constraint_graph->create_node(PNODE);
  block_cache->insert(bb, index);

  if (parent == MAX_U32) {
    // Handle the entry block.
    llvm::Function *f = bb->getParent();
    assert(!meta->func_start_nodes.count(f));
    meta->func_start_nodes[f] = index;
  } else {
    // Connect the parent block to the processed block.
    constraint_graph->add_edge(parent, index);
  }

  llvm::outs() << "  ** Processing block "
               << bb->getName() << " **\n";
    
  BlockState bs(bb, index);
  for (llvm::BasicBlock::iterator i = bb->begin(), e = bb->end();
       i != e; i++) {
    // Process each instruction.
    llvm::Instruction *inst = &*i;
    init_instruction(as, &bs, inst);
  }

  for (llvm::succ_iterator i = llvm::succ_begin(bb),
         e = llvm::succ_end(bb); i != e; i++) {
    // Recurse to process succeeding blocks, using index
    // as the next parent.
    init_blocks(as, block_cache, *i, index);
  }   
}

static void init_function_blocks(AnalysisSet *as,
                                 BlockSet *block_cache,
                                 llvm::Function *f) {

  assert(!f->isDeclaration());

  llvm::outs() << "** Processing function "
               << f->getName() << " **\n";
    
  for (llvm::inst_iterator ii = llvm::inst_begin(f),
         ei = llvm::inst_end(f); ii != ei; ii++) {
    llvm::Instruction *inst = &*ii;
    if (is_pointer(inst)) {
      llvm::outs() << "ADDING POINTER INST: " << inst_str(inst) << "\n";
      llvm::WriteAsOperand(llvm::outs(), inst, false);
      llvm::outs() << "\n";
      
      // Add a node for each pointer instruction.
      as->nodes->add_value(inst);
    }
  }

  // Process each block.
  init_blocks(as, block_cache, &f->getEntryBlock(), MAX_U32);
}

void init_function_internals(llvm::Module *m, AnalysisSet *as) {
  assert(as->cgraph);
  
  BlockSet block_cache;  
  for (llvm::Module::iterator i = m->begin(), e = m->end();
       i != e; i++) {
    llvm::Function *f = i;
    if (f->isDeclaration())
      continue;

    init_function_blocks(as, &block_cache, f);
  }
}
