//===- instructions.cpp ----------------------------------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#include "analysis_set.h"
#include "block_state.h"
#include "function_state.h"
#include "predicates.h"

#include "llvm/Instructions.h"     // for llvm::CallInst, llvm::InvokeInst,
                                   //     llvm::CmpInst, etc.

#include "llvm/Value.h"            // for llvm::Value

static void process_int2ptr(FunctionState *fs, llvm::Value *v);
static u32 process_gep(FunctionState *fs, llvm::ConstantExpr *expr);

static u32 find_value_node_const_ptr(FunctionState *fs, llvm::Value *v) {
  
  assert(v);

  llvm::outs() << "      value: ";
  llvm::WriteAsOperand(llvm::outs(), v, false);
  llvm::outs() << "\n";
  
  u32 node_id = fs->nodes->find_value_node(v, true);
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
    return find_value_node_const_ptr(fs, e->getOperand(0));
      
  case llvm::Instruction::IntToPtr:
    process_int2ptr(as, e);
    return fs->nodes->find_value_node(e);

  case llvm::Instruction::GetElementPtr:
    process_gep(as, e);
    return fs->nodes->find_value_node(e);

  default:
    assert(false && "unknown opcode in const pointer expression");
  }

  return 0;
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

static void add_load_constraint(FunctionState *fs,
                                BlockState *bs,
                                u32 dest, u32 src) {
  
  u32 cons_index = fs->constraints->size();
  fs->constraints->add(ConstraintLoad, dest, src);

  fs->constraint_graph->uses.push_back(bs->position);
  assert(fs->constraint_graph->uses[cons_index] == bs->position);

  fs->constraint_graph->uses_relevant_def[bs->position] = true;
}

static void process_load(FunctionState *fs,
                         BlockState *bs,
                         llvm::Instruction *inst) {  
  assert(inst);
  
  llvm::LoadInst *li = llvm::cast<llvm::LoadInst>(inst);

  u32 node_id = fs->nodes->find_value_node(li);
  u32 src_node_id = find_value_node_const_ptr(fs, li->getPointerOperand());
  if (!src_node_id) {
    return;
  }

  add_load_constraint(fs, bs, node_id, src_node_id);
}

static void add_store_constraint(FunctionState *fs,
                                 BlockState *bs,
                                 u32 dest, u32 src) {

  u32 cons_index = fs->constraints->size();
  fs->constraints->add(ConstraintStore, dest, src);

  SEGNode *node = fs->constraint_graph->get_node(bs->position);
  if (node->type == PNODE) {
    node->type = MNODE;
  } else {
    u32 node_id = fs->constraint_graph->create_node(MNODE);
    fs->constraint_graph->add_edge(bs->position, node_id);
    bs->position = node_id;
  }
  
  fs->constraint_graph->defs.push_back(bs->position);
  assert(fs->constraint_graph->defs[cons_index] == bs->position);
}

static void process_store(FunctionState *fs,
                          BlockState *bs,
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

  u32 sub_dest_id = find_value_node_const_ptr(fs, dest);
  u32 sub_src_id = find_value_node_const_ptr(fs, src);

  if (sub_src_id && sub_dest_id)
    add_store_constraint(fs, bs, sub_dest_id, sub_src_id);    
}

static void process_return(FunctionState *fs,
                           BlockState *bs,
                           llvm::Instruction *inst) {
  assert(inst);
  
  llvm::ReturnInst *ri = llvm::cast<llvm::ReturnInst>(inst);

  llvm::Value *ret_value = ri->getReturnValue();
  if (!ret_value || !is_pointer(ret_value))
    return;

  llvm::Function *f = ri->getParent()->getParent();
  u32 ret_node_id = fs->nodes->find_ret_node(f);
  assert(ret_node_id);

  u32 src_node_id = find_value_node_const_ptr(fs, ret_value);
  if (src_node_id)
    fs->constraints->add(ConstraintCopy, ret_node_id, src_node_id);

  if (bs->contains_call) {
    u32 node_id = fs->constraint_graph->create_node(PNODE);
    fs->constraint_graph->add_edge(bs->position, node_id);
    bs->position = node_id;
  }

  ConstraintGraphMetadata *meta = fs->constraint_graph->meta;  
  assert(!meta->func_ret_nodes.count(bs->block->getParent()));
  meta->func_ret_nodes[bs->block->getParent()] = bs->position;
}

static void process_alloc(FunctionState *fs,
                          BlockState *bs,
                          llvm::Instruction *inst) {
  assert(inst);
  
  llvm::AllocaInst *ai = llvm::cast<llvm::AllocaInst>(inst);
  u32 node_id = fs->nodes->find_value_node(ai);

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
    const std::vector<u32> &sz= fs->global->structs.get_sz(st);
    
    for (u32 i = 0; i < sz.size(); i++) {
      u32 _obj_id = fs->nodes->add_object(ai, sz[i], weak);         
      if (obj_id != NodeNone)
        obj_id = _obj_id;
    }
  }

  if (obj_id == NodeNone)
    obj_id = fs->nodes->add_object(inst, 1, weak);

  fs->constraints->add(ConstraintAddrOf, node_id, obj_id);
}

u32 gep_off(AnalysisSet *as, llvm::User *u) {

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

static void process_gep(FunctionState *fs,
                        BlockState *bs,
                        llvm::Instruction *inst) {
  assert(inst);

  llvm::GetElementPtrInst *gi = llvm::cast<llvm::GetElementPtrInst>(inst);
  u32 node_id = fs->nodes->find_value_node(gi);

  llvm::Value *s = gi->getPointerOperand();  
  if (llvm::isa<llvm::ConstantPointerNull>(s)) {
    if (gi->getNumOperands() == 2) {
      process_int2ptr(fs, bs, inst);
    }
    return;
  }

  u32 sub_node_id = find_value_node_const_ptr(fs, s);
  assert(sub_node_id && "non-null GEP operand has no node");

  u32 off = gep_off(fs->global, gi);
  fs->constraints->add(ConstraintGEP, node_id, sub_node_id, off);
}

void process_int2ptr(FunctionState *fs,
                     llvm::Value *v) {
  assert(v);

  u32 dest_id = 0;
  llvm::Value *op = 0;

  if (llvm::IntToPtrInst *ii = get_int_to_ptr(v)) {
    dest_id = fs->nodes->find_value_node(ii);
    op = ii->getOperand(0);    
  } else if (llvm::GetElementPtrInst *gi = get_gep(v)) {
    assert(is_const_null_ptr(gi->getOperand(0)) &&
           gi->getNumOperands() == 2 &&
           "only single-index GEP of null is used for i20");
    dest_id = fs->nodes->find_value_node(gi);
    op = ii->getOperand(1);
  } else if (llvm::ConstantExpr *expr = get_const_expr(v)) {
    assert(!fs->nodes->contains_value(expr));
    fs->nodes->add_value(expr);
    
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
    u32 sub_node_id = find_value_node_const_ptr(fs, s);
    if (sub_node_id)
      fs->constraints->add(ConstraintCopy, dest_id, sub_node_id);
  }

  if (has_i2p) {
    fs->constraints->add(ConstraintAddrOf, dest_id, NodeUnknownTarget);
  }
}

static void process_int2ptr(FunctionState *fs,
                            BlockState *bs,
                            llvm::Instruction *inst) {
  process_int2ptr(fs, inst);
}

static void process_bitcast(FunctionState *fs,
                            BlockState *bs,
                            llvm::Instruction *inst) {  
  assert(inst);
  
  llvm::BitCastInst *bi = llvm::cast<llvm::BitCastInst>(inst);
  u32 node_id = fs->nodes->find_value_node(bi);

  llvm::Value *src = bi->getOperand(0);
  assert(is_pointer(src));

  u32 sub_node_id = find_value_node_const_ptr(fs, src);
  if (sub_node_id)
    as->constraints->add(ConstraintCopy, node_id, sub_node_id);
}

static void process_phi(FunctionState *fs,
                        BlockState *bs,
                        llvm::Instruction *inst) {
  assert(inst);
  
  llvm::PHINode *pn = llvm::cast<llvm::PHINode>(inst);
  u32 node_id = fs->nodes->find_value_node(pn);

  for (u32 i = 0; i < pn->getNumIncomingValues(); i++) {
    llvm::Value *v = pn->getIncomingValue(i);
    u32 sub_node_id = find_value_node_const_ptr(fs, v);
    if (sub_node_id)
      fs->constraints->add(ConstraintCopy, node_id, sub_node_id);
  }
}

static void process_select(FunctionState *fs,
                           BlockState *bs,
                           llvm::Instruction *inst) {
  assert(inst);
  
  llvm::SelectInst *si = llvm::cast<llvm::SelectInst>(inst);
  u32 node_id = fs->nodes->find_value_node(si);

  llvm::Value *true_value = si->getOperand(1);
  u32 true_node_id = find_value_node_const_ptr(fs, true_value);

  if (true_node_id)
    fs->constraints->add(ConstraintCopy, node_id, true_node_id);
  
  llvm::Value *false_value = si->getOperand(2);
  u32 false_node_id = find_value_node_const_ptr(fs, false_value);

  if (false_node_id)
    fs->constraints->add(ConstraintCopy, node_id, false_node_id);
}

static void process_vararg(FunctionState *fs,
                           BlockState *bs,
                           llvm::Instruction *inst) {
  assert(inst);

  llvm::VAArgInst *vi = llvm::cast<llvm::VAArgInst>(inst);
  u32 node_id = fs->nodes->find_value_node(vi);

  llvm::Function *f = inst->getParent()->getParent();
  u32 vararg_id = fs->nodes->find_vararg_node(f);
  assert(vararg_id && "va_list args not handled yet");

  fs->constraints->add(ConstraintCopy, node_id, vararg_id);
}

static bool ext_no_struct_alloc(FunctionState *fs, llvm::Function *f) {
  return fs->global->ext_info.no_struct_alloc(f);
}

static void process_no_struct_callee(FunctionState *fs,
                                     u32 node_id) {
  
  llvm::Value *v = fs->nodes->find_node(node_id)->val;
  u32 obj_id = fs->nodes->add_object(v, 1, true);
  fs->constraints->add(ConstraintAddrOf, node_id, obj_id);
}

static bool ext_alloc(FunctionState *fs, llvm::Function *f) {
  return fs->global->ext_info.is_alloc(f);
}

static bool ext_realloc(FunctionState *fs, llvm::Function *f) {
  return fs->global->ext_info.get_type(f) == EFT_REALLOC;
}
    
static void process_alloc_callee(FunctionState *fs,
                                 llvm::CallSite &cs,
                                 u32 node_id) {

  llvm::Instruction *inst = cs.getInstruction();
  assert(isnt);
  const llvm::Type *t = trace_alloc_type(as, inst);

  u32 obj_id = NodeNone;
  if (const llvm::StructType *st = llvm::dyn_cast<llvm::StructType>(t)) {
    const std::vector<u32> &sz= fs->global->structs.get_sz(st);

    for (u32 i = 0; i < sz.size(); i++) {
      u32 _obj_id = fs->nodes->add_object(inst, sz[i], true);         
      if (obj_id != NodeNone)
        obj_id = _obj_id;
    }
  }

  if (obj_id == NodeNone)
    obj_id = fs->nodes->add_object(inst, 1, true);

  if (f)
    fs->constraints->add(ConstraintAddrOf, node_id, obj_id);
}

static void process_static_callee(FunctionState *fs,
                                  llvm::Function *f,
                                  llvm::CallSite &cs,
                                  u32 node_id) {
                                      
  std::string func_name = f->getNameStr();    
  std::map<std::string, u32>::const_iterator i_srn =
    fs->global->static_returns.find(func_name);

  u32 obj_id = NodeNone;
  if (i_srn != fs->global->static_returns.end()) {
    obj_id = i_srn->second;
    fs->constraints->add(ConstraintAddrOf, node_id, obj_id);
    return;
  }

  llvm::Instruction *inst = cs.getInstruction();

  if (fs->global->ext_info.has_static2(f)) {
    obj_id = fs->nodes->add_object(inst, 2, true);
    fs->nodes->add_object(inst, 1, true);
    fs->constraints->add(ConstraintAddrOf, node_id, obj_id);
  } else {
    const llvm::Type *t = inst->getType()->getContainedType(0);
    if (const llvm::StructType *st = llvm::dyn_cast<llvm::StructType>(t)) {
      const std::vector<u32> &sz= fs->global->structs.get_sz(st);
        
      for (u32 i = 0; i < sz.size(); i++) {
        u32 _obj_id = fs->nodes->add_object(inst, sz[i], true);         
        if (obj_id != NodeNone)
          obj_id = _obj_id;
      }
    }
    if (obj_id == NodeNone)
      obj_id = fs->nodes->add_object(inst, 1, true);
  }
    
  fs->global->static_returns[func_name] = obj_id;
  fs->constraints->add(ConstraintAddrOf, node_id, obj_id);
}
  
static void process_callee(FunctionState *fs,
                           llvm::Function *f,
                           llvm::CallSite &cs,
                           u32 node_id) {

  if (f && ext_no_struct_alloc(fs, f)) {
    return process_no_struct_callee(fs, node_id);
  }

  if (!f || ext_alloc(fs, f) ||
      (ext_realloc(fs, f) && is_const_null_ptr(cs.getArgument(0)))) {
    return process_alloc_callee(fs, cs, node_id);
  }

  if (as->ext_info.has_static(f)) {
    return process_static_callee(fs, f, cs, node_id);
  }
}

static void __process_direct_call(AnalysisSet *as,
                                  llvm::CallSite &cs,
                                  llvm::Function *f) {
  assert(f);

  if (is_pointer(cs.getType())) {
    u32 node_id = fs->nodes->find_value_node(cs.getInstruction());
    if (!is_pointer(f->getReturnType())) {
      fs->constraints->add(ConstraintAddrOf, node_id, NodeUnknownTarget);
    } else {
      u32 ret_node_id = fs->nodes->find_ret_node(f);
      assert(ret_node_id);
      fs->constraints->add(ConstraintCopy, node_id, ret_node_id);
    }
  }

  llvm::CallSite::arg_iterator arg_it = cs.arg_begin(),
    arg_end = cs.arg_end();
  
  llvm::Function::arg_iterator func_it= f->arg_begin(),
    func_end= f->arg_end();
  
  for(; func_it != func_end && arg_it != arg_end;
      ++arg_it, ++func_it) {

    llvm::Value *aa = *arg_it, *fa = func_it;
    if (!is_pointer(fa->getType()))
      continue;

    u32 fa_node_id = fs->nodes->find_value_node(fa);
    if (!is_pointer(aa)) {
      fs->constraints->add(ConstraintAddrOf, fa_node_id,
                           NodeUnknownTarget);
    } else {
      u32 aa_node_id = fs->nodes->find_value_node(aa);
      if (aa_node_id)
        fs->constraints->add(ConstraintCopy, fa_node_id, aa_node_id);
    }
  }

  if (!f->isVarArg()) {
    assert(arg_it == arg_end && "too many args to non-vararg func");
    return;
  }

  u32 va_node_id = fs->nodes->find_vararg_node(f);
  assert(va_node_id);

  for (; arg_it != arg_end; arg_it++) {
    llvm::Value *aa = *arg_it;

    if (!is_pointer(aa)) {
      fs->constraints->add(ConstraintAddrOf, va_node_id,
                           NodeUnknownTarget);
    } else {
      u32 aa_node_id = find_value_node_const_ptr(fs, aa);
      if (aa_node_id)
        fs->constraints->add(ConstraintCopy, va_node_id, aa_node_id);
    }
  }
}


static u32 eft_la_num_args(FunctionState *fs, llvm::Function *f) {
  switch(fs->global->ext_info.get_type(f)) {
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

static void process_EFT_L_A(FunctionState *fs,
                            llvm::CallSite &cs,
                            llvm::Function *f) {

  llvm::outs() << "process_EFT_L_A: " << f->getName() << "\n";
  
  llvm::Instruction *inst = cs.getInstruction();
  if (!is_pointer(inst))
    return;

  u32 node_id = fs->nodes->find_value_node(inst);
  u32 i_arg = eft_la_num_args(fs, f);

  llvm::Value *src= cs.getArgument(i_arg);
  if (!is_pointer(src->getType())){
    fs->constraints->add(ConstraintAddrOf, node_id, NodeUnknownTarget);
    return;
  }

  u32 sub_node_id = find_value_node_const_ptr(fs, src);
  if (sub_node_id)
    fs->constraints->add(ConstraintCopy, node_id, sub_node_id);
}

static u32 get_max_offset(FunctionState *fs, llvm::Value *v) {
  
  const llvm::Type *t = v->getType();

  const llvm::IntegerType *min_int =
    llvm::Type::getInt8Ty(llvm::getGlobalContext());
  
  assert(is_pointer(t) && t->getContainedType(0) == min_int);

  if (llvm::ConstantExpr *expr = get_const_expr(v)) {
    t = expr->getOperand(0)->getType();
    return 1;
  }

  if (llvm::BitCastInst *bi = get_bitcast(v)) {
    t = bi->getOperand(0)->getType();
    return 1;
  }

  if (llvm::User *u = get_user(v)) {
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

static void add_load_store_cons(FunctionState *fs,
                                llvm::Value *dest,
                                llvm::Value *src,
                                u32 size = 0) {
  assert(dest && src);
  
  u32 dest_index = find_value_node_const_ptr(fs, dest);
  u32 src_index = find_value_node_const_ptr(fs, src);

  if (!dest_index || !src_index)
    return;      

  if (!size) {
    size = std::min(get_max_offset(fs, dest),
                    get_max_offset(fs, src));
  }
    
  for (u32 i = 0; i < size; i++) {
    u32 tnode_id = fs->nodes->add_unreachable(0);
    fs->constraints->add(ConstraintLoad, tnode_id, src_index, i);
    fs->constraints->add(ConstraintStore, dest_index, tnode_id, i);
  }
}

static void process_EFT_L_A0__A0R_A1R(FunctionState *fs,
                                      llvm::CallSite &cs,
                                      llvm::Function *f) {

  llvm::outs() << "process_EFT_L_A0__A0R_A1R: " << f->getName() << "\n";
  
  llvm::Value *dest = cs.getArgument(0);
  llvm::Value *src = cs.getArgument(1);
  add_load_store_cons(fs, dest, src);
    
  llvm::Instruction *inst = cs.getInstruction();
  if (is_pointer(inst->getType()))
    fs->constraints->add(ConstraintCopy,
                         fs->nodes->find_value_node(inst),
                         fs->nodes->find_value_node(dest));
}

static void process_EFT_A1R_A0R(FunctionState *fs,
                                llvm::CallSite &cs,
                                llvm::Function *f) {
  
  llvm::outs() << "process_EFT_A1R_A0R: " << f->getName() << "\n";
    
  llvm::Value *dest = cs.getArgument(0);
  llvm::Value *src = cs.getArgument(1);
  add_load_store_cons(fs, dest, src);
}

static void process_EFT_A3R_A1R_NS(FunctionState *fs,
                                   llvm::CallSite &cs,
                                   llvm::Function *f) {

  llvm::outs() << "process_EFT_A3R_A1R_NS: " << f->getName() << "\n";
    
  llvm::Value *dest = cs.getArgument(3);
  llvm::Value *src = cs.getArgument(1);
  add_load_store_cons(fs, dest, src, 1);
}

static void process_EFT_A1R_A0(FunctionState *fs,
                               llvm::CallSite &cs,
                               llvm::Function *f) {
  
  llvm::outs() << "process_EFT_A1R_A0: " << f->getName() << "\n";
    
  llvm::Value *dest = cs.getArgument(1);
  u32 dest_index = find_value_node_const_ptr(fs, dest);

  llvm::Value *src = cs.getArgument(0);
  u32 src_index = find_value_node_const_ptr(fs, src);
  
  if (dest_index && src_index)
    fs->constraints->add(ConstraintStore, dest_index, src_index);
}

static void process_EFT_A2R_A1(FunctionState *fs,
                               llvm::CallSite &cs,
                               llvm::Function *f) {
  
  llvm::outs() << "process_EFT_A2R_A1: " << f->getName() << "\n";
    
  llvm::Value *dest = cs.getArgument(2);
  u32 dest_index = find_value_node_const_ptr(fs, dest);

  llvm::Value *src = cs.getArgument(1);
  u32 src_index = find_value_node_const_ptr(fs, src);
  
  if (dest_index && src_index)
    fs->constraints->add(ConstraintStore, dest_index, src_index);
}

static void process_EFT_A4R_A1(FunctionState *fs,
                               llvm::CallSite &cs,
                               llvm::Function *f) {

  llvm::outs() << "process_EFT_A4R_A1: " << f->getName() << "\n";
    
  llvm::Value *dest = cs.getArgument(4);
  u32 dest_index = find_value_node_const_ptr(fs, dest);

  llvm::Value *src = cs.getArgument(1);
  u32 src_index = find_value_node_const_ptr(fs, src);
  
  if (dest_index && src_index)
    fs->constraints->add(ConstraintStore, dest_index, src_index);
}

static void process_EFT_L_A0__A2R_A0(FunctionState *fs,
                                     llvm::CallSite &cs,
                                     llvm::Function *f) {

  llvm::outs() << "process_EFT_A0__A2R_A0: " << f->getName() << "\n";
  
  llvm::Instruction *inst = cs.getInstruction();
  if (is_pointer(inst)) {
    u32 node_id = fs->nodes->find_value_node(inst);
    llvm::Value *src = cs.getArgument(0);

    if (!is_pointer(src->getType())) {
      fs->constraints->add(ConstraintAddrOf, node_id,
                           NodeUnknownTarget);
    } else {
      u32 sub_node_id = find_value_node_const_ptr(fs, src);
      if (sub_node_id)
        fs->constraints->add(ConstraintCopy, node_id, sub_node_id);
    }
  }

  llvm::Value *dest = cs.getArgument(2);
  u32 node_id = find_value_node_const_ptr(fs, dest);

  llvm::Value *src = cs.getArgument(0);
  u32 sub_node_id = find_value_node_const_ptr(fs, src);
  if (node_id && sub_node_id)
    fs->constraints->add(ConstraintStore, node_id, sub_node_id);
}

static u32 eft_a_new_num_args(FunctionState *fs, llvm::Function *f) {
  switch(fs->global->ext_info.get_type(f)) {
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

static void process_EFT_A_NEW(FunctionState *fs,
                              llvm::CallSite &cs,
                              llvm::Function *f) {
  
  llvm::outs() << "process_EFT_A_NEW: " << f->getName() << "\n";
    
  u32 i_arg = eft_a_new_num_args(as, f);

  llvm::Value *dest = cs.getArgument(i_arg);
  u32 dest_node_id = find_value_node_const_ptr(fs, dest);
  if (!dest_node_id)
    return;

  const llvm::Type *t = dest->getType()->getContainedType(0);
  assert(is_pointer(t));

  t = t->getContainedType(0);
  while (const llvm::ArrayType *at = llvm::dyn_cast<llvm::ArrayType>(t))
    t = at->getElementType();

  u32 obj_id = NodeNone;
  if (const llvm::StructType *st = llvm::dyn_cast<llvm::StructType>(t)) {
    const std::vector<u32> &sz= fs->global->structs.get_sz(st);
    //  FIXME: X/0 shouldn't really have a value because it's not
    //  pointed to by any program variable, but for now we require
    //  all obj_nodes to have one.
    for (u32 i = 0; i < sz.size(); i++) {
      u32 _obj_id = fs->nodes->add_object(dest, sz[i], true);
      if (obj_id == NodeNone)
        obj_id = _obj_id;
    }
  } else {
    obj_id = fs->nodes->add_object(dest, 1, true);
  }
  
  assert(obj_id != NodeNone);
  
  u32 node_id = fs->nodes->add_unreachable(0);
  fs->constraints->add(ConstraintAddrOf, node_id, obj_id);
  fs->constraints->add(ConstraintStore, dest_node_id, node_id);
}

static void __process_external_call(FunctionState *fs,
                                    llvm::CallSite &cs,
                                    llvm::Function *f) {

  assert(f && fs-global->>ext_info.is_ext(f));
  
  if (fs->global->ext_info.is_alloc(f) ||
      fs->global->ext_info.has_static(f))
    return;

  ExternalFunctionType tF = fs->global->ext_info.get_type(f);
  switch (tF){
  case EFT_REALLOC:
    if (is_const_null_ptr(cs.getArgument(0)))
      break;

  case EFT_L_A0:
  case EFT_L_A1:
  case EFT_L_A2:
  case EFT_L_A8:
    process_EFT_L_A(fs, cs, f);
    break;
    
  case EFT_L_A0__A0R_A1R:
    process_EFT_L_A0__A0R_A1R(fs, cs, f);
    break;

  case EFT_A1R_A0R:
    process_EFT_A1R_A0R(fs, cs, f);
    break;
    
  case EFT_A3R_A1R_NS:
    process_EFT_A3R_A1R_NS(fs, cs, f);
    break;

  case EFT_A1R_A0:
    process_EFT_A1R_A0(fs, cs, f);
    break;

  case EFT_A2R_A1:
    process_EFT_A2R_A1(fs, cs, f);
    break;

  case EFT_A4R_A1:
    process_EFT_A4R_A1(fs, cs, f);
    break;

  case EFT_L_A0__A2R_A0:
    process_EFT_L_A0__A2R_A0(fs, cs, f);
    break;

  case EFT_A0R_NEW:
  case EFT_A1R_NEW:
  case EFT_A2R_NEW:
  case EFT_A4R_NEW:
  case EFT_A11R_NEW:
    process_EFT_A_NEW(fs, cs, f);
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

static void __process_indirect_call(FunctionState *fs,
                                    llvm::CallSite &cs) {

  llvm::Value *called = cs.getCalledValue();
  assert(called);

  if (is_inline_asm(called))
    return;

  llvm::Instruction *inst = cs.getInstruction();
  u32 called_id = find_value_node_const_ptr(fs, called);
  assert(called_id && "null callee");
  
  ConstraintGraphMetadata *meta = fs->cgraph->meta;
  meta->indirect_call_func_nodes.insert(called_id);

  if (is_pointer(cs.getType())) {
    u32 node_id = fs->nodes->find_value_node(inst);
    fs->constraints->add(ConstraintLoad, node_id, called_id,
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
      fs->constraints->add(ConstraintStore, called_id,
                           NodeConstToUnknownTarget, arg_offset);

      Constraint c(ConstraintStore, called_id,
                   NodeConstToUnknownTarget, arg_offset);
      meta->ret_arg_call_cons[c].insert(inst);
    } else {
      u32 aa_node_id = find_value_node_const_ptr(fs, aa);
      if (aa_node_id) {      
        fs->constraints->add(ConstraintStore, called_id,
                             aa_node_id, arg_offset);

        Constraint c(ConstraintStore, called_id, aa_node_id, arg_offset);
        meta->ret_arg_call_cons[c].insert(inst);
      }
    }
  }  
}

static llvm::Function* calledFunction(llvm::CallSite &cs) {
  llvm::Value *callee = cs.getCalledValue();
  llvm::Function *f = llvm::dyn_cast<llvm::Function>(callee);
  if (f)
    return f;

  if (llvm::ConstantExpr *c = get_const_expr(callee)) {
    if (c->getOpcode() == llvm::Instruction::BitCast) {
      if (llvm::Function *f = llvm::dyn_cast<llvm::Function>(c->getOperand(0))) {
        return f;
      }
    }
  }

  return 0;
}

static void add_direct_call_edges(FunctionState *fs,
                                  BlockState *bs,
                                  llvm::Function *callee) {
  // Has a call.
  bs->contains_call = true;

  ConstraintGraphMetadata *meta = fs->constraint_graph->meta;
  meta->func_callsites[bs->position].push_back(callee);

  assert(!meta->callsite_succ.count(bs->position));
  u32 node_id = constraint_graph->create_node(PNODE);
  meta->callsite_succ[bs->position] = node_id;

  fs->constraint_graph->add_edge(bs->position, node_id);
  bs->position = node_id;
}

static void add_external_call_edges(FunctionState *fs,
                                    BlockState *bs,
                                    u32 cons_start) {

  u32 diff = fs->constraints() - cons_start;  
  fs->constraint_graph->defs.insert(constraint_graph->defs.end(), diff, 0);
  fs->constraint_graph->uses.insert(constraint_graph->uses.end(), diff, 0);  

  // Count how many store constraints were added.
  u32 num_stores = 0;
  for (u32 i = cons_start; i < fs->constraints->size(); i++) {
    Constraint *c = fs->constraints->find(i);
    if (c->type == ConstraintStore) {
      num_stores++;
    }
  }

  // Process multiple stores. This can be caused by memcpy/memmove.
  // Create a diamond-shape.
  if (num_stores > 1) {
    // Bottom of the diamond.
    u32 bottom = fs->constraint_graph->create_node(PNODE);
    
    for (u32 i = 0; i < num_stores * 2; i += 2) {
      u32 i0 = cons_start + i;
      assert(fs->constraints->find(i0)->type == ConstraintLoad);
      
      u32 i1 = cons_start + i + 1;
      assert(fs->constraints->find(i1)->type == ConstraintStore);
      
      // Create a non-preserving node.
      u32 node_id = fs->constraint_graph->create_node(MNODE);
      fs->constraint_graph->uses_relevant_def[node_id] = true;
      
      fs->constraint_graph->add_edge(bs->position, node_id);
      fs->constraint_graph->add_edge(node_id, bottom);
      
      fs->constraint_graph->uses[i0] = node_id;
      fs->constraint_graph->defs[i1] = node_id;
    }

    bs->position = bottom;
    return;
  }

  // Process single stores.
  if (num_stores == 1) {
    SEGNode *node = fs->constraint_graph->get_node(bs->position);
    if (node->type == PNODE) {
      // If there's a store, it can longer preserve state.
      node->type = MNODE;
    } else {
      // If it's already non-preserving, add a new mnode
      // and connect it to the graph.
      u32 node_id = fs->constraint_graph->create_node(MNODE);
      fs->constraint_graph->add_edge(bs->position, node_id);
      bs->position = node_id;
    }
  }
    
  // Go through the new constraints.
  for (u32 i = cons_start; i < fs->constraints->size(); i++) {
    Constraint *c = fs->constraints->find(i);
    
    if (c->type == ConstraintStore) {
      fs->constraint_graph->defs[i] = bs->position;
    }
    
    if (c->type == ConstraintLoad) {
      fs->constraint_graph->uses[i] = bs->position;
      fs->constraint_graph->uses_relevant_def[bs->position] = true;
    }
  }
}

void add_indirect_call_edges(FunctionState *fs,
                             BlockState *bs,
                             llvm::CallSite &cs,
                             u32 cons_start) {
  
  // Skip inline assembly.
  if (is_inline_asm(cs.getCalledValue()))
    return;

  // Is there no value at the called value?
  u32 fp = fs->nodes->find_value_node(cs.getCalledValue(), true);
  if (!fp)
    return;

  // Has a call.
  bs->contains_call = true;

  // Go through the new constraints and save them to process later.
  ConstraintGraphMetadata *meta = fs->constraint_graph->meta;
  for (u32 i = cons_start; i < fs->constraints->size(); i++) {
    meta->indirect_call_cons.push_back(i);
  }
  
  // Also save the indirect call inst and current node so
  // we can add the interprocedural control-flow edges later,
  // as well as process indirect external calls.
  std::pair<llvm::CallSite *, u32> call_pair =
    std::make_pair(new llvm::CallSite(cs.getInstruction(),
                                      bs->position));
  meta->indirect_call_pairs.push_back(call_pair);

  // Ensure the call inst has an associated object node.
  assert(!fs->nodes->find_value_node(cs.getInstruction(), true) ||
         fs->nodes->find_object_node(cs.getInstruction()));
  
  assert(!meta->callsite_succ.count(bs->position));
  u32 node_id = fs->constraint_graph->create_node(PNODE);

  meta->callsite_succ[bs->position] = node_id;
  fs->constraint_graph->add_edge(bs->position, node_id);

  bs->position = node_id;
}

static void process_direct_call(FunctionState *fs,
                                BlockState *bs,
                                llvm::CallSite &cs,
                                llvm::Function *callee) {

  assert(callee && !callee->isDeclaration());
  __process_direct_call(fs, cs, callee);
  add_direct_call_edges(fs, bs, callee);
}

static void process_external_call(FunctionState *fs,
                                  BlockState *bs,
                                  llvm::CallSite &cs,
                                  llvm::Function *callee) {
  
  assert(callee && callee->isDeclaration());
  u32 cons_start = fs->constraints.size();
  __process_external_call(fs, cs, callee);
  add_external_call_edges(fs, bs, cons_start);
}

static void process_indirect_call(FunctionState *fs,
                                  BlockState *bs,
                                  llvm::CallSite &cs,
                                  llvm::Function *callee) {
  assert(!callee);  
  u32 cons_start = fs->constraints.size();
  __process_indirect_call(fs, cs);
  add_indirect_call_edges(fs, bs, cs, cons_start);
}

static void process_call(FunctionState *fs,
                         BlockState *bs,
                         llvm::Instruction *inst) {
  assert(inst);

  llvm::CallSite cs(inst);
  llvm::Function *f = calledFunction(cs);

  u32 node_id = fs->nodes->find_value_node(inst, true);
  if (node_id) {
    process_callee(fs, f, cs, node_id);
  }

  u32 cons_start = fs->constraints.size();

  if (f) {
    if (!f->isDeclaration()) {
      process_direct_call(fs, bs, cs, f);
      return;
    }

    process_external_call(fs, bs, cs, f);
    return;
  }
  
  process_indirect_call(fs, bs, cs, f);
}

static const InstHandlers inst_handlers = {
  {llvm::Instruction::Load, process_load},
  {llvm::Instruction::Store, process_store},
  {llvm::Instruction::Return, process_return},
  {llvm::Instruction::Alloca, process_alloc},
  {llvm::Instruction::GetElementPtr, process_gep},
  {llvm::Instruction::IntToPtr, process_int2ptr},
  {llvm::Instruction::BitCast, process_bitcast},
  {llvm::Instruction::PHI, process_phi},
  {llvm::Instruction::Select, process_select},
  {llvm::Instruction::VAArg, process_vararg},
  {llvm::Instruction::Invoke, process_call},
  {llvm::Instruction::Call, process_call},  
};

const InstHandlers default_instruction_handlers() {
  return inst_handlers;
}
