//===- predicates.h - -----------------------------------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#ifndef PREDICATES_H
#define PREDICATES_H

#include "llvm/Instructions.h"     // for llvm::CallInst, llvm::InvokeInst,
                                   //     llvm::CmpInst, etc.
#include "llvm/Type.h"             // for llvm::Type
#include "llvm/Value.h"            // for llvm::Value

bool is_inline_asm(llvm::Value *v) {
  return llvm::isa<llvm::InlineAsm>(v);
}

bool is_pointer(llvm::Value *v) {
  return llvm::isa<llvm::PointerType>(v->getType());    
}

bool is_pointer(const llvm::Type *t) {
  return llvm::isa<llvm::PointerType>(t);
}

bool is_const_null_ptr(llvm::Value *v) {
  return llvm::isa<llvm::ConstantPointerNull>(v);
}

bool is_gep(llvm::ConstantExpr *expr) {
  return expr && expr->getOpcode() == llvm::Instruction::GetElementPtr;
}

bool is_const(llvm::Value *v) {
  return llvm::isa<llvm::Constant>(v);
}

bool is_global_value(llvm::Value *v) {
  return llvm::isa<llvm::GlobalValue>(v);
}

const llvm::ConstantInt *const_int(llvm::Value *v) {
  return llvm::dyn_cast<llvm::ConstantInt>(v);
}

llvm::IntToPtrInst *get_int_to_ptr(llvm::Value *v) {
  return llvm::dyn_cast<llvm::IntToPtrInst>(v);
}

llvm::GetElementPtrInst *get_gep(llvm::Value *v) {
  return llvm::dyn_cast<llvm::GetElementPtrInst>(v);
}

llvm::ConstantExpr *get_const_expr(llvm::Value *v) {
  return llvm::dyn_cast<llvm::ConstantExpr>(v);
}

llvm::Instruction *get_inst(llvm::Value *v) {
  return llvm::dyn_cast<llvm::Instruction>(v);
}

llvm::GlobalVariable *get_global(llvm::Value *v) {
  return llvm::dyn_cast<llvm::GlobalVariable>(v);
}

llvm::BitCastInst *get_bitcast(llvm::Value *v) {
  return llvm::dyn_cast<llvm::BitCastInst>(v);
}

llvm::User *get_user(llvm::Value *v) {
  return llvm::dyn_cast<llvm::User>(v);
}

const llvm::StructType *struct_type(const llvm::Type *typ) {
  return llvm::dyn_cast<llvm::StructType>(typ);
}

#endif // end PREDICATES_H
