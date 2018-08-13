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
