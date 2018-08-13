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

inline bool is_inline_asm(const llvm::Value *v) {
  return llvm::isa<llvm::InlineAsm>(v);
}

inline bool is_pointer(const llvm::Value *v) {
  return llvm::isa<llvm::PointerType>(v->getType());    
}

inline bool is_pointer(const llvm::Type *t) {
  return llvm::isa<llvm::PointerType>(t);
}

inline bool is_const_null_ptr(const llvm::Value *v) {
  return llvm::isa<llvm::ConstantPointerNull>(v);
}

inline bool is_gep(const llvm::ConstantExpr *expr) {
  return expr && expr->getOpcode() == llvm::Instruction::GetElementPtr;
}

inline bool is_const(const llvm::Value *v) {
  return llvm::isa<llvm::Constant>(v);
}

inline bool is_global_value(const llvm::Value *v) {
  return llvm::isa<llvm::GlobalValue>(v);
}

inline bool is_null(const llvm::Constant *c) {
  return c->isNullValue();
}

inline bool is_undefined(const llvm::Value *v) {
  return llvm::isa<llvm::UndefValue>(v);
}

inline bool is_single_value_type(const llvm::Value *v) {
  return v->getType()->isSingleValueType();
}

inline bool is_ptr_to_int(const llvm::ConstantExpr *expr) {
  return expr && expr->getOpcode() == llvm::Instruction::PtrToInt;
}

inline bool is_int_to_ptr(const llvm::ConstantExpr *expr) {
  return expr && expr->getOpcode() == llvm::Instruction::IntToPtr;
}

inline bool is_const_int(const llvm::Value *v) {
  return llvm::isa<llvm::ConstantInt>(v);
}

inline bool is_argument(const llvm::Value *v) {
  return llvm::isa<llvm::Argument>(v);
}

inline void assert_valid_const_expr(const llvm::ConstantExpr *expr) {  
  assert(is_ptr_to_int(expr) ||
         is_int_to_ptr(expr) ||
         is_gep(expr));
}

inline llvm::ConstantExpr *const_expr(llvm::Value **c) {
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

inline llvm::ConstantExpr *const_expr(llvm::Constant **c) {
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

inline llvm::ConstantStruct *const_struct(llvm::Constant *c) {
  return llvm::dyn_cast<llvm::ConstantStruct>(c);
}

inline llvm::ConstantArray *const_array(llvm::Constant *c) {
  return llvm::dyn_cast<llvm::ConstantArray>(c);
}

inline const llvm::ConstantInt *const_int(llvm::Value *v) {
  return llvm::dyn_cast<llvm::ConstantInt>(v);
}

inline const llvm::IntToPtrInst *get_int_to_ptr(const llvm::Value *v) {
  return llvm::dyn_cast<llvm::IntToPtrInst>(v);
}

inline const llvm::GetElementPtrInst *get_gep(const llvm::Value *v) {
  return llvm::dyn_cast<llvm::GetElementPtrInst>(v);
}

inline const llvm::ConstantExpr *get_const_expr(const llvm::Value *v) {
  return llvm::dyn_cast<llvm::ConstantExpr>(v);
}

inline const llvm::Instruction *get_inst(const llvm::Value *v) {
  return llvm::dyn_cast<llvm::Instruction>(v);
}

inline const llvm::GlobalVariable *get_global(const llvm::Value *v) {
  return llvm::dyn_cast<llvm::GlobalVariable>(v);
}

inline const llvm::BitCastInst *get_bitcast(const llvm::Value *v) {
  return llvm::dyn_cast<llvm::BitCastInst>(v);
}

inline const llvm::User *get_user(const llvm::Value *v) {
  return llvm::dyn_cast<llvm::User>(v);
}

inline const llvm::StructType *struct_type(const llvm::Type *typ) {
  return llvm::dyn_cast<llvm::StructType>(typ);
}

#endif // end PREDICATES_H
