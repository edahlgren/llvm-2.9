//===- function_state.h - ----------------------------------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#ifndef DEBUG_STATE_H
#define DEBUG_STATE_H

#include "llvm/Instructions.h"        // for llvm::CallInst, llvm::InvokeInst,
                                      //     llvm::CmpInst
#include "llvm/Support/raw_ostream.h" // for llvm::raw_ostream
#include "llvm/Type.h"                // for llvm::Type

#include <string>

static std::string inst_type_str(const llvm::Instruction *inst) {
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

static std::string inst_str(const llvm::Instruction *inst) {
  std::string str;
  llvm::raw_string_ostream os(str);
  
  os << "op: " << inst->getOpcodeName(inst->getOpcode())
     << " type: " << inst_type_str(inst);

  return os.str();
}

#endif // end DEBUG_STATE_H
