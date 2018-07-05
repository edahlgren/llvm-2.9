//===- analysis.h - Header for lli-exp --------------------------*- C++ -*-===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#ifndef ANALYSIS_H
#define ANALYSIS_H

#include "llvm/Module.h"

#include <iostream>

// ---- Debugging.

const char *instruction_type(llvm::BasicBlock::iterator i);

// ---- Function analysis.

bool user_is_a_caller(llvm::Value::use_iterator i);
bool user_calls_this_function(llvm::Value::use_iterator i);
bool function_is_undefined(llvm::Function *f);

// ---- Block analysis.

bool unconditional_path(llvm::Function *f, llvm::BasicBlock *target);

#endif // end ANALYSIS_H
