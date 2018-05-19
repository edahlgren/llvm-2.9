//===- CallGraphUtils.h - Top-Level BugPoint class --------------*- C++ -*-===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#ifndef CALL_GRAPH_UTILS_H
#define CALL_GRAPH_UTILS_H

#include "llvm/Module.h"

namespace llvm {

void PrintCallGraph(Module &M);
  
} // end llvm namespace

#endif
