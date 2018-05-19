//===- CallGraphUtils.cpp - Utilities for call graphs ---------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#include "CallGraphUtils.h"
#include "llvm/Analysis/CallGraph.h"
#include "llvm/Module.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

void llvm::PrintCallGraph(Module &M) {
  // Intialize a CallGraph.
  CallGraph *cg = NewBasicCallGraph(M);

  // Print the CallGraph to stdout.
  raw_ostream &OS = outs();
  cg->print(OS, &M);
  delete cg;
  OS.flush();
}
