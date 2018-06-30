//===- cfg.cpp - DominatorTree utilities ----------------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#include "analysis.h"
#include "write_dot.h"
#include "dot_traits.h"

#include "llvm/Analysis/Dominators.h"
#include "llvm/Support/raw_ostream.h"

void print_dominator_tree(llvm::Function *f, llvm::raw_ostream &os) {
  llvm::DominatorTree *d = new llvm::DominatorTree();
  d->runOnFunction(*f);
  llvm::write_dot_graph(d, os);
  delete d;

}
