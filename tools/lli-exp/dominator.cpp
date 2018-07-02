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
  // Create a title that is tied to our function. The DominatorTree
  // graph name is not as meaningful by default, see dot_traits.h.
  std::string title = "Dominator tree for " + f->getName().str();

  // The DominatorTree constructor takes for granted that it's
  // called from the FPassManager::runOnFunction, which short-circuits
  // if Function->isDeclaration returns true. If we don't do this,
  // then the DominatorTree constructor will attempt to always get
  // the head of the Function's BasicBlocks list, which will be empty
  // if the Function is a declared but not defined.
  if (f->isDeclaration()) {
    // So to work around the bad code design (that is, DominatorTree
    // is not free from its calling context), write an empty digraph for
    // empty functions to represent that they exist but we cannot compute
    // their DominatorTree. We do this for the control flow graph without
    // any problems because we always use the Function's iterators and
    // never absolute indexing functions like front/back. So use the
    // GraphTraits specialization for Function to write the empty graph.
    llvm::write_dot_graph(f, os, title);
    return;
  }

  // Initialize an empty dominator tree.
  llvm::DominatorTree *d = new llvm::DominatorTree();

  // Build the DominatorTree once we know that there are BasicBlocks to
  // iterate over.
  d->runOnFunction(*f);

  // Serialize the DominatorTree to os using our custom title.
  llvm::write_dot_graph(d, os, title);

  // Don't forget to cleanup.
  delete d;
}
