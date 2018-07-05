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
#include "traits.h"

#include "llvm/Analysis/Dominators.h"
#include "llvm/Support/raw_ostream.h"

void print_dominator_tree(llvm::Function *f, llvm::raw_ostream &os) {
  // Create a title that is tied to our function. The DominatorTree
  // graph name is not as meaningful by default, see dot_traits.h.
  std::string title = "Dominator tree for " + f->getName().str();

  // The DominanceGraph constructor requires a root BasicBlock so we
  // can't build a DominanceGraph from a Function that lacks BasicBlocks.
  //
  // Since declared but not defined Functions lack BasicBlocks, handle
  // them specially.
  if (f->isDeclaration()) {
    // Since we know nothing about f, write an empty digraph. Use the
    // GraphTraits specialization for the control flow graph to do this,
    // which can handle empty Functions.
    //
    // But make sure to use the title above, or else this will be labeled
    // as a CFG instead of a Dominator tree.
    llvm::write_dot_graph(f, os, title);
    return;
  }

  // Build the DominatorTree once we know we can find a well-formed entry
  // BasicBlock to the Function. This BasicBlock will be the root of the graph.
  DominanceGraph *dg = new DominanceGraph(f->front());

  // Serialize the graph to os using our custom title.
  llvm::write_dot_graph(dg, os, title);

  // Don't forget to cleanup.
  delete dg;
}
