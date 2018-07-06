//===- write.cpp - Printing implementation --------------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#include "cfg.h"
#include "dominator.h"
#include "graphml.h"
#include "write.h"
#include "write_dot.h"

#include "llvm/Analysis/Dominators.h"

#include <boost/graph/graphml.hpp>

void write_function_control_flow(llvm::Function *f, llvm::raw_ostream &os) {  
  llvm::write_dot_graph(f, os);
}

void write_function_dominator_tree(llvm::Function *f, llvm::raw_ostream &os) {
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
  DominanceGraph *dg = new DominanceGraph(&f->front());

  // Serialize the graph to os using our custom title.
  llvm::write_dot_graph(dg, os, title);

  // Don't forget to cleanup.
  delete dg;
}

void write_function_dominator_tree_old(llvm::Function *f, llvm::raw_ostream &os) {
  std::string title = "Dominator tree for " + f->getName().str();
  if (f->isDeclaration()) {
    llvm::write_dot_graph(f, os, title);
    return;
  }
  
  llvm::DominatorTree *dt = new llvm::DominatorTree();
  dt->runOnFunction(*f);

  llvm::write_dot_graph(dt, os, title);

  delete dt;    
}

void write_function_graph(FunctionGraph *fg, std::ostream &os) {
  GraphML *g = make_function_graphml(fg);

  boost::dynamic_properties dp;
  function_graphml_dynamic_properties(g, &dp);
  
  boost::write_graphml(os, *g, dp, true);

  delete g;
}
