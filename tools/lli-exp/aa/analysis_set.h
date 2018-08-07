//===- analysis_set.h - Anders pointer analysis ---------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#ifndef ANALYSIS_H
#define ANALYSIS_H

#include "constraints.h" // for Constraints, ConstraintInstMap
#include "nodes.h"       // for Nodes, NodeMap
#include "structs.h"     // for Structs
#include "ext.h"         // ExtInfo

#include "llvm/ADT/DenseSet.h" // for llvm::DenseSet
#include "llvm/Module.h"       // for llvm::Function

#include <set> // for std::set

class AnalysisSet {
 public:
  // State required for the analysis
  // =======================================================
  //
  // These are llvm::Values, wrapped in metadata and loosely
  // organized by type of node.
  Nodes *nodes;

  // These are memory-access relationships between two nodes
  // (read: expresses loads, stores, copies, etc).
  Constraints *constraints;

  // This orders the constraints in a graph (read: expresses
  // how loads and store preceed/succeed each other).
  ConstraintGraph *cgraph;
  
  // Module metadata that speeds up the analysis
  // =======================================================
  Structs structs;
  llvm::DenseSet<llvm::Value *> addr_taken_args;
  std::map<string, u32> static_returns;
  
  // External stubs that makes the analysis more meaningful
  // =======================================================
  ExtInfo ext_info;

  // Insert all of the nodes you ever need to insert from the
  // module.
  AnalysisSet(llvm::Module *m) {
    u32 max_size = 1000000000;
    cgraph = ConstraintGraph(max_size);
    init(m);
  }

 private:
  void init(llvm::Module *m);
};

void print_named_constraints(AnalysisSet *as, int l);

#endif // end ANALYSIS_H
