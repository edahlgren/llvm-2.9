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
  Nodes nodes;
  Constraints constraints;
  ExtInfo ext_info;
  Structs structs;

  std::set<u32> indirect_calls;
  llvm::DenseSet<llvm::Value *> addr_taken_args;
  ConstraintInstMap indirect_constraints;
  NodeMap deref_to_var_nodes;

  AnalysisSet(llvm::Module *m) {
    init(m);
  }

 private:
  void init(llvm::Module *m);
};

#endif // end ANALYSIS_H
