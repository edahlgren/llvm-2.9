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

#include <set> // for std::set

class AnalysisSet {
 public:
  Nodes nodes;
  Constraints constraints;
  ExtInfo ext_info;
  Structs structs;

  std::set<u32> indirect_calls;
  llvm::DenseSet<llvm::Value *> at_args;
  ConstraintInstMap indirect_constraints;
  NodeMap deref_to_var_nodes;

  AnalysisSet(llvm::Module *m) {
    init(m);
  }

 private:
  init(llvm::Module *m);
};

#define FUNC_NODE_OFF_RET 1
#define FUNC_NODE_OFF_ARG0 2

#endif // end ANALYSIS_H
