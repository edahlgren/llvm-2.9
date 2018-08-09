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
#include <string>

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
  std::map<std::string, u32> static_returns;
  
  // External stubs that makes the analysis more meaningful
  // =======================================================
  ExtInfo ext_info;

  // Insert all of the nodes you ever need to insert from the
  // module.
  AnalysisSet(llvm::Module *m) {
    init(m);
  }

 private:
  void init(llvm::Module *m);
};

class IDSet {
 public:
  llvm::DenseMap<u32, u32> cache;

  void add(u32 id, u32 num_fields) {
    cache[id] = num_fields;
  }

  bool lookup(u32 id, u32 *num_fields = 0) {
    llvm::DenseMap<u32, u32>::iterator i = cache.find(id);
    if (i == cache.end()) {
      return false;
    }
    if (num_fields)  {
      *num_fields = i->second;
    }
    return true;
  }
};

u32 gep_off(AnalysisSet *as, llvm::User *u);

void init_function_internals(llvm::Module *m, AnalysisSet *as);

void print_named_constraints(AnalysisSet *as, int l);


#endif // end ANALYSIS_H
