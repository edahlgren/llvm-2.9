//===- cfg.cpp - Control Flow type ----------------------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#ifndef CFG_H
#define CFG_H

#include "llvm/Analysis/CallGraph.h"
#include "llvm/Module.h"
#include "llvm/Support/raw_ostream.h"

// FunctionsMap maps Functions to a wrapper node. The wrapper node links
// a Function to other functions in a graph.
typedef std::map<const llvm::Function *, llvm::CallGraphNode *> FunctionsMap;

// FunctionGraph contains a call graph of functions.
class FunctionGraph {
public:
  // 1. State.
  llvm::CallGraphNode *root;
  llvm::CallGraphNode *ext;
  llvm::CallGraphNode *calls_ext;
  FunctionsMap functions;
  
  // 2. Construction.
  FunctionGraph() {
    llvm::Function *NullFunction = 0;  
    root = 0;
    ext = new llvm::CallGraphNode(const_cast<llvm::Function *>(NullFunction));
    calls_ext = new llvm::CallGraphNode(0);
  }
  
  // 3. Deconstruction.
  ~FunctionGraph() {
    // Was the CallExt node ever initialized?
    if (calls_ext) {
      // Deallocate the CallsExt node.
      calls_ext->allReferencesDropped();
      delete calls_ext;
      calls_ext = 0;
    }

    // Were functions ever added?
    if (functions.empty()) {
      return;
    }
    
    // Deallocate added CallGraphNodes.
    for (FunctionsMap::iterator i = functions.begin(), e = functions.end();
         i != e; ++i) {
      i->second->allReferencesDropped();
      delete i->second;
      i->second = 0;
    }
    
    // Deallocate the map.
    functions.clear();    
  }

  // 4. Iteration
  // (none)

  // 5. Utility methods
  // (none)
};

void link_function_to_graph(FunctionGraph *fg, llvm::Function *f);

#endif // end CFG_H
