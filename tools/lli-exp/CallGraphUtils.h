//===- CallGraphUtils.h - Top-Level BugPoint class --------------*- C++ -*-===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#ifndef CALL_GRAPH_UTILS_H
#define CALL_GRAPH_UTILS_H

#include "llvm/Module.h"

namespace llvm {

// FunctionGraph contains a call graph of functions.
class FunctionGraph {
public:
  // root is the
  CallGraphNode *root;

  // ext is the
  CallGraphNode *ext;

  // calls Ext is the
  CallGraphNode *calls_ext;

  // functionMap is the
  std::map<const Function *, CallGraphNode *> functions;
  
  // Construct the graph.
  FunctionGraph() {
    root = 0;
    ext = new CallGraphNode(const_cast<Function*>(0));
    calls_ext = new CallGraphNode(0);
  }
  
  // Destroy the graph.
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
    
    // Deallocate added functions.
    for (functionMap::iterator i = functions.begin(), e = functions.end();
         i != e; ++i) {
      delete i->second;
    }
    
    // Deallocate the map.
    functions.clear();    
  }
}

void link_function_to_graph(FunctionGraph *fg, Function *F);

// print_entire_call_graph uses BasicCallGraph to print a Module's functions.  
void print_module_call_graph(Module &M);
  
} // end llvm namespace

#endif
