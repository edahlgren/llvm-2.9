//===- CallGraphUtils.h - Top-Level BugPoint class --------------*- C++ -*-===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#ifndef ANALYSIS_H
#define ANALYSIS_H

#include "llvm/Analysis/CallGraph.h"
#include "llvm/Module.h"
#include "llvm/Support/raw_ostream.h"

#include <iostream>

// FunctionsMap maps Functions to a wrapper node. The wrapper node links
// a Function to other functions in a graph.
typedef std::map<const llvm::Function *, llvm::CallGraphNode *> FunctionsMap;

// FunctionGraph contains a call graph of functions.
class FunctionGraph {
public:
  // root is the 
  llvm::CallGraphNode *root;

  // ext is the
  llvm::CallGraphNode *ext;

  // calls Ext is the
  llvm::CallGraphNode *calls_ext;

  // functionMap is the
  FunctionsMap functions;
  
  // Construct the graph.
  FunctionGraph() {
    llvm::Function *NullFunction = 0;  
    root = 0;
    ext = new llvm::CallGraphNode(const_cast<llvm::Function *>(NullFunction));
    calls_ext = new llvm::CallGraphNode(0);
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
};


bool unconditional_path(llvm::Function *f, llvm::BasicBlock *target);

void link_function_to_graph(FunctionGraph *fg, llvm::Function *f);

void print_graph(FunctionGraph *fg, llvm::raw_ostream &os);

void print_graphml(FunctionGraph *fg, std::ostream &os);

#endif // end ANALYSIS_H
