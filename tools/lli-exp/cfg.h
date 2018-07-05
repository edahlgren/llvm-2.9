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
#include "llvm/Assembly/Writer.h"
#include "llvm/Module.h"
#include "llvm/Support/CFG.h"
#include "llvm/Support/DOTGraphTraits.h"
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

namespace llvm {
template<>
struct DOTGraphTraits<Function *> : public DefaultDOTGraphTraits {

  DOTGraphTraits(bool isSimple=false) : DefaultDOTGraphTraits(isSimple) {}

  static std::string getGraphName(const Function *F) {
    return "CFG for '" + F->getNameStr() + "' function";
  }

  static std::string getCompleteNodeLabel(BasicBlock *Node, 
                                          Function *Graph) {
    std::string Str;
    raw_string_ostream OS(Str);

    if (Node->getName().empty()) {
      WriteAsOperand(OS, Node, false);
      OS << ":";
    }

    OS << *Node;
    std::string OutStr = OS.str();
    if (OutStr[0] == '\n') OutStr.erase(OutStr.begin());

    // Process string output to make it nicer...
    for (unsigned i = 0; i != OutStr.length(); ++i)
      if (OutStr[i] == '\n') {                            // Left justify
        OutStr[i] = '\\';
        OutStr.insert(OutStr.begin()+i+1, 'l');
      } else if (OutStr[i] == ';') {                      // Delete comments!
        unsigned Idx = OutStr.find('\n', i+1);            // Find end of line
        OutStr.erase(OutStr.begin()+i, OutStr.begin()+Idx);
        --i;
      }

    return OutStr;
  }

  std::string getNodeLabel(BasicBlock *Node,
                           Function *Graph) {
    return getCompleteNodeLabel(Node, Graph);
  }

  static std::string getEdgeSourceLabel(BasicBlock *Node,
                                        succ_iterator I) {    
    // Label source of conditional branches with "T" or "F"
    const BranchInst *BI = dyn_cast<BranchInst>(Node->getTerminator());
    if (BI) {
      if (BI->isConditional())
        return (I == succ_begin(Node)) ? "T" : "F";
    }
    
    // Label source of switch edges with the associated value.
    const SwitchInst *SI = dyn_cast<SwitchInst>(Node->getTerminator());
    if (SI) {
      unsigned SuccNo = I.getSuccessorIndex();

      if (SuccNo == 0) return "def";
      
      std::string Str;
      raw_string_ostream os(Str);
      os << SI->getCaseValue(SuccNo)->getValue();
      return os.str();
    }    
    return "";
  }
};
} // end llvm namespace.

void link_function_to_graph(FunctionGraph *fg, llvm::Function *f);

#endif // end CFG_H
