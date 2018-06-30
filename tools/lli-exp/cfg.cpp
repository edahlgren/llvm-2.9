//===- cfg.cpp - Control Flow utilities -----------------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#include "analysis.h"
#include "write_dot.h"

#include "llvm/Assembly/Writer.h"
#include "llvm/Support/CFG.h"
#include "llvm/Support/DOTGraphTraits.h"
#include "llvm/Support/GraphWriter.h"
#include "llvm/Support/raw_ostream.h"

namespace llvm {
template<>
struct DOTGraphTraits<Function*> : public DefaultDOTGraphTraits {

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
    if (const BranchInst *BI = dyn_cast<BranchInst>(Node->getTerminator()))
      if (BI->isConditional())
        return (I == succ_begin(Node)) ? "T" : "F";
    
    // Label source of switch edges with the associated value.
    if (const SwitchInst *SI = dyn_cast<SwitchInst>(Node->getTerminator())) {
      unsigned SuccNo = I.getSuccessorIndex();

      if (SuccNo == 0) return "def";
      
      std::string Str;
      raw_string_ostream OS(Str);
      OS << SI->getCaseValue(SuccNo)->getValue();
      return OS.str();
    }    
    return "";
  }
};
}

void print_function_control_flow(llvm::Function *f, llvm::raw_ostream &os) {  
  llvm::write_dot_graph(f, os);
}
