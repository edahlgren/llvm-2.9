//===- dot_traits.h - Specializations of DOTGraphTraits -------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#include "llvm/Analysis/Dominators.h"
#include "llvm/Assembly/Writer.h"
#include "llvm/Support/CFG.h"
#include "llvm/Support/DOTGraphTraits.h"

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

template<>
struct DOTGraphTraits<DominanceNode*> : public DefaultDOTGraphTraits {

  DOTGraphTraits (bool isSimple=false)
    : DefaultDOTGraphTraits(isSimple) {}

  std::string getNodeLabel(DominanceNode *Node, DominanceNode *Graph) {
    BasicBlock *BB = Node->block;

    if (!BB)
      return "Post dominance root node";

    DOTGraphTraits<Function *> dtraits(isSimple());
    return dtraits.getNodeLabel(BB, BB->getParent());
  }
};

template<>
struct DOTGraphTraits<DominanceGraph*> : public DOTGraphTraits<DomTreeNode*> {
  
  DOTGraphTraits (bool isSimple=false)
    : DOTGraphTraits<DominanceNode*>(isSimple) {}

  static std::string getGraphName(DominanceGraph *Graph) {
    return "Dominator tree";
  }

  std::string getNodeLabel(DominanceNode *Node, DominanceGraph *Graph) {
    return DOTGraphTraits<DominanceNode*>::getNodeLabel(Node, Graph->root_node);
  }
};
} // End llvm namespace.
