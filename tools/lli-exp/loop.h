//===- loop.h - Loop structures -------------------------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#ifndef LOOP_H
#define LOOP_H

#include "llvm/BasicBlock.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/GraphTraits.h"
#include "llvm/Support/DOTGraphTraits.h"

class Loop {
public:
  Loop *parent_loop;
  std::vector<Loop *> sub_loops;
  std::vector<llvm::BasicBlock *> blocks;

  typedef typename std::vector<Loop *>::const_iterator iterator;
  iterator begin() const { return sub_loops.begin(); }
  iterator end() const { return sub_loops.end(); }
  bool empty() const { return sub_loops.empty(); }
  
  Loop(llvm::BasicBlock *bb) : parent_loop(0) {
    blocks.push_back(bb);
  }
};

class Loops {
public:
  DenseMap<BlockT *, Loop *> bb_map;
  std::vector<Loop *> top_level_loops;

  typedef typename std::vector<Loop *>::const_iterator iterator;
  iterator begin() const { return top_level_loops.begin(); }
  iterator end() const { return top_level_loops.end(); }
  bool empty() const { return top_level_loops.empty(); }
};

typedef std::vector<Loop *>::iterator LoopsIterator;
typedef GraphTraits<Inverse<llvm::BasicBlock *> > InvBlockTraits;
typedef std::vector<llvm::BasicBlock *>::iterator BlockIterator;

template <> struct GraphTraits<const Loop *> {
  typedef const Loop NodeType;
  typedef Loop::iterator ChildIteratorType;

  static NodeType *getEntryNode(const Loop *L) {
    return L;
  }
  static inline ChildIteratorType child_begin(NodeType *N) {
    return N->begin();
  }
  static inline ChildIteratorType child_end(NodeType *N) {
    return N->end();
  }
};

template <> struct GraphTraits<Loop *> {
  typedef Loop NodeType;
  typedef Loop::iterator ChildIteratorType;

  static NodeType *getEntryNode(Loop *L) {
    return L;
  }
  static inline ChildIteratorType child_begin(NodeType *N) {
    return N->begin();
  }
  static inline ChildIteratorType child_end(NodeType *N) {
    return N->end();
  }
};

template <>
struct DOTGraphTraits<Loop *> : public DefaultDOTGraphTraits {
  DOTGraphTraits (bool isSimple=false)
    : DefaultDOTGraphTraits(isSimple) {}

  std::string getNodeLabel(Loop *node, Loop *parent) {
    BasicBlock *header = node->blocks.front();
    return header->getName();
  }  
};

Loops *find_loops(DominanceGraph *dg);

#endif // end DOMINATOR_H
