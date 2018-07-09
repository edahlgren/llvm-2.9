//===- loop.h - Loop structures -------------------------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#ifndef LOOP_H
#define LOOP_H

#include "dominator.h"

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

  typedef typename std::vector<llvm::BasicBlock*>::const_iterator block_iterator;
  block_iterator block_begin() const { return blocks.begin(); }
  block_iterator block_end() const { return blocks.end(); }
  
  Loop(llvm::BasicBlock *bb) : parent_loop(0) {
    blocks.push_back(bb);
  }

  ~Loop() {
    for (size_t i = 0, e = sub_loops.size(); i != e; ++i)
      delete sub_loops[i];
  }
  
  bool contains(const Loop *l) {
    if (l == this)
      return true;

    if (l == 0)
      return false;

    return contains(l->parent_loop);
  }

  bool contains(const llvm::BasicBlock *bb) const {
    return std::find(block_begin(), block_end(), bb) != block_end();
  }
};

class Loops {
public:
  llvm::DenseMap<llvm::BasicBlock *, Loop *> bb_map;
  std::vector<Loop *> top_level_loops;

  typedef typename std::vector<Loop *>::const_iterator iterator;
  iterator begin() const { return top_level_loops.begin(); }
  iterator end() const { return top_level_loops.end(); }
  bool empty() const { return top_level_loops.empty(); }
};

typedef std::vector<Loop *>::iterator LoopsIterator;
typedef llvm::GraphTraits<llvm::Inverse<llvm::BasicBlock *> > InvBlockTraits;
typedef std::vector<llvm::BasicBlock *>::iterator BlockIterator;

namespace llvm {
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

  typedef Loop::iterator nodes_iterator;

  static nodes_iterator nodes_begin(Loop *l) { return l->begin(); }
  static nodes_iterator nodes_end(Loop *l) { return l->end(); }
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
} // end llvm namespace.

Loops *find_loops(DominanceGraph *dg);

#endif // end DOMINATOR_H
