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

#include "llvm/Assembly/Writer.h"
#include "llvm/BasicBlock.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/GraphTraits.h"
#include "llvm/Support/CFG.h"
#include "llvm/Support/DOTGraphTraits.h"
#include "llvm/Support/raw_ostream.h"

typedef llvm::GraphTraits<llvm::BasicBlock *> BlockTraits;
typedef llvm::GraphTraits<llvm::Inverse<llvm::BasicBlock *> > InvBlockTraits;
typedef std::vector<llvm::BasicBlock *>::iterator BlockIterator;

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
  
  llvm::BasicBlock *header() {
    return blocks.front();
  }

  llvm::BasicBlock *latch() {
    typedef InvBlockTraits::ChildIteratorType iterator;

    llvm::BasicBlock *l;
    for (iterator pi = InvBlockTraits::child_begin(header()),
           pe = InvBlockTraits::child_end(header()); pi != pe; ++pi) {
      if (this->contains(*pi)) {
        if (l)
          return 0;
        l = *pi;
      }
    }
    return l;
  }

  bool exits(llvm::BasicBlock *bb) {
    llvm::BasicBlock *_bb = const_cast<llvm::BasicBlock *>(bb);
    for (BlockTraits::ChildIteratorType si = BlockTraits::child_begin(_bb),
           se = BlockTraits::child_end(_bb); si != se; ++si) {
      if (!this->contains(*si))
        return true;
    }
    return false;
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

typedef std::vector<Loop *>::iterator LoopsIterator;

class Loops {
public:
  typdef llvm::DenseMap<llvm::BasicBlock *, Loop *>::const_iterator block_iterator;
  llvm::DenseMap<llvm::BasicBlock *, Loop *> bb_map;
  std::vector<Loop *> top_level_loops;

  typedef typename std::vector<Loop *>::const_iterator iterator;
  iterator begin() const { return top_level_loops.begin(); }
  iterator end() const { return top_level_loops.end(); }
  bool empty() const { return top_level_loops.empty(); }

  Loops(DominanceGraph *dg) {
    init(dg);
  }

  Loop *find_loop(llvm::BasicBlock *bb) {
    block_iterator i = this->bb_map.find(bb);
    return i != ll->bb_map.end() ? i->second : 0;
  }

  void map_block(llvm::BasicBlock *bb, Loop *l) {
    if (!l) {
      this->bb_map.erase(bb);
      return;
    }

    this->bb_map[bb] = l;
  } 

  void init(DominanceGraph *dg);

  void print(llvm::raw_ostream &os, std::string prefix = "");
};

namespace llvm {
template <>
struct GraphTraits<Loop *> {
  typedef BasicBlock NodeType;
  typedef succ_iterator ChildIteratorType;

  static NodeType *getEntryNode(Loop *l) {
    return l->header();
  }
  static inline ChildIteratorType child_begin(NodeType *node) {
    return succ_begin(node);
  }
  static inline ChildIteratorType child_end(NodeType *node) {
    return succ_end(node);
  }

  typedef Loop::block_iterator nodes_iterator;

  static nodes_iterator nodes_begin(Loop *l) {
    return l->block_begin();
  }
  static nodes_iterator nodes_end(Loop *l) {
    return l->block_end();
  }
};

template <>
struct DOTGraphTraits<Loop *> : public DefaultDOTGraphTraits {
  DOTGraphTraits(bool isSimple=false)
    : DefaultDOTGraphTraits(isSimple) {}

  std::string getNodeLabel(BasicBlock *block, Loop *loop) {
    std::string str;
    if (block == loop->header())
      str += "<header> ";

    if (block == loop->latch())
      str += "<latch> ";

    if (loop->exits(block))
      str += "<exiting> ";

    DOTGraphTraits<Function *> dtraits(isSimple());
    return str + dtraits.getNodeLabel(block, block->getParent());
  }  
};
} // end llvm namespace.

#endif // end DOMINATOR_H
