//===- loop_new.cpp - Loop utilities ------------------------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#include "dominator.h"
#include "loop.h"

#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/Support/raw_ostream.h"

typedef std::vector<llvm::BasicBlock *> BlockArray;

// This is allowed to destroy backedges. Keep your own copy if you care.
static void discover_loop_internals(Loops *forest, Loop *loop, DominanceGraph *dg,
                                    std::vector<llvm::BasicBlock *> backedges) {
  unsigned num_blocks = 0, num_sub_loops = 0;

  while (!backedges.empty()) {
    // Backedges must be DFS ordered, so this is a backwards DFS
    // traversal.
    llvm::BasicBlock *pred = backedges.back();
    backedges.pop_back();

    // Have we discovered this block yet?
    Loop *sub_loop = forest->find_loop(pred);
    if (!sub_loop) {

      // Is the block in the scope of the dominance graph?
      //
      // Normally this means that it's in the scope of a single function,
      // where dg->root_node is the function's entry block.
      if (!dominates(dg, dg->root_node->block, pred)) {
        // Nope, skip it and continue.
        continue;
      }

      // Map the undiscovered block into the forest.
      forest->map_block(pred, loop);

      // We processed another block.
      ++num_blocks;

      // Is this block the loop header?
      if (pred == loop->header()) {
        // Then it doesn't have any predecessors we should explore,
        // so keep it moving.
        continue;
      }

      // Push predecessors of this block onto the list to process, they
      // might be inner blocks of the loop or headers of a sub loop.
      backedges.insert(backedges.end(), InvBlockTraits::child_begin(pred),
                       InvBlockTraits::child_end(pred));

      // And process those new backedges.
      continue;
    }

    // Yes, we discovered this block. Find its outer-most discovered loop.
    Loop *outermost_parent = sub_loop;
    while (Loop *parent = outermost_parent->parent_loop)
      outermost_parent = parent;

    // Is its outermost parent loop the same as the target loop?
    if (outermost_parent == loop) {
      // Then sub_loop is already known to be a sub loop of the target, so
      // skip it and continue.
      continue;
    }

    // Nope, then the sub_loop is a newly discovered loop of the target loop.
    ++num_sub_loops;

    // Attach it to the target loop via it's outermost parent.
    outermost_parent->parent_loop = loop;

    // Increase the number of blocks to be those in the outermost parent.
    num_blocks += outermost_parent->blocks.capacity();

    // Continue traversing along predecessors we haven't seen yet (we treat
    // the outermost parent loop as "seen"). One of these predecessors may
    // lead to another undiscovered sub loop.
    llvm::BasicBlock *outer_header = outermost_parent->header();
    for (typename InvBlockTraits::ChildIteratorType pi =
           InvBlockTraits::child_begin(outer_header),
           pe = InvBlockTraits::child_end(outer_header); pi != pe; ++pi) {
      // Is the loop for this predecessor either undefined or different
      // from the sub loop (the outermost parent) we just processed?
      if (forest->find_loop(*pi) != outermost_parent) {
        // Then add it to the processing queue.
        backedges.push_back(*pi);
      }
    }

    // And process the new backedges, if any were added.
  }

  // Finally expand storage for loops and blocks. At this point they're
  // not inserted, but the blocks are mapped in the forest so we know which
  // loops the blocks belong to.
  loop->sub_loops.reserve(num_sub_loops);
  loop->blocks.reserve(num_blocks);
}

void populate_loops_with_block(Loops *forest, llvm::BasicBlock *bb) {
  // For each of the loops that contain bb, until we get to the top-most
  // one ...
  for (Loop *sub_loop = forest->find_loop(bb); sub_loop;
       sub_loop = sub_loop->parent_loop) {

    // Is this block the header?
    if (bb != sub_loop->header()) {
      // No, it's not special, then just add it to the loop's blocks.
      sub_loop->blocks.push_back(bb);
      continue;
    }

    // Yes it's a header block. We'll take this opportunity to do loop-level
    // things, like manage its loop's parent and reverse its ordering.

    // Is this loop a top level loop (indicated by having no parent?).
    if (!sub_loop->parent_loop) {
      // Then add it to the forest as such.
      forest->top_level_loops.push_back(sub_loop);
    } else {
      // Otherwise add this loop as a sub loop of its parent.
      sub_loop->parent_loop->sub_loops.push_back(sub_loop);
    }

    // Since blocks and sub loops are inserted in post-order, reverse the
    // lists, except for the loop header, which is always the first block.
    std::reverse(sub_loop->blocks.begin() + 1,
                 sub_loop->blocks.end());
    std::reverse(sub_loop->sub_loops.begin(),
                 sub_loop->sub_loops.end());
  }
}

class DFSBlockStack {
public:
  typedef typename BlockTraits::ChildIteratorType block_succ_iter;
  std::vector<std::pair<llvm::BasicBlock *, block_succ_iter> > stack;

  void push(llvm::BasicBlock *bb) {
    stack.push_back(std::make_pair(bb, BlockTraits::child_begin(bb)));
  }

  void pop() {
    stack.pop_back();
  }
  
  bool empty() {
    return stack.empty();
  }

  llvm::BasicBlock *top_block() {
    return stack.back().first;
  }

  block_succ_iter &succ() {
    return stack.back().second;
  }

  block_succ_iter succ_end() {
    return BlockTraits::child_end(top_block());
  }
};

void dfs_populate_loops(Loops *forest, llvm::BasicBlock *start) {
  DFSBlockStack dstack;
  llvm::DenseSet<const llvm::BasicBlock *> visited;

  dstack.push(start);
  visited.insert(start);

  while (!dstack.empty()) {
    while (dstack.succ() != dstack.succ_end()) {
      llvm::BasicBlock *bb = *dstack.succ();

      ++dstack.succ();

      if (!visited.insert(bb).second)
        continue;

      dstack.push(bb);
    }

    populate_loops_with_block(forest, dstack.top_block());
    dstack.pop();
  }
}
                              
Loops *build_loop_forest(DominanceGraph *dg) {
  Loops *forest = new Loops();
  
  // Traverse the dominator tree using a post-order traversal.
  for (llvm::po_iterator<DominanceNode *> i = llvm::po_begin(dg->root_node),
         e = llvm::po_end(dg->root_node); i != e; ++i) {

    // Treat this block as a potential loop header.
    llvm::BasicBlock *header = i->block;

    // Try to find backedges for it. If there aren't any, then this
    // isn't a loop.
    std::vector<llvm::BasicBlock *> backedges;
    for (typename InvBlockTraits::ChildIteratorType pi =
           InvBlockTraits::child_begin(header),
           pe = InvBlockTraits::child_end(header); pi != pe; ++pi) {
      // Is this predecessor in the scope of our analysis?
      if (!dominates(dg, dg->root_node->block, *pi)) {
        // Nope, try other predecessors.
        continue;
      }

      // Does the prospective header dominate one of its predecessors?
      if (dominates(dg, header, *pi)) {
        // Yes, that's a backage that legitimizes this loop.
        backedges.push_back(*pi);
      }      
    }

    // Did we find any backedges to this block?
    if (!backedges.empty()) {
      // Then make a new loop.
      Loop *loop = new Loop(header);

      // And try to find inner blocks and sub loops.
      discover_loop_internals(forest, loop, dg, backedges);
    }    
  }

  // Do a final depth-first traversal to populate blocks and sub loop
  // vectors for all of the loops.
  dfs_populate_loops(forest, dg->root_node->block);

  // Finally return the forest.  
  return forest;
}
