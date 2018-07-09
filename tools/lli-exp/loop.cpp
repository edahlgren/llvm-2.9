//===- loop.cpp - Loop utilities ------------------------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#include "dominator.h"
#include "loop.h"

#include "llvm/ADT/DepthFirstIterator.h"

#include <map>

static Loop *get_loop_starting_at(Loops *ll, llvm::BasicBlock *bb) {
  typename llvm::DenseMap<llvm::BasicBlock *, Loop *>::const_iterator i =
    ll->bb_map.find(bb);
  return i != ll->bb_map.end() ? i->second : 0;
}

static void find_backedges(DominanceGraph *dg, llvm::BasicBlock *bb,
                    std::vector<llvm::BasicBlock *> &backedges) {
  for (typename InvBlockTraits::ChildIteratorType i =
         InvBlockTraits::child_begin(bb), e = InvBlockTraits::child_end(bb);
       i != e; ++i) {
    llvm::BasicBlock *n = *i;
    
    // Does bb dominate this predecessor?
    if (dominates(dg, bb, n)) {
      // Then this is a backedge. Add it to the list.
      backedges.push_back(n);
    }
  }
}

static void move_loop_into(Loop *l, Loop *parent, bool sibling = false) {
  if (sibling) {
    llvm::BasicBlock *header = l->blocks.front();
    assert(parent->contains(header) && "Can't insert loop because parent doesn't contain header");
    
    unsigned num_sub_loops = static_cast<unsigned>(parent->sub_loops.size());
    for (unsigned i = 0; i != num_sub_loops; ++i) {
      Loop *sub_loop = parent->sub_loops[i];
      
      if (sub_loop->contains(header)) {
        move_loop_into(l, sub_loop, sibling);
        return;
      }
    }
  }

  parent->sub_loops.push_back(l);
  l->parent_loop = parent;
}

static void reparent_loop(Loop *child, Loop *new_parent, bool sibling = false) {
  Loop *old_parent = child->parent_loop;
  if (sibling) {
    assert(old_parent && old_parent == new_parent->parent_loop);
    assert(child != new_parent);
  }
  
  LoopsIterator i =
    std::find(old_parent->sub_loops.begin(),
              old_parent->sub_loops.end(),
              child);
  assert(i != old_parent->sub_loops.end());

  old_parent->sub_loops.erase(i);
  child->parent_loop = 0;

  move_loop_into(child, new_parent, sibling);
}

static bool add_block_to_loop(Loops *ll, DominanceGraph *dg,
                       Loop *l, llvm::BasicBlock *x) {
  llvm::BasicBlock *bb = l->blocks[0];
  // See note below, I think this may be misleading.
  llvm::BasicBlock *entry_block = bb->getParent()->begin();

  // Have we already processed this block?
  if (l->contains(x)) {
    // Skip it.
    return false;
  }
    
  // Is this block unreachable from the entry block?
  if (!dominates(dg, entry_block, x)) {
    // Skip it.
    return false;
  }

  // Does this block begin a sub loop?
  Loop *sub_loop = get_loop_starting_at(ll, x);
  if (sub_loop) {
    // Then the sub loop should be an inner loop of the newly
    // created loop l above. Reparent it.
    
    // First do a sanity check.
    Loop *old_parent = sub_loop->parent_loop;
    assert(old_parent &&old_parent != l);

    // Then reparent it.
    reparent_loop(sub_loop, l);
  }

  // And for every block, regardless of whether it's in
  // another loop, add this block to the blocks list.
  l->blocks.push_back(x);

  return true;
  
}

static void fix_loop_nesting(Loop *l) {
  std::map<llvm::BasicBlock *, Loop *> containing_loops;
  for (unsigned i = 0; i != l->sub_loops.size(); ++i) {
    Loop *child = l->sub_loops[i];
    assert(child->parent_loop == l && "invalid child loop");

    llvm::BasicBlock *header = child->blocks.front();
    Loop *containing_loop = containing_loops[header];
    if (containing_loop) {
      reparent_loop(child, containing_loop, true /* siblings */);
      --i;
      continue;
    }

    for (unsigned b = 0, e = child->blocks.size(); b != e; ++b) {

      Loop *block_loop = containing_loops[child->blocks[b]];
      if (!block_loop) {
          containing_loops[header] = child;
          continue;
      }

      if (block_loop != child) {
        Loop *sub_loop = block_loop;
        for (unsigned j = 0, f = sub_loop->blocks.size(); j != f; ++j) {
          containing_loops[sub_loop->blocks[j]] = child;
        }

        reparent_loop(sub_loop, child, true /* siblings */);
        --i;
      }
    }    
  }
}

static Loop *check_for_loop(Loops *ll, DominanceGraph *dg, llvm::BasicBlock *bb) {
  // Have we already processed this block?
  if (ll->bb_map.find(bb) != ll->bb_map.end()) {
    // Then return no loop.
    return 0;
  }

  // Find backedges to bb. An edge (u -> v) is said to be a
  // backedge if v dominates u. These are potential sources of iteration.
  std::vector<llvm::BasicBlock *> backedges;
  find_backedges(dg, bb, backedges);

  // Did we fail to find any backedges?
  if (backedges.empty()) {
    // Then we return that bb is not the first block in the loop.
    return 0;
  }

  // Create a new loop with bb as the first block.
  //
  // This is different from the "entry block" below, which is the parent of bb.
  // This actually seems really confusing/wrong. Some text says that bb is the
  // entry block, which is immediately precedeed by an "initialization block",
  // which is outside of the loop and searves as the "loop header".
  //
  // FIXME: Get the terminology right.
  Loop *l = new Loop(bb);

  // Map the first block to its loop.
  ll->bb_map[bb] = l;

  // Process all of the backedges.
  while (!backedges.empty()) {
    llvm::BasicBlock *x = backedges.back();
    backedges.pop_back();

    // Can we add this block?
    if (add_block_to_loop(ll, dg, l, x)) {
      // Add all of the x's predecessors to be processed.
      backedges.insert(backedges.end(), InvBlockTraits::child_begin(x),
                       InvBlockTraits::child_end(x));    
    }
  }

  // For each of the blocks in this loop ...
  for (BlockIterator i = l->blocks.begin(),
         e = l->blocks.end(); i != e; ++i) {
    // Add unseen inner loops.
    Loop *new_loop = check_for_loop(ll, dg, *i);
    if (new_loop) {
      l->sub_loops.push_back(new_loop);
      new_loop->parent_loop = l;
    }
    
    // Make sure it's connected to the loop forest.
    ll->bb_map.insert(std::make_pair(*i, l));
  }

  // Move child loops around so that they're properly nested.
  fix_loop_nesting(l);

  return l;  
}

void Loops::init(DominanceGraph *dg) {
  llvm::BasicBlock *root = dg->root_node->block;

  for (llvm::df_iterator<llvm::BasicBlock *> ni = llvm::df_begin(root),
         ne = llvm::df_end(root); ni != ne; ++ni) {
    Loop *l = check_for_loop(this, dg, *ni);
    if (l) {
      this->top_level_loops.push_back(l);
    }
  }
}
