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
#include "llvm/Support/raw_ostream.h"

#include <map>

static Loop *get_loop(Loops *ll, llvm::BasicBlock *bb) {
  typename llvm::DenseMap<llvm::BasicBlock *, Loop *>::const_iterator i =
    ll->bb_map.find(bb);
  return i != ll->bb_map.end() ? i->second : 0;
}

static void find_backedges(DominanceGraph *dg, llvm::BasicBlock *bb,
                    std::vector<llvm::BasicBlock *> &backedges) {
  llvm::errs() << "\t** Finding backedges pointing to " << bb->getName() << "\n";
  for (typename InvBlockTraits::ChildIteratorType i =
         InvBlockTraits::child_begin(bb), e = InvBlockTraits::child_end(bb);
       i != e; ++i) {
    llvm::BasicBlock *n = *i;
    
    // Does bb dominate this predecessor?
    llvm::errs() << "\t    Does " << bb->getName() << " dominate " << n->getName() << "? ... ";
    if (dominates(dg, bb, n)) {
      llvm::errs() << " YES, found backedge";
      // Then this is a backedge. Add it to the list.
      backedges.push_back(n);
    } else {
      llvm::errs() << " NO, continue";
    }
    llvm::errs() << "\n";    
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
  llvm::BasicBlock *bb = l->blocks.front();
  // See note below, I think this may be misleading.
  llvm::BasicBlock *entry_block = bb->getParent()->begin();

  llvm::errs() << "\t** Adding block " << x->getName() << " to loop starting at " << bb->getName() << "\n";
  
  // Have we already processed this block?
  if (l->contains(x)) {
    llvm::errs() << "\t    Loop already contains this block, do nothing\n";
    // Skip it.
    return false;
  }
    
  // Is this block unreachable from the entry block?
  if (!dominates(dg, entry_block, x)) {
    llvm::errs() << "\t    Block is unreachable from function entry block " << entry_block->getName() << "\n";
    // Skip it.
    return false;
  }

  llvm::errs() << "\t    Does " << x->getName() << " begin a sub loop?";
  // Does this block begin a sub loop?
  Loop *sub_loop = get_loop(ll, x);
  if (sub_loop) {
    // Then the sub loop should be an inner loop of the newly
    // created loop l above. Reparent it.
    
    // First do a sanity check.
    Loop *old_parent = sub_loop->parent_loop;
    assert(old_parent &&old_parent != l);

    llvm::errs() << " YES, reparenting " << x->getName()
                 << " from " << old_parent->blocks.front()->getName()
                 << " to " << l->blocks.front()->getName();

    // Then reparent it.
    reparent_loop(sub_loop, l);
  } else {
    llvm::errs() << " NO";
  }
  llvm::errs() << "\n";

  llvm::errs() << "\t    Adding " << x->getName() << " to loop's blocks set\n";
  
  // And for every block, regardless of whether it's in
  // another loop, add this block to the blocks list.
  l->blocks.push_back(x);

  return true;
  
}

static void fix_loop_nesting(Loop *l) {
  llvm::errs() << "\t** Fixing " << l->sub_loops.size() << " sub loops:\n";

  std::map<llvm::BasicBlock *, Loop *> containing_loops;
  for (unsigned i = 0; i != l->sub_loops.size(); ++i) {
    Loop *child = l->sub_loops[i];
    assert(child->parent_loop == l && "invalid child loop");

    llvm::BasicBlock *header = child->blocks.front();
    llvm::errs() << "\t    Is sub loop " << i << " starting at " << header->getName()
                 << " in our stash?\n";

    // If there is already a loop which contains this loop, move this loop
    // into the containing loop.
    
    Loop *containing_loop = containing_loops[header];
    if (containing_loop) {
      llvm::BasicBlock *new_header = containing_loop->blocks.front();
      llvm::errs() << "\t    ... YES, reparenting sub loop to " << new_header->getName() << "\n";
      reparent_loop(child, containing_loop, true /* siblings */);

      // The loop got removed from the SubLoops list, see reparent_loop.
      llvm::errs() << "\t    Moving backwards to index " << i << "\n";
      --i;

      continue;
    }

    // This is currently considered to be a top-level loop.  Check to see
    // if any of the contained blocks are loop headers for subloops we
    // have already processed.

    llvm::errs() << "\t    ... NO, iterating through the blocks in the sub loop:\n";      
    for (unsigned b = 0, e = child->blocks.size(); b != e; ++b) {

      llvm::errs() << "\t    Have we stashed a loop containing block " << child->blocks[b]->getName() << "?\n";
      Loop *block_loop = containing_loops[child->blocks[b]];
      if (!block_loop) {
        llvm::errs() << "\t    ... NO, cache header block " << header->getName()
                     << " to point to its loop ...\n";
        containing_loops[header] = child;

        llvm::errs() << "\t    ... and continue on to the other blocks in this sub loop\n";
        continue;
      }

      llvm::errs() << "\t    ... Yes ...\n";
      if (block_loop != child) {
        llvm::errs() << "\t    ... It's different from this sub loop\n";
        
        // Reparent all of the blocks which used to belong to block_loop.
        
        for (unsigned j = 0, f = block_loop->blocks.size(); j != f; ++j) {
          containing_loops[block_loop->blocks[j]] = child;
        }
        llvm::errs() << "\t    ... So re-stash all of its blocks to point to this sub loop\n";

        // There is already a loop which contains this block, that means
        // that we should reparent the loop which the block is currently
        // considered to belong to to be a child of this loop.
        
        llvm::errs() << "\t    ... And reparent it to be a child of this sub loop\n";
        reparent_loop(block_loop, child, true /* siblings */);

        // The loop got removed from the SubLoops list, see reparent_loop.
        llvm::errs() << "\t    Moving backwards to index " << i << "\n";
        --i;
      } else {
        llvm::errs() << "\t    ... It's the same loop as this sub loop, ignore\n";
      }
    }    
  }
}

static Loop *check_for_loop(Loops *ll, DominanceGraph *dg, llvm::BasicBlock *bb, std::string prefix = "    ") {
  llvm::errs() << "\t** Checking for loop at " << bb->getName() << "\n";

  // Have we already processed this block?
  if (ll->bb_map.find(bb) != ll->bb_map.end()) {
    llvm::errs() << "\t    This block is already mapped to a loop in the forest:\n";
    ll->print(llvm::errs(), "\t" + prefix);
    llvm::errs() << "\t    ... return no loop\n";
    // Then return no loop.
    return 0;
  }

  // Find backedges to bb. An edge (u -> v) is said to be a
  // backedge if v dominates u. These are potential sources of iteration.
  std::vector<llvm::BasicBlock *> backedges;
  find_backedges(dg, bb, backedges);

  // Did we fail to find any backedges?
  if (backedges.empty()) {
    llvm::errs() << "\t    Found no backedges ... return no loop\n";
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
  llvm::errs() << "\t** Adding new loop to the loop forest:\n";
  ll->print(llvm::errs(), "\t    ");

  llvm::errs() << "\t    All backedges:\n";
  for (unsigned i = 0; i < backedges.size(); i++) {
    llvm::errs() << "\t     " << backedges[i]->getName()
                 << " -> " << bb->getName() << "\n";
  }

  llvm::errs() << "\t** Processing all backedges:\n";
  
  // Process all of the backedges.
  while (!backedges.empty()) {
    llvm::BasicBlock *x = backedges.back();
    backedges.pop_back();

    llvm::errs() << "\t    Processing " << x->getName()
                 << " -> " << bb->getName() << "\n";

    // Can we add this block?
    if (add_block_to_loop(ll, dg, l, x)) {
      llvm::errs() << "\t    Added " << x->getName() << " to loop\n";

      unsigned current_size = backedges.size();
      
      // Add all of the x's predecessors to be processed.
      backedges.insert(backedges.end(), InvBlockTraits::child_begin(x),
                       InvBlockTraits::child_end(x));

      unsigned new_size = backedges.size();

      if (current_size > new_size) {
        llvm::errs() << "\t    Inserted " << (new_size - current_size) << " predecessors of "
                     << x->getName() << " as new backedges:\n";
        for (; current_size < new_size; current_size++) {
          llvm::errs() << "\t     " << backedges[current_size]->getName()
                       << " -> " << bb->getName() << "\n";          
        }
      } else {
        llvm::errs() << "\t    No predecessors of " << x->getName()
                     << " to insert as new backedges\n";
      }
    } else {
      llvm::errs() << "\t    Didn't add " << x->getName() << " to loop\n";
    }

    llvm::errs() << "\t    All backedges:\n";
    for (unsigned i = 0; i < backedges.size(); i++) {
      llvm::errs() << "\t      " << backedges[i]->getName()
                   << " -> " << bb->getName() << "\n";
    }
  }

  llvm::errs() << "\t** Validating all blocks\n";
  
  // For each of the blocks in this loop ...
  for (BlockIterator i = l->blocks.begin(),
         e = l->blocks.end(); i != e; ++i) {
    llvm::BasicBlock *sb = *i;
    llvm::errs() << "\t    Validating block " << sb->getName() << " in the new loop\n";    
    
    // Add unseen inner loops.
    Loop *new_loop = check_for_loop(ll, dg, *i, "    ");
    if (new_loop) {
      llvm::errs() << "\t    This block is the start of a sub loop!\n";
      l->sub_loops.push_back(new_loop);
      new_loop->parent_loop = l;
      llvm::errs() << "\t    ... added sub loop to this loop\n";
    }
    
    llvm::errs() << "\t    Mapping " << sb->getName() << " into forest:\n";    
    // Make sure it's connected to the loop forest.
    ll->bb_map.insert(std::make_pair(*i, l));
    ll->print(llvm::errs(), "\t     ");
  }

  // Move child loops around so that they're properly nested.
  fix_loop_nesting(l);

  return l;  
}

void Loops::init(DominanceGraph *dg) {
  llvm::BasicBlock *root = dg->root_node->block;

  llvm::errs() << "========================================================\n";
  llvm::errs() << "** Finding loops starting at " << root->getName()
               << " for function " << root->getParent()->getName() << "\n";
  
  for (llvm::df_iterator<llvm::BasicBlock *> ni = llvm::df_begin(root),
         ne = llvm::df_end(root); ni != ne; ++ni) {
    llvm::BasicBlock *n = *ni;
    llvm::errs() << "    Is there a loop in " << n->getName() << "?\n";
    Loop *l = check_for_loop(this, dg, *ni);
    if (l) {
      llvm::errs() << " ... YES, pushing loop into forest";
      this->top_level_loops.push_back(l);
    } else {
      llvm::errs() << " ... NO, moving to next block";
    }
    llvm::errs() << "\n";
  }
  llvm::errs() << "========================================================\n";
  this->print(llvm::errs(), "");
  llvm::errs() << "========================================================\n";
}

void Loops::print(llvm::raw_ostream &os, std::string prefix) {
  os << prefix << "Loop forest:\n";
  os << prefix << " top level loops: " << top_level_loops.size() << "\n";
  typedef llvm::DenseMap<llvm::BasicBlock *, Loop *>::const_iterator map_iter;
  for (map_iter i = this->bb_map.begin(), e = this->bb_map.end();
       i != e; i++) {
    Loop *loop = i->second;
    
    os << prefix << " block: " << i->first->getName() << " mapped to:\n";
    os << prefix << "  header: " << loop->header()->getName() << "\n";
    llvm::BasicBlock *latch_block = loop->latch();
    if (i->first == latch_block) {
      os << prefix << "  latch block\n";
    }

    for (unsigned j = 0; j < loop->sub_loops.size(); j++) {
      llvm::BasicBlock *sub_loop_header = loop->sub_loops[j]->blocks.front();
      os << prefix << "  sub loops starting at "
         << sub_loop_header->getName() << "\n";
    }
  }
}
