//===- analysis.cpp - Module analysis utilities ---------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#include "analysis.h"

#include "llvm/ADT/SmallVector.h"
#include "llvm/Analysis/CallGraph.h"
#include "llvm/Module.h"
#include "llvm/Instructions.h"
#include "llvm/IntrinsicInst.h"
#include "llvm/Support/CallSite.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/CFG.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

// ---- Predicates

// user_is_a_caller returns true if the user (of a value) is a function (with
// or without exception handling) and otherwise returns false.
//
// This is useful for finding functions that contain or call values of interest.
static bool user_is_a_caller(llvm::Value::use_iterator i) {
  // CallInst:   This instruction represents a simple function call.
  //             ".. it is used to cause control flow to transfer to a specific
  //             function, with its incoming arguments bound to specified
  //             values. Upon a 'ret' instruction in the called function,
  //             control flow continues with the instruction after the function
  //             call, and the return value of the function is bound to the
  //             result argument."
  //               - docs/LangRef.html#call-instruction
  //
  // InvokeInst: "The 'invoke' instruction causes control to transfer to a
  //             specified function, with the possibility of control flow
  //             transfer to either the 'normal' label or the 'exception' label.
  //             If the callee function returns with the 'ret' instruction,
  //             control flow will return to the 'normal' label. If the callee
  //             (or any indirect callees) returns via the 'resume' instruction
  //             or other exception handling mechanism, control is interrupted
  //             and continued at the nearest 'exception' label. The 'exception'
  //             label is a landing pad for the exception."
  //               - docs/LangRef.html#invoke-instruction
  llvm::User *u = *i;
  return (llvm::isa<llvm::CallInst>(u) || llvm::isa<llvm::InvokeInst>(u));
}

// user_calls_this_function returns true iff the user is the caller of a value
// that is also the callee.
//
// This isn't true for users of functions as parameters or fields (e.g. Linux
// structures full of function pointers).
//
// This is useful for analyzing whether a function is a true caller of another
// function.
static bool user_calls_this_function(llvm::Value::use_iterator i) {
  llvm::User *u = *i;
  return llvm::CallSite(llvm::cast<llvm::Instruction>(u)).isCallee(i);
}

// function_is_undefined returns true iff this function is not a built-in llvm
// intrinsic function (see include/llvm/Intrinsics.td) and has been declared
// but has not been defined.
//
// This is normally the case if the function is in a header that is separate from
// the actual implementation.
//
// This is useful to know before attempting to analyze the contents of a function.
bool function_is_undefined(llvm::Function *f) {
  return (f->isDeclaration() && !f->isIntrinsic());
}

// Check if a branch starting at BasicBlock leads only to target.
static bool leads_only_to(llvm::BasicBlock *branch, llvm::BasicBlock *target) {
  // Does the branch already match the target? Then there's
  // nothing else to do.
  if (branch == target) {
    return true;
  }
  
  // Get the terminator instruction for the branch. It could be a branch
  // or a switch instruction (or maybe even something else) so we use the
  // more general "successor" interface to operate on the chain of blocks.
  llvm::TerminatorInst *term = branch->getTerminator();
  unsigned successors = term->getNumSuccessors();
  assert(successors > 0);

  // Only one destination? This is true for unconditional branches
  // and weird switch instructions with only a default block.
  if (successors == 1) {
      // Then get the destination and check that it's to the target.
      llvm::BasicBlock *end = term->getSuccessor(0);
      // This is another base case.
      return (end == target);
  }

  // There are multiple possible destinations, we need to check each.
  for (unsigned i = 0; i != successors; ++i) {
    llvm::BasicBlock *succ = term->getSuccessor(i);
    if (!leads_only_to(succ, target)) {
      return false;
    }
  }
  
  // If everything checked out then we can return true;
  return true;
}

bool unconditional_path(llvm::Function* f, llvm::BasicBlock *target) {
  // Iterate through the BasicBlocks that would lead to this target.
  for (llvm::pred_iterator pi = llvm::pred_begin(target), e = llvm::pred_end(target); pi != e; ++pi) {
    // Has this predecessor BasicBlock escaped the scope of our function?    
    llvm::BasicBlock *pred = *pi;
    if (pred->getParent() != f) {
      // Then ignore it, we're only concerned with BasicBlocks in this Function.
      //
      // CHECKME: Is this really necessary?
      continue;
    }

    // Then check that each predecessor leads only to the target. This
    // is a recursive function that travels all the way down the branches
    // of pred looking for unconditional terminators.
    if (!leads_only_to(pred, target)) {
      return false;
    }
  }

  // If all predecessor blocks check out, then we're good.
  return true;
}

// ---- Wrappers

// get_or_cast_function finds a f in the FunctionGraph, returning its CallGraphNode
// wrapper, or wraps f in a new CallGraphNode. It does not link f to the FunctionGraph
// in any way.
static llvm::CallGraphNode *get_or_cast_function(FunctionGraph *fg, const llvm::Function *f) {
  llvm::CallGraphNode *&cgn = fg->functions[f];
  if (cgn) {
    return cgn;
  }
  cgn = new llvm::CallGraphNode(const_cast<llvm::Function *>(f));
  return cgn;
}

// ---- Debuggers

static const char *instruction_type(llvm::BasicBlock::iterator i) {
  if (llvm::isa<llvm::BranchInst>(i)) {
    return "branch";
  }
  if (llvm::isa<llvm::SwitchInst>(i)) {
    return "switch";
  }
  if (llvm::isa<llvm::CallInst>(i)) {
    return "call";
  }
  if (llvm::isa<llvm::InvokeInst>(i)) {
    return "invoke";
  }
  if (llvm::isa<llvm::ReturnInst>(i)) {
    return "return";
  }
  if (llvm::isa<llvm::UnwindInst>(i)) {
    return "unwind";
  }
  if (llvm::isa<llvm::UnreachableInst>(i)) {
    return "unreachable";
  }
  if (llvm::isa<llvm::AllocaInst>(i)) {
    return "alloca";
  }
  if (llvm::isa<llvm::LoadInst>(i)) {
    return "load";
  }
  if (llvm::isa<llvm::StoreInst>(i)) {
    return "store";
  }
  if (llvm::isa<llvm::GetElementPtrInst>(i)) {
    return "element-get-ptr";
  }
  if (llvm::isa<llvm::ExtractElementInst>(i)) {
    return "element-extract";
  }
  if (llvm::isa<llvm::InsertElementInst>(i)) {
    return "element-insert";
  }
  if (llvm::isa<llvm::ShuffleVectorInst>(i)) {
    return "shuffle";
  }
  if (llvm::isa<llvm::InsertValueInst>(i)) {
    return "value-insert";
  }
  if (llvm::isa<llvm::ExtractValueInst>(i)) {
    return "value-extract";
  }
  if (llvm::isa<llvm::BinaryOperator>(i)) {
    return "binary";
  }
  if (llvm::isa<llvm::CastInst>(i)) {
    return "cast";
  }
  if (llvm::isa<llvm::CmpInst>(i)) {
    return "cmp";
  }
  if (llvm::isa<llvm::IndirectBrInst>(i)) {
    return "break-indirect";
  }
  if (llvm::isa<llvm::PHINode>(i)) {
    return "phi";
  }
  if (llvm::isa<llvm::SelectInst>(i)) {
    return "select";
  }
  return "unknown";
}


// ---- Call graph

// link_function_to_graph links f as a node in the FunctionGraph by connecting
// it to its callers and its callees.
void link_function_to_graph(FunctionGraph *fg, llvm::Function *f) {
  // Cast the function to a node that can be linked in the graph.
  llvm::CallGraphNode *node = get_or_cast_function(fg, f);

  // Does the function have external linkage?
  if (!f->hasLocalLinkage()) {
    // Then an external function must call it.
    fg->ext->addCalledFunction(llvm::CallSite(), node);

    // Then entry point has external linkage, are we there?
    if (f->getName() == "main") {
      // Did we already find "main"?
      if (fg->root) {
        // We can't pick both, so claim that root is any external function.
        fg->root = fg->ext;
      } else {
        // Keep track of "main".
        fg->root = node;
      }
    }
  }

  // What calls this function?
  //
  // e.g. use_iterator (caller) -> f (callee)
  //
  for (llvm::Value::use_iterator i = f->use_begin(), e = f->use_end(); i != e; ++i) {
    // Is there any user of this function that isn't a function and doesn't call f?
    if (!user_is_a_caller(i) || !user_calls_this_function(i)) {
      // Then link f as called by any external function. From this perspective
      // we can't tell which one.
      fg->ext->addCalledFunction(llvm::CallSite(), node);
      break;
    }
  }

  // Is the function undefined, at least in this translation unit?
  if (function_is_undefined(f)) {
    // Then it could call anything externally. Add a link to fg->calls_ext to
    // represent this.
    node->addCalledFunction(llvm::CallSite(), fg->calls_ext);

    // Also return, as there's no point to check for callees below.
    return;
  }

  // What's inside of this function?
  //
  // e.g. f (caller) -> basic block (instructions, e.g. calls)
  //
  // Search through the function's basic blocks (i.e. guts) looking for calls
  // to other functions.
  for (llvm::Function::iterator bb = f->begin(), bbe = f->end(); bb != bbe; ++bb) {
    for (llvm::BasicBlock::iterator ii = bb->begin(), ie = bb->end(); ii != ie; ++ii) {
      // Is this value in the basic block an interesting call site?
      llvm::CallSite cs(llvm::cast<llvm::Value>(ii));
      bool debug = llvm::isa<llvm::DbgInfoIntrinsic>(ii);
      if (cs && !debug) {
        const llvm::Function *callee = cs.getCalledFunction();
        // Is the callee defined?
        if (callee) {
          // Link it to this function.
          node->addCalledFunction(cs, get_or_cast_function(fg, callee));
        } else {
          // Link this function to an external function.
          node->addCalledFunction(cs, fg->calls_ext);
        }
      }
    }
  }
}

// ---- Dominance graph

// Iterate through the children of bb and assign all blocks a number
// in depth-first search order. This makes more sense as a recursive
// algorithm but that might blow the stack, so we use a more obtuse,
// iterative one.
static unsigned build_dominance_info_records(DominanceGraph &dg, llvm::BasicBlock *bb, unsigned n) {
  llvm::SmallVector<std::pair<llvm::BasicBlock *,
                              llvm::BasicBlock::ChildIteratorType>, 32> worklist;

  worklist.push_back(std::make_pair(bb, llvm::BasicBlock::child_begin(bb)));

  while (!worklist.empty()) {
    llvm::BasicBlock *bb = worklist.back().first;
    llvm::BasicBlock::ChildIteratorType next_succ = worklist.back().second;

    DominanceInfoRec &bbinfo = dg.info[bb];

    if (next_succ == llvm::BasicBlock::child_begin(bb)) {
      bbinfo.dfsnum = bbinfo.semi = ++n;
      bbinfo.label = bb;
      dg.vertex.push_back(bb);
    }

    unsigned bb_dfs_num = bbinfo.dfs_num;

    if (next_succ == llvm::BasicBlock::child_end(bb)) {
      worklist.pop_back();
      continue;
    }

    ++worklist.back().second;

    llvm::BasicBlock *succ = *next_succ;
    DominanceInfoRec &succ_vinfo = dg.info[succ];
    if (succ_vinfo.semi == 0) {
      succ_vinfo.parent = bb_dfs_num;
      worklist.push_back(std::make_pair(succ, llvm::BasicBlock::child_begin(succ)));
    }
  }

  return n;
}

// Return a block with the minimum semidominator number of all blocks
// in between the most distance ancestor of bb to be processed and bb.
//
// But if bb hasn't been processed at all, then we "skip" it by just
// returning bb itself.
static llvm::BasicBlock *get_minimum_semidominator(DominanceGraph &dg, llvm::BasicBlock *bb,
                                                   unsigned last_linked) {
  DominanceInfoRec &bbinfo = dg.info[bb];

  if (bbinfo.dfs_num < last_linked) {
    return bb;
  }

  llvm::SmallVector<llvm::BasicBlock *, 32> work;
  llvm::SmallPtrSet<llvm::BasicBlock *, 32> visited;

  if (bbinfo.parent >= last_linked) {
    work.push_back(bb);
  }

  while (!work.empty()) {
    llvm::BasicBlock *v = work.back();
    DominanceInfoRec &v_info = dg.info[v];
    llvm::BasicBlock *v_parent = dg.vertex[v_info.parent];

    if (visited.insert(v_parent) && v_info.parent >= last_linked) {
      work.push_back(v_parent);
      continue;
    }

    work.pop_back();

    if (v_info.parent < last_linked) {
      continue;
    }

    DominanceInfoRec &v_parent_info = dg.info[v_parent];
    llvm::BasicBlock *v_parent_label = v_parent_info.label;
    llvm::BasicBlock *v_label = v_info.label;

    if (dg.info[v_parent_label].semi < dg.info[v_label].semi) {
      v_info.label = v_parent_label;
    }
    
    v_info.parent = v_parent_info.parent;
  }

  return bbinfo.label;
}

// Get the immediate dominator of this block. This should only be called
// during dominance_build, otherwise idoms will be empty. There are other
// methods to get the immediate dominator from the nodes we built.
inline llvm::BasicBlock *get_idom(DominanceGraph &dg, llvm::BasicBlock *bb) const {
  typename llvm::DenseMap<llvm::BasicBlock*, llvm::BasicBlock*>::const_iterator i =
    dg.idoms.find(bb);
  return i != dg.idoms.end() ? i->second : 0;
}

// Get or create a node for this block.
DominanceNode *get_node_for_block(DominanceGraph &dg, llvm::BasicBlock *block) {
  typename node_map_type::iterator i = dg.nodes.find(block);
  if (i != dg.nodes.end() && i->second)
    return i->second;

  // Haven't calculated this node yet?  Get or calculate the node for the
  // immediate dominator.
  llvm::BasicBlock *idom = get_idom(dg, block);
  assert(idom || dg.nodes[NULL]);   
  DominanceNode *idom_node = get_node_for_block(dg, idom);
    
  // Add a new tree node for this BasicBlock, and link it as a child of
  // IDomNode
  DominanceNode *child = new DominanceNode(block, idom_node);
  return dg.nodes[block] = idom_node->add_child(child);
}

// This function works in 5 steps:
//
//   Step 1.
//     Number the blocks in depth-first order and initialize
//     variables used in future steps.
//
//   Step 2.
//     Implicitly define the immediate dominator of vertices.
//
//   Step 3.
//     Calculate "semidominators" of all vertices.
//
//   Step 4.
//     Explicitly define the immediate dominator of vertices.
//
//   Step 5.
//     Build nodes from the immediate dominators found in
//     Step 4 and destroy any intermediate state.
void DominanceGraph::init(llvm::BasicBlock *root) {
  // This is a placeholder for the mapping of a BasicBlock at the
  // depth-first search number 0. It points to a NULL block so that
  // numbering of real blocks can start at 1. See dominance_dfs_num
  // for more details.
  this.vertex.push_back(NULL);
  
  // Step 1.
  //   Do a depth-first search through the root, adding blocks to the
  //   dg->vertex list in that order and assigning the same number to
  //   the block's info.
  //
  // When finished, n will be the highest number assigned to a block and also
  // the number of blocks.
  //
  // This populates dfsnum, label, and parent in the info, but not the
  // semidominator number. That needs to be computed below.
  unsigned n = 0;
  n = build_dominance_info_records(this, root, n);

  // These buckets map the dfs number of a block v to the dfs number of another
  // block w, where v is a semidominator of w.
  //
  // Note: Since v can be the semidominator of more than one block, the original
  // Lengauer-Tarjan algorithm uses vertex set to repesent each bucket. Instead
  // we make it so that bucket[i] always contains the vertex of the *next* element
  // to be process for block i. In this way, a bucket acts like an iterator.
  llvm::SmallVector<unsigned, 32> buckets;
  
  // Skip the first element, it's a placeholder.
  buckets.resize(n + 1);
  for (unsigned i = 1; i <= n; ++i) {
    // Start by claiming that block i is a semidominator of itself, and is the
    // first block to process.
    buckets[i] = i;
  }

  // Step 2.
  //   Implicitly define the immediate dominator of vertices.
  //
  // Starting at n, process blocks in decreasing dfs order, stopping before
  // the first two blocks (the first is a placeholder and the second is the root).
  for (unsigned i = n; i >= 2; --i) {
    // Get the block at this dfs number.
    llvm::BasicBlock *w = this.vertex[i];

    // Get the scratch data for this block, where we will store all the
    // temporary state about this block as we find its immediate dominator.
    // Note that the [] operator of llvm::DenseMap inserts a freshly constructed
    // DominanceInfoRec if w is not mapped. See FindAndConstruct in DenseMap.
    DominanceInfoRec winfo = this.info[w];

    // Iterate through the blocks that this block semidominates until we find
    // ourselves back at this block. Note that when this loop begins, since we
    // assigned all buckets to point to themselves, this will be initially
    // skipped. We'll fall into this logic once we encouter a block that was
    // processed as a semidominator, see the last step in this loop.
    for (unsigned j = i; buckets[j] != i; j = buckets[j]) {
      // Get the semidominated block.
      llvm::BasicBlock *v = this.vertex[buckets[j]];
      
      // Get the block between the root and v with the minimum semidominator,
      // or just v if we haven't processed v yet.
      llvm::BasicBlock *u = get_minimum_semidominator(this, v, i + 1);

      // Does the semidominator of u come before w in the depth-first search
      // ordering?
      //
      // This logic is a big fishy ... what exactly is going on here? It would
      // be nice to do a little illustration of the algorithm in an animated
      // graphic to show how things change.
      if (this.info[u].semi < i) {
        // Then v and u have the same dominator.
        this.idoms[v] = u;
      } else {
        // Otherwise w is both a semidominator of v and its immediate dominator.
        this.idoms[v] = w;
      }
    }

    // Step 3.
    //   Calculate the semidominators of this block and fill in the buckets.
    //
    // This is what powers the computation above, which traverses the buckets.
    // For some reason (??) we want to run the code that implicitly finds the
    // dominators first.
    //
    // Start by assigning the semidominator of this block to be its direct
    // parent. This value was computed by dominance_dfs_num.
    winfo.semi = winfo.parent;

    // Iterate through blocks that point *to* this block. They might be outside
    // the scope of the root block, and if so, we shouldn't expect to find them
    // in the vertex or info map.
    typedef GraphTraits<Inverse<llvm::BasicBlock *> > InverseTraits;
    typedef InverseTraits::ChildIteratorType InverseIterator;
    for (typename InverseIterator ci = InverseTraits::child_begin(w),
           e = InverseTraits::child_end(w); ci != e; ++ci) {
      llvm::BasicBlock *bb = *ci;
      // Is this precedessor outside the scope of this analysis?
      if (!this.info.count(bb)) {
        // Then skip it.
        continue;
      }

      // Otherwise like above get the block with the lowest semidominator number
      // between the root and bb.
      llvm::BasicBlock *u = get_minimum_semidominator(this, bb, i + 1);      

      // Is the minimum semidominator number of u even less than this block's
      // semidominator (default above is its parent's dfs number)?
      unsigned semiu = this.info[u].semi;
      if (semiu < winfo.semi) {
        // Then use the lower semidominator.
        winfo.semi = semiu;
      }
    }

    // Is the semidominator of this block still its parent?
    if (winfo.semi == winfo.parent) {
      // Then implicitly make this block's immediate dominator its parent.
      this.idoms[w] = this.vertex[winfo.parent];
    } else {
      // Otherwise push this block and its semidominator into their respective
      // buckets so that they map to each other. This makes the current
      // semidominator the next value to be processed for this block.
      buckets[i] = buckets[winfo.semi];
      buckets[winfo.semi] = i;
    }
  }

  // Now process the root.
  //
  // Go through blocks that are semidominated by the root.
  unsigned root_i = 1;
  for (unsigned j = root_i; buckets[j] != root_i; j = buckets[j]) {
    // Get a pointer to the semidominated block.
    unsigned semidominated_i = buckets[j];
    llvm::BasicBlock *v = this.vertex[semidominated_i];
    
    // Claim that they're also immediately dominated by the root.
    this.idoms[v] = root;
  }

  // Step 4.
  //   Explicitly define the immediate dominator of vertices.
  //
  // Like above, starting at the last dfs number, process blocks in decreasing
  // dfs order, stopping before the root.
  for (unsigned i = 2; i <= n; ++i) {
    // Get the block at this dfs number.
    llvm::BasicBlock *w = this.vertex[i];
    
    // Get the implicit immediate dominator of this block.
    llvm::BasicBlock *&widom = this.idoms[w];

    // Get the semidominator of this block.
    unsigned semiw = this.info[w].semi;

    // Is the implicit immediate dominator not its semidominator?
    if (widom != this.vertex[semiw]) {
      // Then reassign this block's immediate dominator to be the same as
      // the immediate dominator of its old immediate dominator.
      widom = this.idoms[widom];
    }
  }

  // Organize idoms and info into into DominanceNodes.
  //
  // First, make the root node.
  dg->nodes[root] = this.root_node = new DominanceNode(root, 0);

  // Then make children of root.
  for (unsigned i = 2; i <= n; ++i) {
    // Get the block at this dfs number.
    llvm::BasicBlock *w = this.vertex[i];

    // Have we already inserted this node?
    llvm::BasicBlock *bbnode = this.nodes[w];
    if (bbnode) {
      // Skip it.
      continue;
    }

    // Get the immediate dominator of this block.
    llvm::BasicBlock *immdom = get_idom(this, w);
    assert(immdom || this.nodes[NULL]);

    // Lookup the node for the immediate dominator of this block
    // or create one if it doesn't exist.
    DominanceNode *idomnode = get_node_for_block(this, immdom);

    // Create a node for this block, setting the immediate dominator
    // node as its dominator.
    DominanceNode *child = new DominanceGraph(w, idomnode);

    // Finally add this node as the child of its dominator and the
    // entry at this block.
    this.nodes[w] = idomnode->add_child(child);
  }

  // Free the scratch space. It should probably be made clear that these
  // fields are useless after we actually build the graph.
  this.idoms.clear();
  this.info.clear();
  std::vector<llvm::BasicBlock *>().swap(this.vertex);

  // Finally assign depth-first search numbers to the dominance graph
  // itself. These numbers hang off the DominanceNodes we inserted
  // above. It also updates the state of the graph to support optimizations
  // based on these numbers, until a new node is added, which then makes
  // these dfs numbers stale.
  this.rebuild_numbers();
}

// See comments in analysis.h.
void DominanceGraph::rebuild_graph(llvm::BasicBlock *new_root) {
  // Wipe the entire DominanceGraph clean. The same function is called
  // when the DominanceGraph is destroyed.
  this.reset();
  this.init(new_root);
}

// See comments in analysis.h.
void DominanceGraph::rebuild_numbers() {
    DominanceNode *r = root_node;
    if (!r) {
      return;
    }

    unsigned dfs_num = 0;   
    llvm::SmallVector<std::pair<DominanceNode*,
                                typename DominanceNode::iterator>, 32> workstack;

    workstack.push_back(std::make_pair(r, r->begin()));
    r->dfs_num_in = dfs_num++;

    while (!workstack.empty()) {
      DominanceNode *node = workstack.back().first;
      typename DominanceNode::iterator it = workstack.back().second;

      if (it == node->end()) {
        node->dfs_num_out = dfs_num++;
        workstack.pop_back();
      } else {
        DominanceNode *child = *it;
        ++workstack.back().second;

        workstack.push_back(std::make_pair(child, child->begin()));
        child->dfs_num_in = dfs_num++;
      }
    }

    slow_queries = 0;
    dfs_info_valid = true;
}
