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

static unsigned dominance_dfs_num(DominanceGraph *dg, llvm::BasicBlock *bb, unsigned n) {
  bool is_child_of_artificial_exit = (n != 0);

  llvm::SmallVector<std::pair<llvm::BasicBlock *,
                              llvm::BasicBlock::ChildIteratorType>, 32> worklist;

  worklist.push_back(std::make_pair(bb, llvm::BasicBlock::child_begin(bb)));

  while (!worklist.empty()) {
    llvm::BasicBlock *bb = worklist.back().first;
    llvm::BasicBlock::ChildIteratorType next_succ = worklist.back().second;

    DominanceInfoRec &bbinfo = dg->info[bb];

    if (next_succ == llvm::BasicBlock::child_begin(bb)) {
      bbinfo.dfsnum = bbinfo.semi = ++n;
      bbinfo.label = bb;

      dg->vertex.push_back(bb);

      if (is_child_of_artificial_exit) {
        bbinfo.parent = 1;
      }

      is_child_of_artificial_exit = false;
    }

    unsigned bb_dfs_num = bbinfo.dfs_num;

    if (next_succ == llvm::BasicBlock::child_end(bb)) {
      worklist.pop_back();
      continue;
    }

    ++worklist.back().second;

    llvm::BasicBlock *succ = *next_succ;
    DominanceInfoRec &succ_vinfo = dg->info[succ];
    if (succ_vinfo.semi == 0) {
      succ_vinfo.parent = bb_dfs_num;
      worklist.push_back(std::make_pair(succ, llvm::BasicBlock::child_begin(succ)));
    }
  }

  return n;
}

static llvm::BasicBlock *evaluate_dominance(DominanceGraph *dg, llvm::BasicBlock *bb,
                                     unsigned last_linked) {
  DominanceInfoRec &bbinfo = dg->info[bb];

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
    DominanceInfoRec &v_info = dg->info[v];
    llvm::BasicBlock *v_parent = dg->vertex[v_info.parent];

    if (visited.insert(v_parent) && v_info.parent >= last_linked) {
      work.push_back(v_parent);
      continue;
    }

    work.pop_back();

    if (v_info.parent < last_linked) {
      continue;
    }

    DominanceInfoRec &v_parent_info = dg->info[v_parent];
    llvm::BasicBlock *v_parent_label = v_parent_info.label;
    llvm::BasicBlock *v_label = v_info.label;

    if (dg->info[v_parent_label].semi < dg->info[v_label].semi) {
      v_info.label = v_parent_label;
    }
    
    v_info.parent = v_parent_info.parent;
  }

  return bbinfo.label;
}

//   Based on:
//     A Fast Algorithm for Finding Dominators in a Flowgraph
//     T. Lengauer & R. Tarjan, ACM TOPLAS July 1979, pgs 121-141.
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
static void do_dominance_calculate(DominanceGraph *dg, llvm::Function *f) {
  unsigned n = 0;
  bool multiple_roots = (dg->roots.size() > 1);

  if (multiple_roots) {
    // What does it mean to index into dt->info using NULL?
    DominanceInfoRec bbinfo = dt->info[NULL];
    bbinfo.dfsnum = bbinfo.semi = ++n;
    // Why is the label explicitly set to NULL here?
    bbinfo.label = NULL;

    // Why is it OK to let bbinfo fall out of scope here?

    // Is this the same as dg->vertex.push_back(0), like in
    // recalculate_dominance?
    dt->vertex.push_back(NULL);
  }

  // Step 1.
  for (unsigned i = 0, e = static_cast<unsigned>(dg->roots.size());
       i != e; ++i) {
    n = dominance_dfs_num(dg, dg->roots[i], n);
  }

  // What is this doing?
  multiple_roots |= (dt->is_post_dominator() && n != f->size());

  // What are these buckets for?
  llvm::SmallVector<unsigned, 32> buckets;
  buckets.resize(n + 1);
  for (unsigned i = 1; i <= n; ++i) {
    buckets[i] = i;
  }

  for (unsigned i = n; i >= 2; --i) {
    llvm::BasicBlock *w = dg->vertex[i];
    DominanceInfoRec winfo = dg->info[w];

    // Step 2.
    for (unsigned j = i; buckets[j] != i; j = buckets[j]) {
      llvm::BasicBlock *v = dg->vertex[buckets[j]];
      llvm::BasicBlock *u = evaluate_dominance(dg, v, i + 1);
      dg->idoms[v] = dg->info[u].semi < i ? u : w;
    }

    // Step 3.
    winfo.semi = winfo.parent;
    typedef GraphTraits<Inverse<llvm::BasicBlock *> > InverseTraits;
    typedef InverseTraits::ChildIteratorType InverseIterator;
    for (typename InverseIterator ci = InverseTraits::child_begin(w),
           e = InverseTraits::child_end(w); ci != e; ++ci) {
      llvm::BasicBlock *bb = *ci;
      if (dg->info.count(bb)) {
        llvm::BasicBlock *u = evaluate_dominance(dg, bb, i + 1);
        unsigned semiu = dg->info[u].semi;
        if (semiu < winfo.semi) {
          winfo.semi = semiu;
        }
      }
    }

    // What is this doing?
    if (winfo.semi == winfo.parent) {
      dg->idoms[w] = dg->vertex[winfo.parent];
    } else {
      buckets[i] = buckets[winfo.semi];
      buckets[winfo.semi] = i;
    }
  }
  if (n >= 1) {
    llvm::BasicBlock *root = dg->vertex[1];
    for (unsigned j = 1; buckets[j] != 1; j = buckets[j]) {
      llvm::BasicBlock *v = dg->vertex[buckets[j]];
      dg->idoms[v] = root;
    }
  }

  // Step 4.
  for (unsigned i = 2; i <= n; ++i) {
    llvm::BasicBlock *w = dg->vertex[i];
    llvm::BasicBlock *&widom = dg->idoms[w];
    unsigned semiw = dg->info[w].semi;
    if (widom != dg->vertex[semi]) {
      widom = dg->idoms[widom];
    }
  }

  if (dg->roots.empty()) {
    return;
  }

  llvm::BasicBlock *root = !multiple_roots ? dg->roots[0] : 0;
  dg->nodes[root] = dg->root_node =
    new DominanceGraph(root, 0);

  for (unsigned i = 2; i <= n; ++i) {
    llvm::BasicBlock *w = dg->vertex[i];

    llvm::BasicBlock *bbnode = dg->nodes[w];
    if (bbnode) {
      continue;
    }

    llvm::BasicBlock *immdom = dg->get_idom(w);
    assert(immdom || dg->nodes[NULL]);

    llvm::BasicBlock *idomnode = dg->get_node_for_block(immdom);
    DominanceGraph *child = new DominanceGraph(w, idomnode);
    dg->nodes[w] = idomnode->add_child(child);
  }

  dg->idoms.clear();
  dg->info.clear();
  std::vector<llvm::BasicBlock *>().swap(dg->vertex);

  dg->update_dfs_numbers();
}

void dominance_calculate(DominanceGraph *dg, llvm::Function *f, bool post = false) {
  dg->reset();
  dg->vertex.push_back(0);

  if (post) {
    for (llvm::Function::iterator i = f->begin(); e = f->end(); i != e; ++i) {
      if (std::distance(GraphTraits<Function *>::child_begin(i),
                        GraphTraits<Function *>::child_end(i)) == 0) {
        dg->roots.push_back(i);
      }

      dg->idoms[i] = 0;
      dg->nodes[i] = 0;
    }
  } else {
    llvm::BasicBlock *head = f->front();
    dg->roots.push_back(head);
    dg->idoms[head] = 0;
    dg->nodes[head] = 0;
  }

  do_dominance_calculate(dg, f);
}
