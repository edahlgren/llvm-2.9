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
bool user_is_a_caller(llvm::Value::use_iterator i) {
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
bool user_calls_this_function(llvm::Value::use_iterator i) {
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

// ---- Wrappers

// get_or_cast_function finds a f in the FunctionGraph, returning its CallGraphNode
// wrapper, or wraps f in a new CallGraphNode. It does not link f to the FunctionGraph
// in any way.
llvm::CallGraphNode *get_or_cast_function(FunctionGraph *fg, const llvm::Function *f) {
  llvm::CallGraphNode *&cgn = fg->functions[f];
  if (cgn) {
    return cgn;
  }
  cgn = new llvm::CallGraphNode(const_cast<llvm::Function *>(f));
  return cgn;
}

const char *instruction_type(llvm::BasicBlock::iterator i) {
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

// ---- Public interface

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

void print_node(llvm::CallGraphNode *node, llvm::raw_ostream &os) {  
  // Cast the node back to a function.
  llvm::Function *f = node->getFunction();

  // Print the name of the function.
  if (f) {
    os << "  call graph node for function: '" << f->getName() << "'";
  } else {
    os << "  call graph node <<null function>>";
  }

  // Print the number of references on the function.
  os << "<<" << node << ">> #references=" << node->getNumReferences() << "\n";

  // Print the functions that it calls (i.e. its callees).
  for (llvm::CallGraphNode::const_iterator i = node->begin(), e = node->end(); i != e; ++i) {
    os << "    CallSite<" << i->first << "> calls ";
    llvm::Function *fi = i->second->getFunction();
    if (fi) {
      os << "function '" << fi->getName() << "'\n";
    } else {
      os << "external node\n";
    }
  }

  // Separate each function with a newline to make reading easier.
  os << "\n";
}

// decorate_graph adds a border to a printed graph.
void decorate_graph(llvm::raw_ostream &os) {
  os << "|---------------------------------------------------------------------|\n";
}

// print_graph traverses the graph, first to the root, and then to nodes in
// a consistent order, functions with the lowest address in the intermediate
// representation first.
void print_graph(FunctionGraph *fg, llvm::raw_ostream &os) {
  // Mark the beginning of the graph.
  decorate_graph(os);
  
  // Make the root easily readable.
  os << "\n";

  // Print the root of the graph.
  os << "  root: ";
  llvm::Function *root = fg->root->getFunction();
  if (root) {
    os << root->getName() << "\n";
  } else {
    os << "  <<null function: 0x" << fg->root << ">>\n";
  }

  // Separate root from function analysis.
  os << "\n";

  // Print the other functions in any order.
  for (FunctionsMap::iterator i = fg->functions.begin(), e = fg->functions.end(); i != e; ++i) {
    print_node(i->second, os);
  }

  // Mark the end of the graph.
  decorate_graph(os);
}

void print_module_call_graph(llvm::Module &m) {
  // Intialize a CallGraph.
  llvm::CallGraph *cg = NewBasicCallGraph(m);

  // Print the CallGraph to stdout.
  llvm::raw_ostream &os = llvm::outs();
  cg->print(os, &m);

  // Cleanup.
  delete cg;

  // Flush the stdout.
  os.flush();
}
