//===- cfg.cpp - Control Flow utilities -----------------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#include "analysis.h"
#include "cfg.h"
#include "write_dot.h"

#include "llvm/Module.h"
#include "llvm/IntrinsicInst.h"
#include "llvm/Support/raw_ostream.h"

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
