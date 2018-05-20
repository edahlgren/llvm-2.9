//===- CallGraphUtils.cpp - Utilities for call graphs ---------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#include "analysis.h"

#include "llvm/Analysis/CallGraph.h"
#include "llvm/Module.h"
#include "llvm/Instructions.h"
#include "llvm/IntrinsicInst.h"
#include "llvm/Support/CallSite.h"
//#include "llvm/Support/Casting.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

llvm::CallGraphNode *get_or_cast_function(FunctionGraph *fg, const llvm::Function *f) {
  llvm::CallGraphNode *&cgn = fg->functions[f];
  if (cgn) {
    return cgn;
  }
  cgn = new llvm::CallGraphNode(const_cast<llvm::Function *>(f));
  return cgn;
}

// These are predicates that help us understand how values in the intermediate
// representation are used.

// user_is_a_caller returns true if the user (of a value) is a function (with
// or without exception handling) and otherwise returns false.
//
// Notes:
//
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
//
bool user_is_a_caller(llvm::Value::use_iterator i) {
  llvm::User *u = *i;
  return (llvm::isa<llvm::CallInst>(u) || llvm::isa<llvm::InvokeInst>(u));
}

// user_calls_this_function returns true iff the user is the caller of the value
// and the value is the callee.
bool user_calls_this_function(llvm::Value::use_iterator i) {
  llvm::User *u = *i;
  return llvm::CallSite(llvm::cast<llvm::Instruction>(u)).isCallee(i);
}

// function_is_undefined 
bool function_is_undefined(llvm::Function *f) {
  return (f->isDeclaration() && !f->isIntrinsic());
}

void link_function_to_graph(FunctionGraph *fg, llvm::Function *f) {
  // Cast the function to a vertex in the graph.
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
  for (llvm::Function::iterator bb = f->begin(), bbe = f->end(); bb != bbe; ++bb) {
    // Search through the function's basic blocks (i.e. guts) looking for
    // calls to other functions.
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

void decorate_graph(llvm::raw_ostream &os) {
  os << "|---------------------------------------------------------------------|\n";
}

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
