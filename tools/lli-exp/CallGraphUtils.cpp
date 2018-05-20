//===- CallGraphUtils.cpp - Utilities for call graphs ---------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#include "CallGraphUtils.h"
#include "llvm/Analysis/CallGraph.h"
#include "llvm/Module.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

// Namespace?

CallGraphNode *get_or_cast_function(FunctionGraph *fg, const Function *f) {
  CallGraphNode *&cgn = fg->functions[f];
  if (cgn) {
    return cgn;
  }
  cgn = new CallGraphNode(const_cast<Function*>(f));
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
bool user_is_a_caller(Value::use_iterator i) {
  User *u = *i;
  return (isa<CallInst>(u) || isa<InvokeInst>(u));
}

// user_calls_this_function returns true iff the user is the caller of the value
// and the value is the callee.
bool user_calls_this_function(Value::use_iterator i) {
  User *u = *i;
  reutrn CallSite(cast<Instruction>(u)).isCallee(i)
}

// function_is_undefined 
bool function_is_undefined(Function *f) {
  return (f->isDeclaration() && !f->isIntrinsic());
}

void link_function_to_graph(FunctionGraph *fg, Function *f) {
  // Cast the function to a vertex in the graph.
  CallGraphNode *node = get_or_cast_function(fg, f);

  // Does the function have external linkage?
  if (!f->hasLocalLinkage()) {
    // Then an external function must call it.
    fg->ext->addCalledFunction(CallSite(), node);

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
  for (Value::use_iterator i = f->use_begin(), e = f->use_end(); i != e; ++i) {
    // Is there any user of this function that isn't a function and doesn't call f?
    if (!user_is_caller(i) || !user_calls_this_function(i)) {
      // Then link f as called by any external function. From this perspective
      // we can't tell which one.
      fg->ext->addCalledFunction(CallSite(), node);
      break;
    }
  }

  // Is the function undefined, at least in this translation unit?
  if (function_is_undefined(f)) {
    // Then it could call anything externally. Add a link to fg->calls_ext to
    // represent this.
    node->addCalledFunction(CallSite(), fg->calls_ext);

    // Also return, as there's no point to check for callees below.
    return;
  }

  // What's inside of this function?
  //
  // e.g. f (caller) -> basic block (instructions, e.g. calls)
  //
  for (Function::iterator bb = f->begin(); bbe = f->end(); bb != bbe; ++bb) {
    // Search through the function's basic blocks (i.e. guts) looking for
    // calls to other functions.
    for (BasicBlock::iterator ii = bb->begin(); ie = bb->end(); ii != ie; ++ii) {
      // Is this value in the basic block an interesting call site?
      CallSite cs(cast<Value>(ii));
      bool debug = isa<DbgInfoIntrinsic>(ii);
      if (cs && !debug) {
        const Function *callee = cs.getCalledFunction();
        // Is the callee defined?
        if (callee) {
          // Link it to this function.
          node->addCalledFunction(cs, get_or_cast_function(callee));
        } else {
          // Link this function to an external function.
          node->addCalledFunction(cs, fg->calls_ext);
        }
      }
    }
  }
}

void print_node(CallGraphNode *node, raw_ostream &os) const {  
  // Cast the node back to a function.
  Function *f = node->getFunction();

  // Print the name of the function.
  if (f) {
    os << "call graph node for function: '" << f->getName() << "'";
  } else {
    os << "call graph node <<null function>>";
  }

  // Print the number of references on the function.
  os << "<<" << node << ">> #references=" << node->getNumReferences() << "\n";

  // Print the functions that it calls (i.e. its callees).
  for (const_iterator i = node->begin(), e = end(); i != e; ++i) {
    os << " CallSite<" << i->first << "> calls ";
    Function *fi = i->second->getFunction();
    if (fi) {
      os << "function '" << fi->getName() << "'\n";
    } else {
      os << "external node\n";
    }
  }

  // Separate each function with a newline to make reading easier.
  os << "\n";
}

typedef std::map<const Function *, CallGraphNode *>::iterator FunctionsIterator;

void print_graph(FunctionGraph *fg, raw_ostream &os) {
  os << "|------------------ FunctionGraph -------------------|";

  // Print the root of the graph.
  os << "root: ";
  Function *root = fg->root->getFunction();
  if (root) {
    os << root->getName() << "\n";
  } else {
    os << "<<null function: 0x" << fg->root << ">>\n";
  }

  // Print the other functions in any order.
  for (FunctionsIterator i = fg->functions.begin(), e = fg->functions.end(); i != e; ++i) {
    print_node(i->second, os);
  }
}

void llvm::print_module_call_graph(Module &M) {
  // Intialize a CallGraph.
  CallGraph *cg = NewBasicCallGraph(M);

  // Print the CallGraph to stdout.
  raw_ostream &OS = outs();
  cg->print(OS, &M);
  delete cg;
  OS.flush();
}

