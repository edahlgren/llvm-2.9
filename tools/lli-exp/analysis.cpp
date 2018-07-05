//===- analysis.cpp - Module analysis utilities ---------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#include "analysis.h"

#include "llvm/ADT/SmallVector.h"
#include "llvm/Module.h"
#include "llvm/Instructions.h"
#include "llvm/IntrinsicInst.h"
#include "llvm/Support/CallSite.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/CFG.h"
#include "llvm/Support/Debug.h"

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
