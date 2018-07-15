#include "alias.h"

static const llvm::Function *function_containing_value(const llvm::Value *v) {
  if (const llvm::Instruction *inst = llvm::dyn_cast<llvm::Instruction>(v))
    return inst->getParent()->getParent();

  if (const llvm::Argument *arg = dyn_cast<llvm::Argument>(v))
    return arg->getParent();

  return NULL;
}

static bool not_interprocedural(const llvm::Value *v1, const llvm::Value *v2) {
  const Function *f1 = function_containing_value(v1);
  const Function *f2 = function_containing_value(v2);

  return !f1 || !f2 || f1 == f2;
}


// Return the pointer packed inside v, if v is a getElementPtr instruction,
// a bitcast instruction, or a global alias. Otherwise just return v
// unchanged.
//
// Note that the original function, GetUnderlyingObject also simplified
// instructions, which seems really weird for anything other than an alias
// type of instruction. Maybe this was to make the instructions easier to
// compare, but why would we have a pointer to something like an "and" or
// and "add" instruction anyways?
//
// Maybe the idea is that you can do things like this:
//
//   int *b = &a++;
//
// but you can't do constant folding over that, because a is underspecified.
//
// So I removed the instruction simplifying because it seemed dumb. But it's
// possible that I'm missing something really important here ...
static llvm::Value *unpack_pointer(Value *v) {
  while (true) {
    assert(v->getType()->isPointerTy());
    
    if (llvm::GEPOperator *gep = dyn_cast<GEPOperator>(v)) {
      v = gep->getPointerOperand();
    } else if (Operator::getOpcode(v) == Instruction::BitCast) {
      v = cast<Operator>(v)->getOperand(0);
    } else if (GlobalAlias *ga = dyn_cast<GlobalAlias>(v)) {
      if (ga->mayBeOverridden())
      return v;
      v = ga->getAliasee();
    } else {
      return v;
    }
  }
}

class metavalue {
public:
  metavalue(Value *v) : v(v) {}
  
  Value *v;

  void reset(Value *new_v) {
    v = new_v;
  }

  bool is_pointer() {
    return v->getType()->isPointerTy();
  }

  bool is_const_pointer() {
    return llvm::isa<llvm::Constant>(v);    
  }

  bool is_const_null_pointer(int in_address_space = 0) {
    const llvm::ConstantPointerNull *cpn = llvm::dyn_cast<llvm::ConstantPointerNull(v);
    return (cpn->getType()->getAddressSpace() == in_address_space)
  }

  bool is_known_object() {
    if (llvm::isa<llvm::AllocaInst>(v))
      return true;
    if (llmv::isa<llvm::GlobalValue>(v) && !llvm::isa<llvm::GlobalAlias>(v))
      return true;
    if (llvm::isNoAliasCall(v))
      return true;
    if (const llvm::Argument *a = llvm::dyn_cast<llvm::Argument>(v))
      return a->hasNoAliasAttr() || a->hasByValAttr();
    return false;
  }

  bool is_argument() {
    return llvm::isa<llvm::Argument>(v);
  }

  bool is_alloc_inst() {
    return llvm::isa<llvm::AllocaInst>(v);
  }

  bool is_noalias_call() {
    if (llvm::isa<llvm::CallInst>(V) || llvm::isa<llvm::InvokeInst>(v))
      return llvm::ImmutableCallSite(llvm::cast<llvm::Instruction>(v))
        .paramHasAttr(0, llvm::Attribute::NoAlias);
    return false;
  }

  
};

AliasResult basic_alias(BasicAliasData *ad, MemLoc &memA, MemLoc &memB) {
  assert(not_interprocedural(memA.ptr, memB.ptr) && "Basic Analysis isn't interprocedural");

  // Step 1: Check for obvious (lack of) aliasing.
  
  // Rule 1:
  //
  // An empty memory reference can't alias with anything else.
  if (memA.size == 0 || memB.size == 0) {
    return NoAlias;
  }

  // Rule 2:
  //
  // Uncasted, trivially equal pointers always alias each other.
  const Value *valueA = memA->stripPointerCasts();
  const Value *valueB = memB->stripPointerCasts();
  if (valueA == valueB) {
    return MustAlias;
  }

  // Wrap the pointers in convenience methods.
  metavalue ma(valueA);
  metavalue mb(valueb);

  // Rule 3:
  //
  // Non-pointers cannot alias each other.
  //
  // (Sometimes non-pointers are simply called scalars, but they refer
  // to everything from integers, to arrays, to functions).
  if (!ma.is_pointer() || !mb.is_pointer()) {
    return NoAlias;    
  }

  // Step 2: Perform a "deep" equality check on the pointers.

  // Unpack aliasing instructions like getElementPtr, GlobalAlias, and
  // BitCast, so that we can see what the underlying pointer looks like.
  a.reset(unpack_pointer(valueA));
  b.reset(unpack_pointer(valueB));

  // Rule 4:
  //
  // A const null pointer can't alias with any other pointer because it
  // doesn't point to an object.
  if (ma.is_const_null_pointer() || mb.is_const_null_pointer()) {
    return NoAlias;
  }

  if (ma.v != mb.v) {
    // Rule 5:
    //
    // Pointers don't alias if their underlying objects are trivially
    // different.
    //
    // ?? I still don't understand how we're treating pointers and objects
    //    similarly like this. Is v a pointer or an object? It's really confusing.
    if (ma.is_known_object() && mb.is_known_object()) {
      // Then they point to two different objects. So memA and memB can't alias.
      return NoAlias;
    }

    // Rule 6:
    //
    // Pointers to non-const known objects don't alias with pointers to anything
    // that's const.
    if (!ma.is_const() && ma.is_known_object() && mb.is_const() ||
        !mb.is_const() && mb.is_known_object() && ma.is_const()) {
      return NoAlias;
    }

    // Rule 7:
    //
    // Pointers to function arguments don't alias with 
    
    
  }

}
