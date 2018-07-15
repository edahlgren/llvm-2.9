
enum AliasResult {
  NoAlias = 0,        // No dependencies.
  MayAlias,           // Anything goes.
  PartialAlias,       // Pointers differ, but pointees overlap.
  MustAlias           // Pointers are equal.
};

struct MemLoc {
  const llvm::Value *ptr;
  uint64_t size;
};

class BasicAliasData {
  const llvm::TargetData *td;
  llvm::SmallPtrSet<const llvm::Value*, 16> Visited;
};

AliasResult basic_alias(BasicAliasData *ad, MemLoc &memA, MemLoc &memB);
