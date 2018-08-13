//===- block_state.h - ----------------------------------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#ifndef BLOCK_STATE_H
#define BLOCK_STATE_H

#include "int.h"  // for u32

#include "llvm/Module.h"           // for llvm::BasicBlock

#include <map>    // for std::map

class BlockState {
public:
  const llvm::BasicBlock *block;
  u32 position;
  bool contains_call;
  u32 constraints_sz;

  BlockState(const llvm::BasicBlock *bb, u32 position) :
    block(bb),
    position(position),
    contains_call(false),
    constraints_sz(0) {}
};

class BlockCache {
public:
  std::map<const llvm::BasicBlock *, u32> cache;
  
  u32 lookup(const llvm::BasicBlock *block) {
    std::map<const llvm::BasicBlock *, u32>::iterator it =
      cache.find(block);
    
    if (it != cache.end()) {
      assert(it->second);
      return it->second;
    }
    
    return 0;
  }

  void insert(const llvm::BasicBlock *block, u32 index) {
    cache[block] = index;
  }
};

#endif // end BLOCK_STATE_H
