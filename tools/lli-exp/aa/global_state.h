//===- global_state.h - ---------------------------------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#ifndef GLOBAL_STATE_H
#define GLOBAL_STATE_H

#include "constraints.h"
#include "ext.h"
#include "nodes.h"
#include "predicates.h"
#include "structs.h"

#include "llvm/ADT/DenseMap.h"     // for llvm::DenseMap
#include "llvm/GlobalVariable.h"   // for llvm::Type
#include "llvm/Module.h"           // for llvm::Function

#include <string>

struct GlobalType {
  const llvm::Type *type;
  bool is_array;

  GlobalType(const llvm::GlobalVariable *g) {
    type = g->getType()->getContainedType(0);
    while (const llvm::ArrayType *at = llvm::dyn_cast<llvm::ArrayType>(type)) {
      type = at->getElementType();
      is_array = true;
    }
  }
};

class ModuleState {
 public:
  Structs structs;
  ExtInfo ext_info;
  std::string entry_point;

  ModuleState() : entry_point("main") {}
  ModuleState(std::string entry_point) : entry_point(entry_point) {}
};

class IDSet {
 public:
  llvm::DenseMap<u32, u32> cache;

  void add(u32 id, u32 num_fields) {
    cache[id] = num_fields;
  }

  bool lookup(u32 id, u32 *num_fields = 0) {
    llvm::DenseMap<u32, u32>::iterator i = cache.find(id);
    if (i == cache.end()) {
      return false;
    }
    if (num_fields)  {
      *num_fields = i->second;
    }
    return true;
  }
};

class GlobalState {
 public:
  ModuleState *module;
  IDSet done_set;
  Nodes *nodes;
  Constraints *constraints;  
  llvm::DenseSet<const llvm::Value *> addr_taken_args;
  
  GlobalState(ModuleState *module) : module(module) {
    nodes = new Nodes();
    constraints = new Constraints();
  }

  void process_function_signature(const llvm::Function *f);
  void process_global_signature(const llvm::GlobalVariable *g);
  void process_global_value(const llvm::GlobalVariable *g);
};

#endif // end GLOBAL_STATE_H
