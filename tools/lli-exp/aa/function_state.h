//===- function_state.h - ----------------------------------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#ifndef FUNCTION_STATE_H
#define FUNCTION_STATE_H

#include "block_state.h"
#include "constraints.h"
#include "global_state.h"
#include "nodes.h"
#include "structs.h"

#include "llvm/Instructions.h"     // for llvm::CallInst, llvm::InvokeInst,
                                   //     llvm::CmpInst, etc.
#include "llvm/Module.h"           // for llvm::Function, llvm::BasicBlock

#include <map>    // for std::map

#define PARENT_ENTRY_BLOCK MAX_U32

struct MutableFunctionState {
  bool owns_nodes;
  Nodes *nodes;

  bool owns_constraints;
  Constraints *constraints;

  bool owns_constraint_graph;
  ConstraintGraph *constraint_graph;

  MutableFunctionState(Nodes *nodes,
                       Constraints *constraints,
                       ConstraintGraph *constraint_graph)
    : owns_nodes(true),
      nodes(nodes),
      owns_constraints(true),
      constraints(constraints),
      owns_constraint_graph(true),
      constraint_graph(constraint_graph) {}

  ~MutableFunctionState() {
    if (owns_nodes)
      delete nodes;

    if (owns_constraints)
      delete constraints;

    if (owns_constraint_graph)
      delete constraint_graph;
  }
  
  Nodes *acquire_nodes() {
    owns_nodes = false;
    return nodes;
  }

  Constraints *acquire_constraints() {
    owns_constraints = false;
    return constraints;
  }

  ConstraintGraph *acquire_constraint_graph() {
    owns_constraint_graph = false;
    return constraint_graph;
  }
};

//typedef unsigned Opcode;
//typedef void (*InstHandler)(FunctionState *, BlockState *, llvm::Instruction *);
//typedef std::map<Opcode, InstHandler> InstHandlers;

class FunctionState {
public:
  typedef unsigned Opcode;
  typedef void (*InstHandler)(FunctionState *, BlockState *, const llvm::Instruction *);
  typedef std::map<Opcode, InstHandler> InstHandlers;
 
  const GlobalState *global;
  const InstHandlers handlers;

  BlockCache block_cache;  
  bool owns_mutable_state;
  
  Nodes *nodes;
  Constraints *constraints;
  ConstraintGraph *constraint_graph;
  std::map<std::string, u32> static_returns;
  
  FunctionState(const GlobalState *global,
                const InstHandlers handlers)
    : global(global),
      handlers(handlers),
      owns_mutable_state(true) {
    
    nodes = new Nodes(global->nodes->size());
    constraints = new Constraints();
    constraint_graph = new ConstraintGraph();
  }

  MutableFunctionState acquire_mutable_state() {
    owns_mutable_state = false;
    return MutableFunctionState(nodes, constraints, constraint_graph);
  }
  
  ~FunctionState() {
    if (owns_mutable_state) {
      delete nodes;
      delete constraints;
      delete constraint_graph;
    }
  }

  bool contains_value(const llvm::Value *v) {
    if (global->nodes->value_nodes.count(v) != 0)
      return true;
    
    return nodes->value_nodes.count(v) != 0;
  }
  
  u32 add_unreachable(const llvm::Value *v, u32 obj_sz = 0,
                      bool weak = false) {
    return nodes->add_unreachable(v, obj_sz, weak);
  }
  
  u32 add_value(const llvm::Value *v, u32 obj_sz = 0,
                bool weak = false) {
    return nodes->add_value(v, obj_sz, weak);
  }

  u32 add_object(const llvm::Value *v, u32 obj_sz = 0,
                 bool weak = false) {
    return nodes->add_object(v, obj_sz, weak);
  }
  
  Node *find_node(u32 id) {    
    if (id < nodes->start)
      global->nodes->find_node(id);
    
    return nodes->find_node(id);
  }
  
  u32 find_value_node(const llvm::Value *v,
                      bool allow_null = false) const {
    if (u32 index = global->nodes->find_value_node(v, true)) {
      return index;
    }
    return nodes->find_value_node(v, allow_null);
  }

  u32 find_object_node(const llvm::Value *v,
                       bool allow_null = false) const {
    if (u32 index = global->nodes->find_object_node(v, true)) {
      return index;
    }
    return nodes->find_object_node(v, allow_null);
  }
  
  u32 find_ret_node(const llvm::Function *f) const {
    if (u32 index = global->nodes->find_ret_node(f, true)) {
      return index;
    }
    return nodes->find_ret_node(f);
  }
  
  u32 find_vararg_node(const llvm::Function *f) const {
    if (u32 index = global->nodes->find_vararg_node(f, true)) {
      return index;
    }
    return nodes->find_vararg_node(f);
  }
  
  void process_function(const llvm::Function *f);
  void process_block(const llvm::BasicBlock *bb, u32 parent_index);
};

const FunctionState::InstHandlers default_instruction_handlers();

#endif // end FUNCTION_STATE_H
