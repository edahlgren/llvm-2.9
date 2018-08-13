//===- functions.cpp ------------------------------------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#include "debug.h"
#include "function_state.h"
#include "predicates.h"

#include "llvm/Module.h"               // for llvm::Function, llvm::BasicBlock
#include "llvm/Support/CFG.h"          // for llvm::succ_begin, llvm::succ_end
#include "llvm/Support/InstIterator.h"

void FunctionState::process_function(const llvm::Function *f) {
  assert(!f->isDeclaration());
  
  for (llvm::const_inst_iterator ii = llvm::inst_begin(f),
         ei = llvm::inst_end(f); ii != ei; ii++) {
    const llvm::Instruction *inst = &*ii;
    if (is_pointer(inst))
      this->nodes->add_value(inst);    
  }

  // Process blocks recursively, starting at the entry block.
  this->process_block(&f->getEntryBlock(), PARENT_ENTRY_BLOCK);  
}

void FunctionState::process_block(const llvm::BasicBlock *bb,
                                  u32 parent_index) {

  if (u32 index = this->block_cache.lookup(bb)) {
    assert(parent_index != PARENT_ENTRY_BLOCK);
    this->constraint_graph->add_edge(parent_index, index);
    return;
  }

  llvm::outs() << "  ** Processing block "
               << bb->getName() << " **\n";
    
  u32 index = this->constraint_graph->create_node(PNODE);
  this->block_cache.insert(bb, index);

  if (parent_index != PARENT_ENTRY_BLOCK) {
    this->constraint_graph->add_edge(parent_index, index);
  } else {
    ConstraintGraphMetadata *meta = this->constraint_graph->meta;
    const llvm::Function *f = bb->getParent();
    assert(!meta->func_start_nodes.count(f));
    meta->func_start_nodes[f] = index;
  }

  BlockState bs(bb, index);
  for (llvm::BasicBlock::const_iterator i = bb->begin(), e = bb->end();
       i != e; i++) {
    const llvm::Instruction *inst = i;
    llvm::outs() << "    ** " << inst_str(inst) << "\n";

    InstHandlers::const_iterator it = this->handlers.find(inst->getOpcode());
    if (it == this->handlers.end()) {
      bool ptr = is_pointer(inst->getType());  
      assert(!ptr && "unknown instruction is a pointer");
      continue;
    }

    InstHandler handler = it->second;
    handler(this, &bs, inst);
  }

  for (llvm::succ_const_iterator i = llvm::succ_begin(bb),
         e = llvm::succ_end(bb); i != e; i++)
    this->process_block(*i, index);
}
