#include "seg.h"
#include "anders_constraints.h"

class Processor {
public:
  SEG *graph; // Graph being built.

  std::vector<u32> defs;  // Store constraint -> node containing store
  std::vector<u32> uses;  // Load constraint -> node containing load

  std::map<llvm::BasicBlock *, u32> bb_start; // Start nodes for BasicBlocks
  typedef std::map<llvm::BasicBlock *, u32>::iterator bb_iterator;

  std::map<llvm::Function *, u32> func_start_nodes; // Start nodes for Functions  
  std::map<u32, std::vector<llvm::Function *> > func_callsites; // callsite -> function targets
  std::map<llvm::Function *, u32> func_ret_nodes; // function -> return node
  std::map<u32, u32> callsite_succ; // callsite -> local successor
  std::vector<u32> idr_cons; // indices of constraints from idr calls
  std::vector<std::pair<llvm::CallInst *, u32> > idr_calls; // <idr call, callsite> pairs

  Processor(SEG *graph) : graph(graph) {}
};

static llvm::Function* calledFunction(llvm::CallInst *ci) {
  if (llvm::Function *f = ci->getCalledFunction()) {
    return f;
  }

  llvm::Value *v = ci->getCalledValue();

  if (llvm::ConstantExpr *c = llvm::dyn_cast<llvm::ConstantExpr>(v)) {
    if (c->getOpcode() == llvm::Instruction::BitCast) {
      if (llvm::Function *f = llvm::dyn_cast<llvm::Function>(c->getOperand(0))) {
        return f;
      }
    }
  }

  return 0;
}

class BlockState {
public:
  bool contains_call;
  u32 constraints_sz;
  u32 position;

  BlockState(u32 position) :
    contains_call(false),
    constraints_sz(0),
    position(position) {}
};

static void Processor::add_call_edges(BlockState *bs, llvm::Instruction *inst) {
  llvm::CallInst *ci = llvm::cast<llvm::CallInst>(inst);
  
  // Step 1.
  //
  // Handle direct calls.
  llvm::Function *f = calledFunction(ci);
  if (f) {
    // Step 1.1.
    //
    // Process non-external calls first.
    if (!ext_info->is_ext(f)) {
      // Has a call.
      bs->contains_call = true;

      this->func_callsites[bs->position].push_back(f);

      assert(!this->callsite_succ.count(bs->position));
      u32 next = this->graph->insert_new_node(PNODE);
      this->callsite_succ[bs->position] = next;

      this->graph->connect_nodes(bs->position, next);
      bs->position = next;

      return;
    }

    // Step 1.2.
    //
    // Count how many store constraints were added.
    u32 num_stores = 0;
    for (u32 i = bs->cons_sz; i < bs->cons_sz; i++) {
      if (constraints[i].type == ConstraintStore) {
        num_stores++;
      }
    }

    // Step 1.3.
    //
    // Process multiple stores. This can be caused by memcpy/memmove.
    // Create a diamond-shape.
    if (num_stores > 1) {
      // Bottom of the diamond.
      u32 bottom = this->insert_new_node(PNODE);

      for (u32 i = 0; i < num_stores * 2; i += 2) {
        assert(constraints[bs->cons_sz + i].type == ConstraintLoad);
        assert(constraints[bs->cons_sz + i + 1].type == ConstraintStore);

        // Create a non-preserving node.
        u32 next = this->graph->insert_new_node(MNODE);
        SEGNode *node = this->graph->get_node(next);
        node.uses_relevant_def = true;

        // And connect it to the graph.
        this->graph->connect_nodes(bs->position, next);
        this->graph->connect_nodes(next, bottom);

        this->uses[bs->cons_sz + i] = next;
        this->defs[bs->cons_sz + i + 1] = next;
      }

      bs->position = bottom;
      return;
    }

    // Step 1.4.
    //
    // Process single stores.
    if (num_stores == 1) {
      SEGNode *node = this->graph->get_node(bs->position);
      if (node->pnode) {
        // If there's a store, it can longer preserve state.
        node->pnode = false;
      } else {
        // If it's already non-preserving, add a new mnode
        // and connect it to the graph.
        u32 next = this->graph->insert_new_node(MNODE);
        this->graph->connect_nodes(bs->position, next);
        bs->position = next;
      }
    }
    
    // Go through the new constraints.
    for (u32 i = bs->cons_sz; i < constraints.size(); i++) {
      Constraint &c = constraints[i];

      if (c.type == ConstraintStore) {
        this->defs[i] = bs->position;
      }
      if (c.type == ConstraintLoad) {
        SEGNode *node = this->graph->get_node(bs->position);
        node->uses_relevant_def = true;
        this->uses[i] = bs->position;
      }
    }

    return;
  }

  // Step 2.
  //
  // Handle indirect calls.
  if (llvm::isa<llvm::InlineAsm>(ci->getCalledValue())) {
    // Skip inline assembly.
    return;
  }

  // Is there no value at the called value?
  u32 fp = get_value_node(ci->getCalledValue(), true);
  if (!fp) {
    return;
  }

  // Has a call.
  bs->contains_call = true;

  // Go through the new constraints and save them to process later.
  for (u32 i = bs->cons_sz; i < constraints.size(); i++) {
    this->idr_cons.push_back(i);
  }
  
  // Also save the indirect call inst and current node so
  // we can add the interprocedural control-flow edges later,
  // as well as process indirect external calls.
  this->idr_calls.push_back(std::make_pair<llvm::CallInst *, u32>(ci, bs->position));
  
  // Ensure the call inst has an associated object node.
  assert(!get_value_node(ci, true) || get_object_node(ci));
  
  // Ensure a single successor for the callsite.
  assert(!this->call_succ.count(bs->position));
  u32 next = this->insert_new_node(PNODE);
  this->call_succ[bs->position] = next;

  this->callsite_succ[bs->position] = next;
  this->connect_nodes(bs->position, next);

  bs->position = next;
  return;
}

static void add_load_edges(llvm::Instruction *inst) {
  SEGNode *node = this->get_node(bs->position);
  node->uses_relevant_def = true;

  // Go through the new constraints.
  for (u32 i = bs.cons_sz; i < constraints.size(); i++) {
    if (constraints[i].type == ConstraintLoad) {
      this->uses[i] = bs->position;
      break;
    }
  }
}

static void add_store_edges(llvm::Instruction *inst) {
  SEGNode *node = this->get_node(bs->position);
  if (node->pnode) {
    node->pnode = true;
  } else {
    u32 next = this->insert_new_node(MNODE);
    this->connect_nodes(bs->position, next);
    bs->position = next;
  }

  // There may have been multiple constraints added, but there
  // will be exactly one store constraint
  for (u32 i = bs->cons_sz; i < constraints.size(); i++) {
    Constraint &c = constraints[i];
    if (c.type == ConstraintStore) {
      this->defs[i] = bs->position;
      break;
    }
  }
}

static void Processor::process_block(SEGIndex parent, llvm::BasicBlock *bb) {
  // Step 1.
  //
  // Process blocks we've seen before.
  bb_iterator bb_it = this->bb_start.find(bb);
  if (bb_it != this->bb_start.end()) {
    // Simply make the edge and return.
    assert(parent != MAX_U32);
    this->graph->connect_nodes(parent, bb_it->second);
    return;
  }

  // Step 2.
  //
  // Initialize the start nodes.
  SEGIndex index = this->graph->insert_new_node(PNODE);
  this->bb_start[bb] = index;

  if (parent != MAX_U32) {
    this->graph->connect_nodes(parent, index);
  } else {
    Function *f = bb->getParent();
    assert(!this->func_start_nodes.count(f));
    this->func_start_nodes[f] = index;
  }

  // Step 3.
  //
  // Iterate through instructions, translate them to constraints,
  // and create nodes for them in the graph.
  BlockState bs(index);
  for (llvm::BasicBlock::iterator i = bb->begin(), i = bb->end();
       i != e; i++) {
    llvm::Instruction *inst = &*i;
    bool is_pointer = llvm::isa<llvm::PointerType>(inst->getType());
    unsigned opcode = inst->getOpcode();

    bs.constraints_sz = contraints.size();    

    if (opcode == llvm::Instruction::Ret) {
      assert(!is_pointer);
      this->process_return(inst);

      if (bs.contains_call) {
        u32 next = this->graph->insert_new_node(PNODE);
        this->graph->connect_nodes(bs.position, next);
        bs.position = next;
      }
      
      assert(!this->func_ret_nodes.count(bb->getParent()));
      this->func_ret_nodes[bb->getParent()] = bs.position;

      continue;
    }
    if (opcode == llvm::Instruction::Malloc ||
        llvm::Instruction::Alloca) {
      assert(is_pointer);
      this->process_alloc(inst);

      continue;
    }
    if (opcode == llvm::Instruction::GetElementPtr) {
      assert(is_pointer);
      this->process_gep(inst);

      continue;
    }
    if (opcode == llvm::Instruction::IntToPtr) {
      assert(is_pointer);
      this->process_int2ptr(inst);

      continue;
    }
    if (opcode == llvm::Instruction::BitCast) {
      if (is_pointer) {
        this->process_bitcast(inst);
      }

      continue;
    }
    if (opcode == llvm::Instruction::PHI) {
      if (is_pointer) {
        this->process_phi(inst);
      }

      continue;
    }
    if (opcode == llvm::Instruction::Select) {
      if (is_pointer) {
        this->process_select(inst);
      }

      continue;
    }
    if (opcode == llvm::Instruction::VAArg) {
      if (is_pointer) {
        this->process_vaarg(inst);
      }

      continue;
    }
    
    bool call = false;
    if (opcode == llvm::Instruction::Invoke ||
        llvm::Instruction::Call) {      
      this->process_call(inst);
      call = true;
    }

    bool load = false;
    if (opcode == llvm::Instruction::Load) {
      if (is_pointer) {
        this->process_load(inst);
        load = true;
      }
    }

    bool store = false;
    if (opcode == llvm::Instruction::Store) {
      assert(!is_pointer);
      this->process_store(inst);
      store = true;
    }

    if (!call && !load && !store) {
      assert(!is_pointer && "unknown instruction has pointer type");
    }    

    int constraints_diff = constraints.size() - bs.constraints_sz;
    if (!constraints_diff && !call) {
      continue;
    }

    this->defs.insert(this->defs.end(), constraints_diff, 0);
    assert(this->defs.size() == constraints.size());

    this->uses.insert(this->uses.end(), constraints_diff, 0);
    assert(this->uses.size() == constraints.size());

    if (call) {
      add_call_edges(&bs, inst);
    }
    if (load) {
      add_load_edges(&bs, inst);      
    }
    if (store) {
      add_store_edges(&bs, inst);
    }
  }

  // Step 4.
  //
  // Recurse into the successors of this block.
  for (llvm::succ_iterator i = llvm::succ_begin(bb), i = llvm::succ_end(bb);
       i != e; i++) {
    this->process_block(bs.position, *i);
  }   
}

static void Processor::process_function(llvm::Function *f) {
  // Step 1.
  //
  // Make nodes for all pointer-return instructions.
  for (llvm::inst_iterator i = llvm::inst_begin(f), i = llvm::inst_end(f);
       i != e; i++) {
    llvm::Instruction *inst = &*i;
    if (llvm::isa<llvm::PointerType>(inst->getType())) {
      nodes.push_back(new Node(inst));
      value_nodes[inst] = next_node++;
    }
  }

  // Step 2.
  //
  // Insert entries for the global constraints into defs and uses.
  if (defs.empty() && uses.empty()) {
    defs.assign(constraints.size(), 0);
    uses.assign(constraints.size(), 0);
  }

  // Step 3.
  //
  // Process the function's blocks.
  u32 parent = MAX_U32;
  process_blocks(parent, &f->getEntryBlock());

  // Step 4.
  //
  // Clean up.
  bb_start.clear();
}
