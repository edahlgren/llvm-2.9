#include "seg.h"
#include "anders_constraints.h"

class Processor {
public:
  SEG *graph; // Graph being built.

  std::vector<u32> defs;  // Store constraint -> node containing store
  std::vector<u32> uses;  // Load constraint -> node containing load

  std::map<llvm::BasicBlock*, u32> bb_start; // Start nodes for BasicBlocks
  typedef std::map<llvm::BasicBlock*, u32>::iterator bb_iterator;

  std::map<llvm::Function*, u32> fun_start; // Start nodes for Functions  
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

static void add_call_edges(llvm::Instruction *inst) {
  llvm::CallInst *ci = llvm::cast<llvm::CallInst>(inst);
  
  // Step 1.
  //
  // Handle direct calls.
  llvm::Function *f = calledFunction(ci);
  if (f) {
    // Not an external call.
    if (!ext_info->is_ext(f)) {
      block_call = true;
      fun_cs[n].push_back(F);

      // guarantee a single successor for the callsite
      //
      assert(!call_succ.count(n));
      u32 next = create_node();
      call_succ[n] = next;
      add_edge(n,next); n = next;
      return;
    }

    // see how many stores were added
    //
    u32 num_stores = 0;
    for (u32 i = curr_cons; i < cons_sz; ++i) {
      if (constraints[i].type == store_cons) { num_stores++; }
    }
    
    // memcpy/move, etc can create multiple loads and stores; we
    // want to treat each load/store pair in parallel, so we
    // create separate nodes for each pair and create a
    // "diamond" in the CFG -- we are aided by the fact that the
    // constraint generator placed the related pairs of
    // constraints in sequence: <load,store>, <load,store>, ...
    //
    if (num_stores > 1) {
      u32 b = create_node(); // the bottom of the diamond
      
      for (u32 i = 0; i < num_stores*2; i += 2) {
        assert(constraints[curr_cons+i].type == load_cons);
        assert(constraints[curr_cons+i+1].type == store_cons);
        
        u32 next = create_node(true);
        G[next].r = true;
        add_edge(n,next); add_edge(next,b);
        uses[curr_cons+i] = next;
        defs[curr_cons+i+1] = next;
      }
      
      n = b;
      return;
    }

    if (num_stores == 1) {
      if (!NP(n)) {
        G[n].np = true;
      } else {
        u32 next = create_node(true);
        add_edge(n,next); n = next;
      }
    }
    
    // map any loads and stores to the ICFG node
    //
    for (u32 i = curr_cons; i < cons_sz; ++i) {
      Constraint& C = constraints[i];
      
      if (C.type == store_cons) {
        defs[i] = n;
      }
      else if (C.type == load_cons)  {
        G[n].r = true; uses[i] = n;
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

  u32 fp = get_value_node(ci->getCalledValue(), true);
  if (!fp) {
    return;
  }
  
  block_call = true;
  
  // we can process loads and stores from indirect calls
  // directly without computing SSA form, since the objects in
  // question are, by construction, already in SSA form; we
  // save these constraints to process later
  //
  for (u32 i = curr_cons; i < cons_sz; ++i) {
    idr_cons.push_back(i);
  }
  
  // we also save the indirect call inst and current node so
  // we can add the interprocedural control-flow edges later,
  // as well as process indirect external calls
  //
  idr_calls.push_back(make_pair<CallInst*,u32>(ci,n));
  
  // make sure call inst has an associated object node
  //
  assert(!get_val_node(ci,true) || get_obj_node(ci));
  
  // guarantee a single successor for the callsite
  //
  assert(!call_succ.count(n));
  u32 next = create_node();
  call_succ[n] = next;
  add_edge(n,next); n = next;
}

static void add_load_edges(llvm::Instruction *inst) {
  G[n].r = true;

  // there may have been multiple constraints added, but there
  // will be exactly one load constraint
  //
  for (u32 i = curr_cons; i < cons_sz; ++i) {
    if (constraints[i].type == load_cons) { uses[i] = n; break; }
  }
}

static void add_store_edges(llvm::Instruction *inst) {
  if (!NP(n)) {
    G[n].np = true;
  } else {
    u32 next = create_node(true);
    add_edge(n,next); n = next;
  }
  
  // there may have been multiple constraints added, but there
  // will be exactly one store constraint
  for (u32 i = curr_cons; i < cons_sz; ++i) {
    Constraint& C = constraints[i];
    if (C.type == store_cons) {
      defs[i] = n;
      break;
    }
  }
}

enum InstClass {
  InstClassNone = 0,
  InstClassCall,
  InstClassLoad,
  InstClassStore,
}

static ConstraintType Processor::process_instruction(llvm::Instruction *inst) {
  bool is_pointer = llvm::isa<llvm::PointerType>(inst->getType());

  switch (inst->getOpcode()) {
  case llvm::Instruction::Ret:
    assert(!is_pointer);
    this->process_return(inst);
    return InstClassNone;
    
  case llvm::Instruction::Invoke:
  case llvm::Instruction::Call:
    this->process_call(inst);
    return InstClassCall;
    
  case llvm::Instruction::Malloc:
  case llvm::Instruction::Alloca:
    assert(is_pointer);
    this->process_alloc(inst);
    return InstClassNone;

  case llvm::Instruction::Load:
    if (is_pointer) {
      this->process_load(inst);
      return InstClassLoad;
    }
    return InstClassNone;

  case llvm::Instruction::Store:
    assert(!is_pointer);
    this->process_store(inst);
    return InstClassStore;
      
  case llvm::Instruction::GetElementPtr:
    assert(is_pointer);
    this->process_gep(inst);
    return InstClassNone;

  case llvm::Instruction::IntToPtr:
    assert(is_pointer);
    this->process_int2ptr(inst);
    return InstClassNone;

  case llvm::Instruction::BitCast:
    if (is_pointer) {
      this->process_bitcast(inst);
    }
    return InstClassNone;

  case llvm::Instruction::PHI:
    if (is_pointer) {
      this->process_phi(inst);
    }
    return InstClassNone;

  case llvm::Instruction::Select:
    if (is_pointer) {
      this->process_select(inst);
    }
    return InstClassNone;

  case llvm::Instruction::VAArg:
    if (is_pointer) {
      this->process_vaarg(inst);
    }
    return InstClassNone;

  default:
    assert(!is_pointer && "unknown instruction has pointer return type");
    return InstClassNone;      
  }
}

static void Processor::process_block(u32 parent, llvm::BasicBlock *bb) {
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
  for (llvm::BasicBlock::iterator i = bb->begin(), i = bb->end();
       i != e; i++) {
    llvm::Instruction *inst = &*i;
    InstClass inst_class = this->process_instruction(inst);

    if (inst_class == InstClassCall) {
      n = this->add_call_edges(inst);
    } else if (inst_class == InstClassLoad) {
      n = this-{add_load_edges(inst);
    } else if (inst_class == InstClassStore) {
      n = this->add_store_edges(inst);
    }
  }

  // Step 4.
  //
  // Recurse into the successors of this block.
  for (llvm::succ_iterator i = llvm::succ_begin(bb), i = llvm::succ_end(bb);
       i != e; i++) {
    this->process_block(n, *i);
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
