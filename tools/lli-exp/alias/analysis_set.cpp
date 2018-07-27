#include "analysis_set.h"

AnalysisSet *init_analysis_set(llvm::Module *m) {
  AnalysisSet *as = new AnalysisSet();

  // Step 1.
  //
  // Add the placeholder nodes (the ones that come before the first
  // real node) and position the first node.
  for (u32 i = 0; i < NodeFirst; i++) {
    // Initialize them with empty llvm::Values.
    Node *node = new Node();

    // If this is supposed to represent the unknown target of pointers
    // cast from int, then it's actually an object because its address
    // is taken. The special node ConstToUnknownNode is its initial pointer.
    if (i == NodeUnknownTarget) {
      node->obj_sz = 1;
      as->constraints->add(ConstraintAddrOf,
                           NodeConstToUnknownTarget,
                           NodeUnknownTarget);
    }
    nodes.push_back(new Node);
  }
  as->nodes->next = NodeFirst;

  // Step 2.
  //
  // Find and map all struct types in the program.
  llvm::TypeSymbolTable &tst = m->getTypeSymbolTable();
  for (llvm::TypeSymbolTable::iterator i = tst.begin(), e = tst.end();
       i != e; i++) {
    const llvm::StructType *st = llvm::dyn_cast<llvm::StructType>(i->second);
    if (st) {
      as->structs->analyze(st);
    }
  }

  // Step 3.
  //
  // Add constraints for functions pointers, function args, and function
  // return values;
  for (llvm::Module::iterator i = m->begin(); e = m->end(); i != e; i++) {
    llvm::Function *f = *i;
    as->add_constraints(i);
  }
  
  // Step 4.
  //
  // Add constraints for globals. This might include adding constraints
  // for const GEP expressions using the global.
  for (llvm::Module::global_iterator i = m->global_begin(), e = m->global_end();
       i != e; i++) {
    llvm::GlobalVariable *g = *i;
    as->add_constraints(i);
  }

  // Step 5.
  //
  // Initialize globals (separately from Step 4). This is because an
  // initializer may refer to a global below it.
  for (llvm::Module::global_iterator i = m->global_begin(), e = m->global_end();
       i != e; i++) {
    llvm::GlobalVariable *g = *i;
    if (g->hasInitializer()) {
      as->initialize(g);
    }
  }

  // Step 6.
  //
  // Initialize the GEP constant expressions.
  as->initialize_geps();
}

void AnalysisSet::validate() {
  assert(this->node->next == this->nodes->size());  
}

FlowAnalysisSet *init_flow_analysis_set(AnalysisSet *as,
                                        Processor *proc,
                                        BDDSet *bdds,
                                        u32 num_tmp) {
  
  FlowAnalysisSet *fas = new FlowAnalysisSet();

  fas->top.assign(as->nodes->nodes.size() + num_tmp, PtsSet());
  fas->strong.assign(as->last_obj() + 1, false);

  for (int i = 0; i < as->last_obj() + 1; i++) {
    fas->strong[i] = !as->nodes->nodes[i]->weak;
  }

  fas->pdom = fdd_ithset(0);
  fas->g_to_p = bdd_newpair();
  fdd_setpair(g_to_p, 1, 0);
  fas->bdd_off = bdds->gep_bdds;

  for (int i = 0; i < as->constraints.size(); i++) {
    Constraint &c = as->constraints[i];
    if (c.off == MAX_U32) {
      continue;
    }

    u32 index = fas->dfs->insert(c);

    if (proc->defs[i]) {
      assert(c.type == ConstraintStore);
      fas->defs.resize(index + 1, 0);
      fas->defs[index] = proc->defs[i];
      
      continue;
    }

    if (proc->uses[i]) {
      assert(c.type == ConstraintLoad);
      fas->uses.resize(index + 1, 0);
      fas->uses[index] = proc->uses[i];
      
      continue;
    }

    if ((c.type == ConstraintAddrOf ||
         c.type == ConstraintCopy) &&
        c.dest <= as->last_obj()) {
      fas->global_to_dfg[c.dest].push_back(index);
    }
  }

  fas->dfg->finalize_insert();

  return fas;
}
