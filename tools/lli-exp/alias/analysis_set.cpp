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
