//===- analysis_set.cpp -- ------------------------------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#include "analysis_set.h"
#include "function_state.h"
#include "global_state.h"

#include "llvm/GlobalVariable.h"   // for llvm::Type
#include "llvm/Module.h"           // for llvm::Function

const FunctionState::InstHandlers inst_handlers =
  default_instruction_handlers();

void AnalysisSet::init(llvm::Module *m) {  
  // 0.
  ModuleState *ms = new ModuleState();

  // 1.
  for (u32 i = 0; i < NodeFirst; i++) {
    u32 obj_sz = 0;
    if (i == NodeUnknownTarget) {
      obj_sz = 1;
      this->constraints->add(ConstraintAddrOf,
                             NodeConstToUnknownTarget,
                             NodeUnknownTarget);
    }
    this->nodes->add_unreachable(nullptr, obj_sz);
  }

  // 2.
  GlobalState *gs = new GlobalState(ms);

  // -- 2.1.
  for (llvm::Module::iterator i = m->begin(), e = m->end();
       i != e; i++) {
    gs->process_function_signature(i);
  }

  // -- 2.2.
  for (llvm::Module::global_iterator i = m->global_begin(),
         e = m->global_end(); i != e; i++) {
    gs->process_global_signature(i);
  }

  // -- 2.3.
  for (llvm::Module::global_iterator i = m->global_begin(),
         e = m->global_end(); i != e; i++) {
    gs->process_global_value(i);
  }

  // 3.
  std::vector<MutableFunctionState> states;
  for (llvm::Module::iterator i = m->begin(), e = m->end();
       i != e; i++) {
    llvm::Function *f = i;
    if (f->isDeclaration())
      continue;

    // -- 3.1.
    FunctionState *fs = new FunctionState(gs, inst_handlers);
    fs->process_function(f);
    states.push_back(fs->acquire_mutable_state());
  }

  // 4.
  // TODO: merge everything together.

  this->dump(llvm::outs());
}

void print_named_constraints(Nodes *nodes, Constraints *cons, int l = 0) {
  llvm::raw_ostream &os = llvm::outs();
  
  os.indent(l) << "Constraints {" << "\n";
  for (Constraints::iterator i = cons->begin(), e = cons->end();
       i != e; ++i) {
    
    Constraint c = *i;      
    os.indent(l + 1) << "constraint {" << "\n";
    os.indent(l + 2) << "type:\t\t" << c.type_string() << "\n";

    Node *dest_node = nodes->find_node(c.dest);
    os.indent(l + 2) << "(d)est:\t" << c.dest
                     << dest_node->value_to_string(l + 2) << "\n";

    Node *src_node = nodes->find_node(c.src);
    os.indent(l + 2) << "(s)rc:\t" << c.src
                     << src_node->value_to_string(l + 2) << "\n";
    
    os.indent(l + 2) << "offset:\t" << c.off << "\n";
    os.indent(l + 1) << "}" << "\n";
  }
  os.indent(l) << "}" << "\n";  
}

void AnalysisSet::dump(llvm::raw_ostream &os) {
  os << "===========================================================" << "\n";
  nodes->print();
  print_named_constraints(nodes, constraints);
  structs.print();
  cgraph->print();
  os << "===========================================================" << "\n";

  os << "Stats" << "\n";
  os << "----------------" << "\n";
  os << nodes->size() << " nodes" << "\n";
  os << constraints->size() << " constraints" << "\n";
  os << structs.size() << " structs" << "\n";
}
