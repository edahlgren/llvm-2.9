#include "seg.h"
#include "transform.h"

void SEG::print(std::ostream &os) {
  
}

bool SEG::node_survivies_reduction(SEGNode *node) {
  return node->uses_relevant_def || (!node->has_const_transfer_func && !node->pnode);    
}

// This reduces the graph to make it "more sparse". It works by applying
// several well-known transformations and obeying the graph's predicate
// node_survives_reduction for each node that could be reduced.
void SEG::reduce() {
  T4 t4(this);
  t4.run();

  T2 t2(this);
  t2.run(t4.torder);

  T6 t6(this);
  t6.run(t4.rdefs);

  T5 t5(this);
  t5.run(t6.pnode_rdefs);
}

SEG *build_constraint_based_seg(llvm::Module *m, u64 max_size = 1000000000) {
  // Make a new sparse evaluation graph.
  SEG *graph = new SEG(max_size);

  // Walk the module to process its instructions, filling in the graph and
  // collecting constraints.
  AndersConstraints *cs = build_anders_constraints(m, graph);

  // Reduce the graph to make it more sparse.
  graph->reduce();

  /**

  // Build a data flow graph from the constraints.
  DataFlowGraph *dfg = build_dfg(cs);

  // Do we need the constraints anymore now that they've been
  // incorporated into the dfg?
  delete cs;

  // Partition variables in the dfg. Maybe this can be in dfg_from_constraints
  // above.
  dfg->partition();

  // Incorporate interprocedural edges from the dfg.
  graph->extend(dfg);

  // Get rid of the dfg, we don't need it anymore.
  delete dfg;
  
  // Return the final sparse representation.
  return graph;
  **/
}
