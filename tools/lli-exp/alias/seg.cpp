

SEG *build_constraint_based_seg(llvm::Module *m, u64 max_size = 1000000000) {
  // Make a new sparse evaluation graph.
  SEG *graph = new SEG(max_size);

  // Walk the module to process its instructions, filling in the graph and
  // collecting constraints.
  Constraints *cs = get_module_anders_constraints(m, graph);

  // Reduce the graph to make it more sparse.
  graph->reduce();

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
}
