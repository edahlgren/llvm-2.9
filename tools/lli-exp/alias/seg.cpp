#include "seg.h"
#include "transform.h"

bool SEG::node_survives_reduction(SEGNode *node) {
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
  // Step 1.
  //
  // Initialize a new evaluation graph and a processor wrapper that
  // will hold def-use information.
  SEG *graph = new SEG(max_size);
  Processor *proc = new Processor(graph);

  // Step 2.
  //
  // Walk the module to find constraints and fill in def-use information.
  Constraints *cs = build_constraints(m, proc);

  // Step 3.
  //
  // Optimize the constraints so that they'll be easier to solve for.
  cs->optimize();

  // ??
  graph->make_interprocedural(m);
  
  // Step 4.
  //
  // Solve the constraints.
  cs->solve();

  
  
  
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

// Write the graph in the dot language to os.o
void SEG::print(std::ostream &os) {
  // Start the graph wrapper.
  os << "strict digraph SEG {" << std::endl;

  // Process each node.
  for (SEGIndex i = 1; i < this->nodes.size(); i++) {
    SEGNode *node = this->get_node(i);

    // Does this node not represent itself?
    if (!node->represents_itself()) {
      // Then it's been "squashed" into something else, skip it.
      continue;
    }

    // Has this node been marked deleted?
    if (node->del) {
      // Yes, skip it.
      continue;
    }

    // Start the node header.
    os << "\t" << i << " [label=\"";
    os << i << ":";

    // Is this a preserving node?
    if (node->pnode) {
      // Mark it with a "DEF" label.
      os << "DEF";
    }

    // Would this node survive reduction?
    if (this->node_survives_reduction(node)) {
      // Is it a preserving node?
      if (node->pnode) {
        // Add a separate to distinguish from the label above.
        os << "_";        
      }

      // Mark it with a "USE" label.
      os << "USE";
    }

    // Complete the label.
    os << "\"";

    // Is this the start node?
    if (node == this->start) {
      // Then make it red so that it's easy to distinguish.
      os << ",color=red";
    }

    // Finish the node header.
    os << "];" << std::endl;

    // Write the edges from predecessors to this node.
    for (SEGIndexSet::iter pi = node->pred.begin(), pe = node->pred.end();
         pi != pe; pi++) {
      SEGIndex rep_index = this->get_representative_index(*pi);

      // Does this node point to itself?
      if (rep_index == i) {
        // Yes, skip it.
        continue;
      }

      // Has this predecessor been marked deleted?
      if (this->get_node(rep_index)->del) {
        // Yes, skip it.
        continue;
      }

      // Write the dge.
      os << "\t" << rep_index << " -> " << i << ";" << std::endl;
    }
  }

  // Close the graph wrapper. And we're done.
  os << "}" << std::endl;
}
