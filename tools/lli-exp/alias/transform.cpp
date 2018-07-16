// This squashes a and b into each other, returning the index of the
// node that represents the other (could be a or b). This only applies
// to the T4 transformation. There arex other sematics for squashing
// nodes in different transformations.
static SEGIndex squash(T4 *t4, SEGIndex a, SEGIndex b) {
  // Are the nodes trivially the same?
  if (a == b) {
    // We can only squash pnodes into other nodes.
    assert(t4->graph->get_node(b)->is_pnode);
    
    // Then just return the index of the first.
    return a;
  }

  // Get pointers to the nodes.
  SEGNode *na = t4->graph->get_node(a), *nb = t4->graph->get_node(b);

  // Ensure that the squashed node has the lower rank. This makes it
  // so that b is always the squashed node with a lower rank.
  //
  // Does b have a higher rank than a?
  if (nb->rank() > na->rank()) {
    // That's not what we want, flip a and b around.
    SEGIndex t = a;
    a = b;
    b = t;    
  }
  if (nb->rank() == na->rank()) {
    // That's not what we want either. Give a the higher rank.
    na->rep--;
  }

  // Require pointers to the nodes and ensure that our rank relation
  // now holds.
  na = t4->graph->get_node(a), nb = t4->graph->get_node(b);
  assert(na->rank() > nb->rank());

  // Also assert that we're only squashing a pnode.
  assert(nb->is_pnode);
    
  // Finally, squash.
  na->uses_relevant_def |= nb->uses_relevant_def;
  na->has_const_transfer_func |= nb->has_const_transfer_func;
  na->pred |= nb->pred;
  nb->rep = a;

  return a;
}

// This reduces the predecessors of a pnodes to the minimal set and
// marks unreachable pnodes as unreachable.
//
// This operation does not delete the memory that the unreachable pnodes
// point to, it only removes edges to them in the graph.
//
// Actually this only operates on predecessors, is this enough? Probably,
// because any subtrees are always invalid if their parent is unreachable.
static void rm_unreachable_pnodes(SEG *graph, SEGIndexSet &torder) {

  // Iterate through the nodes in a topological order. Note that torder
  // may already be a sparse order if it was derived from a reduced graph.
  for (SEGIndexSet::iter i = torder.begin(), e = torder.end();
       i != e; ++i) {
    // Get the node at this order.
    SEGNode *node = graph->get_node(*i);

    // This is the set of predecessors we will retain for this node.
    SEGIndexSet retain;

    // We are only removing unreachable *pnodes*. We don't assert here
    // because we're not going to assume that the torder is only full
    // of pnodes. But in some cases that's true and should be checked
    // before calling this function.
    if (!node->pnode) {
      continue;
    }

    // Iterate through the predecessors of this node.
    for (SEGIndex::iter pi = node->pred.begin(), pe = node->pred.end(),
           pi != pe; pi++) {
      // What is the representative node of this predecessor?
      SEGNode *rep_index = graph->get_representative_index(*pi);

      // Did the predecessor get squashed into the current node?
      if (rep_index == *i) {
        // Yes, skip it, there's no point retaining it.
        continue;
      }

      // Was the predecessor marked with del?
      if (graph->get_node(rep_index)->del) {
        // Yes, there's no point retaining this either.
        continue;
      }

      // Otherwise, if the predecessor wasn't squashed into the current
      // node and it wasn't marked with del, then add it to our list of
      // nodes to retain.
      retain.insert(rep_index);
    }

    // Are there any predecessors we should retain?
    if (retain.empty()) {
      // No, this node is unreachable, mark it as such.
      node->rep = 0;
      assert(node->unreachable());
    } else {
      // Yes, there are predecessors we should retain, only retain those
      // by overwritting the predecessors of this node with the ones from
      // the retain set.
      node->pred.destructive_copy(retain);      
    }

    // Process next node.
  }
}

// Apply the T4 transform to a node in the graph.
//
// This algorithm needs to be explained. There are various papers that
// can help with this.
//
// Step 1.
// Step 2.
// Step 3.
//
static void apply_transform(T4 *t4, SEGIndex index) {
  // Get a pointer to the node.
  SEGNode *node = t4->graph->get_node(index);

  // Increment the depth-first search counter and assign a dfs number
  // to the node.
  node->dfs_num = t4->dfs_counter++;
  u32 orig_dfs_num = node->dfs_num;
  
  // Iterate through all of the predecessors of the node.
  for (SEGIndexSet::iter i = node->pred.begin(), e = node->pred.end();
       i != e; i++) {
    SEGNode *pred = graph->get_representative_node(*i);

    // Is this a pnode?
    if (!pred->pnode) {
      // No, continue, we can only apply the T4 transformation to
      // pnodes.
      continue;
    }

    // Has this predecessor been deleted yet?
    if (!pred->del) {
      // No, it's still a candidate for processing. Then have we
      // processed it yet?
      if (!pred->dfs_num) {
        // No, process it. This in effect allows us to process pnodes
        // backwards up the tree until we run into no more predecessors.
        apply_transform(t4, *i);
      }

      // Does this node have a dfs number that's larger than the dfs
      // number of this predecessor?
      if (node->dfs_num > pred->dfs_num) {
        // Then the steal the lower dfs number from its predecessor.
        //
        // ** Why is this a good idea? Is this actually safe? This seems
        // generally weird/wrong.
        node->dfs_num = pred->dfs_num;
      }
    }
  }

  // After we've finished iterating through the node's predecessors,
  // check to see if this node is squashable.
  //
  // Did it acquire a different order (i.e. from one of its predecessors)?
  if (node->dfs_num != orig_dfs_num) {
    // Yes, then push it onto the stack so we can squash it later.
    t4->merge_stack.push(node);

    // We'll just return here because we only keep track of the topological
    // order of nodes that are retained by the transformation, not ones that
    // are squashed.
    return;
  }

  // Otherwise, it shouldn't get smashed, so maybe we should squash other
  // nodes into *it* instead. Check to see if there are any nodes that are
  // suitable.
  while (!t4->merge_stack.empty()) {
    SEGIndex squashable_index = t4->merge_stack.top();
    SEGNode *squashable = t4->graph->get_node(squashable_index);

    // Can we squash this into our node?
    if (squashable->dfs_num >= orig_dfs_num) {
      // Yes, squash away and remove it from the stack. The returned
      // index is the new index of the original node with the squashable
      // node merged into it.
      index = squash(t4, index, squashable_index);

      // Remove the element we just processed from the stack and continue
      // trying to merge.
      t4->merge_stack.pop();
      continue;
    }

    // If we can't squash this one, then stop trying. We can't squash any of
    // the remaining elements of the stack either because the stack is
    // organized in descending dfs order.
    break;
  }

  // Since this node wasn't squashed, we need to give it an order in the
  // topological order. That's just its index after any nodes were squashed
  // into it.
  t4->torder.insert(index);

  // ** Mark this node as deleted ... WHAT WHY?
  node->del = true;

  // We're totally done with this node.
  return;
}

// Run the T4 transformation on the graph. At the moment, a T4 object can only
// be run once, which is fine for us at the moment. In the future it should be
// re-executable.
void T4::run() {
  for (SEGIndex i = 1; i < this->graph->nodes.size(); ++i) {
    SEGNode *node = this->graph->get_node(i);

    // Is this a standalone node? That means that it hasn't been
    // squashed/merged into another node.
    if (node->represents_itself()) {
      // Is this a p-node and is it yet to be processed?
      if (node->is_pnode && !node->dfs_num) {
        // Yes. Then we must attempt to do a T4 transformation on it.
        apply_transform(this, i);
      }

      // Check all nodes that were squashed (pnodes and mnodes alike)
      // for nodes that should remain in the sparse representation. 
      if (this->graph->node_survives_reduction(node)) {
        this->rdefs.push_back(node);
      }

      // ** This seems wonky/weird: The original code says that this needs
      // to happen because "T6 assumes that all rep nodes have del == true).
      // This seems like crazy, interconnected code design.
      node->del = true;
    }
  }

  // Assert that we processed all of the nodes we pushed onto the stack.
  assert(this->merge_stack.empty());

  // Remove any pnodes that the transformation made unreachable. This is what
  // actually prunes the graph.
  rm_unreachable_pnodes(this->graph, this->torder);

  // Finish the transformation.
  return;
}
