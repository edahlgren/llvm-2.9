//===- cfg.cpp - DominatorTree utilities ----------------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#include "dominator.h"

#include "llvm/Support/raw_ostream.h"

// Iterate through the children of bb and assign all blocks a number
// in depth-first search order. This makes more sense as a recursive
// algorithm but that might blow the stack, so we use a more obtuse,
// iterative one.
static unsigned build_dominance_info_records(DominanceGraph *dg, llvm::BasicBlock *v, unsigned n) {
  llvm::SmallVector<std::pair<llvm::BasicBlock *,
                              llvm::succ_iterator>, 32> worklist;
  worklist.push_back(std::make_pair(v, llvm::succ_begin(v)));
  while (!worklist.empty()) {
    llvm::BasicBlock *bb = worklist.back().first;
    llvm::succ_iterator next_succ = worklist.back().second;

    DominanceInfoRec &bbinfo = dg->info[bb];

    if (next_succ == llvm::succ_begin(bb)) {
      bbinfo.dfsnum = bbinfo.semi = ++n;
      bbinfo.label = bb;
      dg->vertex.push_back(bb);
    }

    unsigned bb_dfs_num = bbinfo.dfsnum;

    if (next_succ == llvm::succ_end(bb)) {
      worklist.pop_back();
      continue;
    }

    ++worklist.back().second;

    llvm::BasicBlock *succ = *next_succ;
    DominanceInfoRec &succ_vinfo = dg->info[succ];
    if (succ_vinfo.semi == 0) {
      succ_vinfo.parent = bb_dfs_num;
      worklist.push_back(std::make_pair(succ, llvm::succ_begin(succ)));
    }
  }

  return n;
}

// Return a block with the minimum semidominator number of all blocks
// in between the most distance ancestor of bb to be processed and bb.
//
// But if bb hasn't been processed at all, then we "skip" it by just
// returning bb itself.
static llvm::BasicBlock *get_minimum_semidominator(DominanceGraph *dg, llvm::BasicBlock *bb,
                                                   unsigned last_linked) {
  DominanceInfoRec &bbinfo = dg->info[bb];

  if (bbinfo.dfsnum < last_linked) {
    return bb;
  }

  llvm::SmallVector<llvm::BasicBlock *, 32> work;
  llvm::SmallPtrSet<llvm::BasicBlock *, 32> visited;

  if (bbinfo.parent >= last_linked) {
    work.push_back(bb);
  }

  while (!work.empty()) {
    llvm::BasicBlock *v = work.back();
    DominanceInfoRec &v_info = dg->info[v];
    llvm::BasicBlock *v_parent = dg->vertex[v_info.parent];

    if (visited.insert(v_parent) && v_info.parent >= last_linked) {
      work.push_back(v_parent);
      continue;
    }

    work.pop_back();

    if (v_info.parent < last_linked) {
      continue;
    }

    DominanceInfoRec &v_parent_info = dg->info[v_parent];
    llvm::BasicBlock *v_parent_label = v_parent_info.label;
    llvm::BasicBlock *v_label = v_info.label;

    if (dg->info[v_parent_label].semi < dg->info[v_label].semi) {
      v_info.label = v_parent_label;
    }
    
    v_info.parent = v_parent_info.parent;
  }

  return bbinfo.label;
}

// Get the immediate dominator of this block. This should only be called
// during dominance_build, otherwise idoms will be empty. There are other
// methods to get the immediate dominator from the nodes we built.
llvm::BasicBlock *DominanceGraph::get_idom(llvm::BasicBlock *bb) const {
  typename llvm::DenseMap<llvm::BasicBlock*, llvm::BasicBlock*>::const_iterator i =
    this->idoms.find(bb);
  return i != this->idoms.end() ? i->second : 0;
}

DominanceNode *DominanceGraph::get_node(llvm::BasicBlock *bb) const {
  typename node_map_type::const_iterator i = this->nodes.find(bb);
  return i != this->nodes.end() ? i->second : 0;
}

// Get or create a node for this block.
DominanceNode *get_node_for_block(DominanceGraph *dg, llvm::BasicBlock *block) {
  typename DominanceGraph::node_map_type::iterator i = dg->nodes.find(block);
  if (i != dg->nodes.end() && i->second)
    return i->second;

  // Haven't calculated this node yet?  Get or calculate the node for the
  // immediate dominator.
  llvm::BasicBlock *idom = dg->get_idom(block);

  llvm::errs() << "idom: " << idom << " null: " << dg->nodes[NULL] << "\n";
  
  assert(idom || dg->nodes[NULL]);   
  DominanceNode *idom_node = get_node_for_block(dg, idom);
    
  // Add a new tree node for this BasicBlock, and link it as a child of
  // IDomNode
  DominanceNode *child = new DominanceNode(block, idom_node);
  return dg->nodes[block] = idom_node->add_child(child);
}

void DominanceGraph::print_idoms(llvm::raw_ostream &os, std::string title = "") {
  os << "IDoms ---- ";
  if (!title.empty()) {
    os << title << ":";
  }
  os << "\n";

  for (unsigned int i = 0; i < this->vertex.size(); ++i) {
    llvm::BasicBlock *dom = this->get_idom(this->vertex[i]);
    if (this->vertex[i] != 0) {
      os << "DFS num: " << i
         << " " << this->vertex[i]
         << " dominator: " << dom
         << " semidominator: " << this->vertex[this->info[this->vertex[i]].semi]
         << "\n";
    }
  }
}

void DominanceGraph::print_vertices(llvm::raw_ostream &os, std::string title = "") {
  os << "Vertices ---- ";
  if (!title.empty()) {
    os << title << ":";
  }
  os << "\n";
  
  for (unsigned int i = 0; i < this->vertex.size(); i++) {
    os << "DFS num: " << i << " block: " << this->vertex[i] << "\n";
  }
}

// This function works in 5 steps:
//
//   Step 1.
//     Number the blocks in depth-first order and initialize
//     variables used in future steps.
//
//   Step 2.
//     Implicitly define the immediate dominator of vertices.
//
//   Step 3.
//     Calculate "semidominators" of all vertices.
//
//   Step 4.
//     Explicitly define the immediate dominator of vertices.
//
//   Step 5.
//     Build nodes from the immediate dominators found in
//     Step 4 and destroy any intermediate state.
void DominanceGraph::init(llvm::BasicBlock *root) {
  // This is a placeholder for the mapping of a BasicBlock at the
  // depth-first search number 0. It points to a NULL block so that
  // numbering of real blocks can start at 1. See dominance_dfs_num
  // for more details.
  this->vertex.push_back(NULL);
  
  // Step 1.
  //   Do a depth-first search through the root, adding blocks to the
  //   dg->vertex list in that order and assigning the same number to
  //   the block's info.
  //
  // When finished, n will be the highest number assigned to a block and also
  // the number of blocks.
  //
  // This populates dfsnum, label, and parent in the info, but not the
  // semidominator number. That needs to be computed below.
  unsigned n = 0;
  n = build_dominance_info_records(this, root, n);

  // Add the root node to point to no dominator.
  this->idoms[root] = 0;
  this->nodes[root] = 0;

  // These buckets map the dfs number of a block v to the dfs number of another
  // block w, where v is a semidominator of w.
  //
  // Note: Since v can be the semidominator of more than one block, the original
  // Lengauer-Tarjan algorithm uses vertex set to repesent each bucket. Instead
  // we make it so that bucket[i] always contains the vertex of the *next* element
  // to be process for block i. In this way, a bucket acts like an iterator.
  llvm::SmallVector<unsigned, 32> buckets;
  
  // Skip the first element, it's a placeholder.
  buckets.resize(n + 1);
  for (unsigned i = 1; i <= n; ++i) {
    // Start by claiming that block i is a semidominator of itself, and is the
    // first block to process.
    buckets[i] = i;
  }

  // Step 2.
  //   Implicitly define the immediate dominator of vertices.
  //
  // Starting at n, process blocks in decreasing dfs order, stopping before
  // the first two blocks (the first is a placeholder and the second is the root).
  for (unsigned i = n; i >= 2; --i) {
    // Get the block at this dfs number.
    llvm::BasicBlock *w = this->vertex[i];

    // Get the scratch data for this block, where we will store all the
    // temporary state about this block as we find its immediate dominator.
    // Note that the [] operator of llvm::DenseMap inserts a freshly constructed
    // DominanceInfoRec if w is not mapped. See FindAndConstruct in DenseMap.
    DominanceInfoRec &winfo = this->info[w];

    // Iterate through the blocks that this block semidominates until we find
    // ourselves back at this block. Note that when this loop begins, since we
    // assigned all buckets to point to themselves, this will be initially
    // skipped. We'll fall into this logic once we encouter a block that was
    // processed as a semidominator, see the last step in this loop.
    for (unsigned j = i; buckets[j] != i; j = buckets[j]) {
      // Get the semidominated block.
      llvm::BasicBlock *v = this->vertex[buckets[j]];
      
      // Get the block between the root and v with the minimum semidominator,
      // or just v if we haven't processed v yet.
      llvm::BasicBlock *u = get_minimum_semidominator(this, v, i + 1);

      // Does the semidominator of u come before w in the depth-first search
      // ordering?
      //
      // This logic is a big fishy ... what exactly is going on here? It would
      // be nice to do a little illustration of the algorithm in an animated
      // graphic to show how things change.
      if (this->info[u].semi < i) {
        // Then v and u have the same dominator.
        this->idoms[v] = u;
      } else {
        // Otherwise w is both a semidominator of v and its immediate dominator.
        this->idoms[v] = w;
      }
    }

    // Step 3.
    //   Calculate the semidominators of this block and fill in the buckets.
    //
    // This is what powers the computation above, which traverses the buckets.
    // For some reason (??) we want to run the code that implicitly finds the
    // dominators first.
    //
    // Start by assigning the semidominator of this block to be its direct
    // parent. This value was computed by dominance_dfs_num.
    winfo.semi = winfo.parent;

    // Iterate through blocks that point *to* this block. They might be outside
    // the scope of the root block, and if so, we shouldn't expect to find them
    // in the vertex or info map.
    typedef llvm::GraphTraits<llvm::Inverse<llvm::BasicBlock *> > InverseTraits;
    for (typename InverseTraits::ChildIteratorType ci =
           InverseTraits::child_begin(w),
           e = InverseTraits::child_end(w); ci != e; ++ci) {
      llvm::BasicBlock *bb = *ci;
      // Is this precedessor outside the scope of this analysis?
      if (!this->info.count(bb)) {
        // Then skip it.
        continue;
      }

      // Otherwise like above get the block with the lowest semidominator number
      // between the root and bb.
      llvm::BasicBlock *u = get_minimum_semidominator(this, bb, i + 1);      

      // Is the minimum semidominator number of u even less than this block's
      // semidominator (default above is its parent's dfs number)?
      unsigned semiu = this->info[u].semi;
      if (semiu < winfo.semi) {
        // Then use the lower semidominator.
        winfo.semi = semiu;
      }
    }

    // Is the semidominator of this block still its parent?
    if (winfo.semi == winfo.parent) {
      // Then implicitly make this block's immediate dominator its parent.
      this->idoms[w] = this->vertex[winfo.parent];
    } else {
      // Otherwise push this block and its semidominator into their respective
      // buckets so that they map to each other. This makes the current
      // semidominator the next value to be processed for this block.
      buckets[i] = buckets[winfo.semi];
      buckets[winfo.semi] = i;
    }
  }

  // Now process the root.
  //
  // Go through blocks that are semidominated by the root.
  unsigned root_i = 1;
  for (unsigned j = root_i; buckets[j] != root_i; j = buckets[j]) {
    // Get a pointer to the semidominated block.
    unsigned semidominated_i = buckets[j];
    llvm::BasicBlock *v = this->vertex[semidominated_i];
    
    // Claim that they're also immediately dominated by the root.
    this->idoms[v] = root;
  }

  // Step 4.
  //   Explicitly define the immediate dominator of vertices.
  //
  // Like above, starting at the last dfs number, process blocks in decreasing
  // dfs order, stopping before the root.
  for (unsigned i = 2; i <= n; ++i) {
    // Get the block at this dfs number.
    llvm::BasicBlock *w = this->vertex[i];
    
    // Get the implicit immediate dominator of this block.
    llvm::BasicBlock *idomw = this->idoms[w];

    // Get the semidominator of this block.
    llvm::BasicBlock *semiw = this->vertex[this->info[w].semi];
    
    // Is the implicit immediate dominator not its semidominator?
    if (idomw != semiw) {
      // Then reassign this block's immediate dominator to be the same as
      // the immediate dominator of its old immediate dominator.
      this->idoms[w] = this->get_idom(idomw);
    }
  }

  // Organize idoms and info into into DominanceNodes.
  //
  // First, make the root node.
  this->nodes[root] = this->root_node = new DominanceNode(root, 0);

  // Then make children of root.
  for (unsigned i = 2; i <= n; ++i) {
    // Get the block at this dfs number.
    llvm::BasicBlock *w = this->vertex[i];

    // Have we already inserted this node?
    DominanceNode *bbnode = this->nodes[w];
    if (bbnode) {
      // Yes, skip it.
      continue;
    }

    // Get the immediate dominator of this block.
    llvm::BasicBlock *immdom = this->get_idom(w);    
    assert(immdom || this->nodes[NULL]);

    // Lookup the node for the immediate dominator of this block
    // or create one if it doesn't exist.
    DominanceNode *idomnode = get_node_for_block(this, immdom);

    // Create a node for this block, setting the immediate dominator
    // node as its dominator.
    DominanceNode *child = new DominanceNode(w, idomnode);

    // Finally add this node as the child of its dominator and the
    // entry at this block.
    this->nodes[w] = idomnode->add_child(child);
  }

  // Free the scratch space. It should probably be made clear that these
  // fields are useless after we actually build the graph.
  this->idoms.clear();
  this->info.clear();
  std::vector<llvm::BasicBlock *>().swap(this->vertex);

  // Finally assign depth-first search numbers to the dominance graph
  // itself. These numbers hang off the DominanceNodes we inserted
  // above. It also updates the state of the graph to support optimizations
  // based on these numbers, until a new node is added, which then makes
  // these dfs numbers stale.
  this->rebuild_dfs_numbers();
}

// See comments in analysis.h.
void DominanceGraph::rebuild_graph(llvm::BasicBlock *new_root) {
  // Wipe the entire DominanceGraph clean. The same function is called
  // when the DominanceGraph is destroyed.
  this->reset();
  this->init(new_root);
}

// See comments in analysis.h.
void DominanceGraph::rebuild_dfs_numbers() {
    DominanceNode *r = root_node;
    if (!r) {
      return;
    }

    unsigned dfs_num = 0;   
    llvm::SmallVector<std::pair<DominanceNode*,
                                typename DominanceNode::iterator>, 32> workstack;

    workstack.push_back(std::make_pair(r, r->begin()));
    r->dfs_num_in = dfs_num++;

    while (!workstack.empty()) {
      DominanceNode *node = workstack.back().first;
      typename DominanceNode::iterator it = workstack.back().second;

      if (it == node->end()) {
        node->dfs_num_out = dfs_num++;
        workstack.pop_back();
      } else {
        DominanceNode *child = *it;
        ++workstack.back().second;

        workstack.push_back(std::make_pair(child, child->begin()));
        child->dfs_num_in = dfs_num++;
      }
    }

    slow_queries = 0;
    dfs_info_valid = true;
}

// These are methods which answer questions about dominance.

// Returns true iff a dominates b. This function will crash if dg
// does not contain both a and b.
//
// This is the slow version of DominanceNode->dominated_by().
static bool dominated_by_slow(DominanceGraph *dg, const DominanceNode *a,
                    const DominanceNode *b) {
  // Are they undefined?
  if (a == 0 || b == 0) {
    return false;
  }

  // Walk up the tree.
  const DominanceNode *idom;
  while ((idom = b->idom) != 0 && idom != a && idom != b) {
    b = idom;
  }

  // Hunh? What is this doing?
  return idom != 0;  
}

// Returns true iff a dominates b and a != b. This function will
// crash if dg does not contain both a and b.
static bool properly_dominates(DominanceGraph *dg, const DominanceNode *a,
                               const DominanceNode *b) {
  return dominated_by_slow(dg, a, b);
}

// Returns true iff a dominates b and a != b. This function will
// crash if dg does not contain both a and b.
bool properly_dominates(DominanceGraph *dg, const llvm::BasicBlock *a,
                        const llvm::BasicBlock *b) {
  // Is this a dumb, trivial match?
  if (a == b) {
    return false;
  }

  return properly_dominates(dg,
                            dg->get_node(const_cast<llvm::BasicBlock *>(a)),
                            dg->get_node(const_cast<llvm::BasicBlock *>(b)));
}

// Returns true iff a dominates b. This function will crash if dg
// does not contain both a and b.
static bool dominates(DominanceGraph *dg, const DominanceNode *a,
                      const DominanceNode *b) {
  // Do the nodes trivially match?
  if (b == a) {
    return true;
  }

  // Are they undefined?
  if (a == 0 || b == 0) {
    return false;
  }

  // Are the dfs numbers on nodes valid for the whole graph?
  if (dg->dfs_info_valid) {
    // Great, take the fast path.
    return b->dominated_by(a);
  }

  // Count this as a slow query.
  dg->slow_queries++;

  // Are we above the threshold of slow operations?
  if (dg->slow_queries > dg->slow_threshold) {
    // Rebuild the depth-first search numbers to optimize the graph.
    // This resets the value of slow_queries to 0.
    dg->rebuild_dfs_numbers();

    // Now we can take the fast-path.
    return b->dominated_by(a);
  }

  // Dang, we need to walk the whole graph.
  return dominated_by_slow(dg, a, b);
}

// Returns true iff a dominates b. If dg doesn't contain a and b then
// this will return false.
bool dominates(DominanceGraph *dg, const llvm::BasicBlock *a,
               const llvm::BasicBlock *b, bool strict = false) {
  DominanceNode *node_a = dg->get_node(const_cast<llvm::BasicBlock *>(a));
  DominanceNode *node_b = dg->get_node(const_cast<llvm::BasicBlock *>(b));

  if (strict) {
    assert(node_a && "Potentially dominating block not in graph?");
    assert(node_b && "Potentially dominated block not in graph?");
  }

  return dominates(dg, node_a, node_b);
}

// Return the nearest common dominator BasicBlock for a and b, or
// NULL if there is no such possible block.
llvm::BasicBlock *nearest_common_dominator(DominanceGraph *dg, llvm::BasicBlock *a,
                                           llvm::BasicBlock *b, bool strict = false) {
  if (strict) {
    assert(a->getParent() == b->getParent() && "Two blocks are not in the same function");
  }

  if (a->getParent() != b->getParent()) {
    return NULL;
  }

  llvm::BasicBlock &entry = a->getParent()->front();
  if (a == &entry || b == &entry) {
    return &entry;
  }

  if (dominates(dg, b, a, strict))
      return b;

  if (dominates(dg, a, b, strict))
      return a;

  DominanceNode *node_a = dg->get_node(const_cast<llvm::BasicBlock *>(a));
  DominanceNode *node_b = dg->get_node(const_cast<llvm::BasicBlock *>(b));

  llvm::SmallPtrSet<DominanceNode*, 16> node_a_idoms;
  node_a_idoms.insert(node_a);
  DominanceNode *idom_a = node_a->idom;
  while (idom_a) {
    node_a_idoms.insert(idom_a);
    idom_a = idom_a->idom;
  }

  DominanceNode *idom_b = node_b->idom;
  while (idom_b) {
    if (node_a_idoms.count(idom_b) != 0) {
      return idom_b->block;
    }

    idom_b = idom_b->idom;
  }

  return NULL;
}
