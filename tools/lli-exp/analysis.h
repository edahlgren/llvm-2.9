//===- analysis.h - Header for lli-exp --------------------------*- C++ -*-===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#ifndef ANALYSIS_H
#define ANALYSIS_H

#include "llvm/Analysis/CallGraph.h"
#include "llvm/Module.h"
#include "llvm/Support/raw_ostream.h"

#include <iostream>

// ---- Utility interfaces.

const char *instruction_type(llvm::BasicBlock::iterator i);
bool unconditional_path(llvm::Function *f, llvm::BasicBlock *target);

// ---- Print interfaces.

void print_function_control_flow(llvm::Function *f, llvm::raw_ostream &os);
void print_dominator_tree(llvm::Function *f, llvm::raw_ostream &os);
void print_graphml(FunctionGraph *fg, std::ostream &os);

// ---- Caller-callee graphs.

// FunctionsMap maps Functions to a wrapper node. The wrapper node links
// a Function to other functions in a graph.
typedef std::map<const llvm::Function *, llvm::CallGraphNode *> FunctionsMap;

// FunctionGraph contains a call graph of functions.
class FunctionGraph {
public:
  // 1. State.
  llvm::CallGraphNode *root;
  llvm::CallGraphNode *ext;
  llvm::CallGraphNode *calls_ext;
  FunctionsMap functions;
  
  // 2. Construction.
  FunctionGraph() {
    llvm::Function *NullFunction = 0;  
    root = 0;
    ext = new llvm::CallGraphNode(const_cast<llvm::Function *>(NullFunction));
    calls_ext = new llvm::CallGraphNode(0);
  }
  
  // 3. Deconstruction.
  ~FunctionGraph() {
    // Was the CallExt node ever initialized?
    if (calls_ext) {
      // Deallocate the CallsExt node.
      calls_ext->allReferencesDropped();
      delete calls_ext;
      calls_ext = 0;
    }

    // Were functions ever added?
    if (functions.empty()) {
      return;
    }
    
    // Deallocate added CallGraphNodes.
    for (FunctionsMap::iterator i = functions.begin(), e = functions.end();
         i != e; ++i) {
      i->second->allReferencesDropped();
      delete i->second;
      i->second = 0;
    }
    
    // Deallocate the map.
    functions.clear();    
  }

  // 4. Iteration
  // (none)

  // 5. Utility methods
  // (none)
};

void link_function_to_graph(FunctionGraph *fg, llvm::Function *f);

// ---- Dominance graphs.

// This is a vertex in a DominanceGraph (see below). It wraps a BasicBlock
// and points to its immediate dominator and any other BasicBlocks that it
// dominates.
class DominanceNode {
public:
  // 1. State.

  // The BasicBlock that this wraps.
  llvm::BasicBlock *block;

  // The immediate dominator of block.
  DominanceNode *idom;

  // The blocks that block immediately dominates.
  std::vector<DominanceNode *> children;

  // The depth-first search numbers for inward edges and outward edges.
  // These may not be computed or valid at any time, because they depend
  // on the state of the graph that contains this node.
  //
  // A valid number is anything greater than -1 (but by convention we also
  // skip number 0).
  int dfs_num_in, dfs_num_out;

  // 2. Construction.
  DominanceNode(llvm::BasicBlock *bb, DominanceNode *dom)
    : block(bb), idom(dom), dfs_num_in(-1), dfs_num_out(-1) {}

  // 3. Destruction.
  ~DominanceNode() {
    // We don't free anything? Why not?
  }

  // 4. Iteration.
  //
  // Make it easy to iterate over the blocks dominated by this node's block.
  typedef std::vector<DominanceNode *>::iterator iterator;
  typedef std::vector<DominanceNode *>::const_iterator const_iterator;
  iterator begin() { return children.begin(); }
  iterator end() { return children.end(); }
  const_iterator begin() const { return children.begin(); }
  const_iterator end() const { return children.end(); }

  // 5. Utility methods (minimal).
  //
  // Reset the immediate dominator of this node.
  void reset_idom(DominanceNode *new_idom) {
    assert(idom && "No immediate dominator?");
    
    if (idom != new_idom) {
      typename std::vector<llvm::BasicBlock *>::iterator i =
                  std::find(idom->children.begin(), idom->children.end(), this);
      assert(i != idom->children.end() &&
             "Not in immediate dominator children set!");
      // I am no longer your child...
      idom->children.erase(i);

      // Switch to new dominator
      idom = new_idom;
      idom->children.push_back(this);
    }
  }

  // Add a node that is dominated by this node.
  DominanceNode *add_child(DominanceNode *child) {
    children.push_back(child);
    return child;
  }

  // Returns true if other matches this node.
  bool matches(DominanceNode *other) {
    if (children.size() != other->children.size())
      return false;

    llvm::SmallPtrSet<llvm::BasicBlock *, 4> other_children;
    for (iterator i = other->begin(), e = other->end(); i != e; ++i) {
      llvm::BasicBlock *nd = (*i)->block;
      other_children.insert(nd);
    }

    for (iterator i = begin(), e = end(); i != e; ++i) {
      llvm::BasicBlock *n = (*i)->block;
      if (other_children.count(n) == 0)
        return false;
    }
    return true;
  }

  // Returns true if this node is dominated by other. It is only
  // safe to call this method if the depth-first search numbers
  // of this node are valid.
  bool dominated_by(DominanceNode *other) {
    assert(dfs_num_in > 0 && dfs_num_out > 0 && "Invalid dfs numbers?");
    return dfs_num_in >= other->dfs_num_in &&
      dfs_num_out <= other->dfs_num_out;
  }  
};

// This is scratch record for initializing the state of a DominanceNode.
// It is destroyed immediately after DominanceNodes are built.
struct DominanceInfoRec {
  // 1. State

  // The depth-first search number of a BasicBlock in the control flow.
  unsigned dfsnum;

  // The parent of a BasicBlock in the control flow.
  unsigned parent;

  // The semidominator of a BasicBlock in the control flow.
  unsigned semi;

  // ???
  llvm::BasicBlock *label;

  // 2. Construction.
  DominanceInfoRec() : dfsnum(0), parent(0), semi(0), label(0) {}

  // 3. Deconstruction.
  // (none)

  // 4. Iteration.
  // (none)

  // 5. Utility methods.
  // (none)
};

// This graph expresses the "dominance" tree of BasicBlocks starting at
// a single entry point (e.g. a Function).
//
// Calculating this graph is useful because it tells us which BasicBlocks
// are necessarily executed before others. This helps us identify loop
// structure and eliminate common expressions across the whole program.
//
// We only support calculating "pre-dominance", that is, what BasicBlock
// must precede another BasicBlocks. There is also "post-dominance" but
// that isn't supported yet, partly because it's not as helpful and also
// to make the logic simpler.
//
// Not thread-safe.
class DominanceGraph {
public:
  // 1. State.

  // This is the BasicBlock that has no dominating BasicBlock. By default,
  // the entry block of a Function is the root of the DominanceGraph for that
  // Function.
  //
  // Note that this does not support post-dominator graphs, because in that
  // case there can be multiple roots.
  DominatorNode *root_node;

  // These are the edges of the graph: a block points to a single immediately
  // dominating node, which contains a pointer the dominating block. The
  // graph can be traversed upwards like this (towards the root) because
  // it contains pre-dominance relationships.
  typedef llvm::DenseMap<llvm::BasicBlock *, DominanceNode *> node_map_type;
  node_map_type nodes;

  // All of this state is temporary. We build it while initializing the graph
  // to construct the nodes above. It is completely cleared at the end of
  // initialization and/or rebuilding of the graph.

  // This maps the depth-first search order of a BasicBlock in a control flow
  // graph to that BasicBlock.
  std::vector<llvm::BasicBlock *> vertex;

  // This maps BasicBlocks to its intermediate metadata:
  //
  //   - dfs number from the callgraph (see vertex above)
  //   - label ... ???
  //   - parent block
  //   - minimum semidominating block's dfs number
  llvm::DenseMap<llvm::BasicBlock *, DominanceInfoRec> info;

  // This maps BasicBlocks to their immediately dominating BasicBlock.
  llvm::DenseMap<llvm::BasicBlock *, llvm::BasicBlock *> idoms;

  // This state controls optimizations on the graph.

  // Do the nodes in the map above have valid depth-first search numbers
  // assigned to them?
  bool dfs_info_valid;

  // This is the count of traversals that don't take advantage of depth-first
  // search numbers on nodes.  
  unsigned int slow_queries;

  // This is the threshold for updating the depth-first search numbers. See the
  // default below.
  unsigned int slow_query_threshold;

  // 2. Construction.
  DominanceGraph(llvm::BasicBlock *root, unsigned int threshold = 32)
    : dfs_info_valid(false), slow_queries(0), slow_queries_threshold(threshold) {
    dominance_rebuild(this, root);
  }

  // 3. Deconstruction.
  ~DominatorTreeBase() {
    // Blow everything away.
    reset();
  }

  // 4. Iteration.
  // (none)

  // 5. Utility methods (minimal).

  // Initialize the graph so that nodes are BasicBlocks, starting at root,
  // and edges point from a BasicBlock to another BasicBlock that immediately
  // dominates it.
  //
  // This method is only safe to call in the constructor or after a reset.
  //
  // Preconditions:
  //   root must be non-NULL.
  //
  // Based on:
  //   A Fast Algorithm for Finding Dominators in a Flowgraph
  //   T. Lengauer & R. Tarjan, ACM TOPLAS July 1979, pgs 121-141.
  void init(llvm::BasicBlock *root);

  // This clears the memory held by the graph. Once it is called, the graph
  // should either be immediately re-initialized or be destroyed.
  void reset() {
    // For each of the nodes, destroy its unique dominating node.
    for (typename node_map_type::iterator i = nodes.begin(), e = nodes.end();
         i != e; ++i) {
      // Could this ever result in a double free if multiple nodes are
      // immediately dominated by the same node?
      delete i->second;
    }
    // Free the map. This doesn't free BasicBlocks held by the map,
    // as we don't own those.
    nodes.clear();

    // Clear out any temporary storage. This should already be cleared
    // after we rebuilding the graph, but it's better to be safe than sorry.
    idoms.clear();
    info.clear();
    vertex.clear();

    // Allow the root's destructor to be called. The iteration above over the
    // nodes list should already have deleted the root dominator node because
    // at least one node is dominated by it.
    root = 0;
  }

  // Same as init but clear the state of the graph first. This is safe to
  // call at any time whereas init is not.
  void rebuild_graph(llvm::BasicBlock *new_root);

  // Iterate through the nodes in the graph and assign numbers to the nodes
  // in depth-first search order. Calling it is optional but it makes some
  // operations faster.
  void rebuild_dfs_numbers();
}

#endif // end ANALYSIS_H
