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

class DominanceNode {
public:
  // 1. State.
  llvm::BasicBlock *block;
  DominanceNode *idom;
  std::vector<DominanceNode *> children;
  int dfs_num_in, dfs_num_out;

  // 2. Construction.
  DominanceNode(llvm::BasicBlock *bb, DominanceNode *dom)
    : block(bb), idom(dom), dfs_num_in(-1), dfs_num_out(-1) {}

  // 3. Destruction.
  ~DominanceNode() {}

  // 4. Iteration.
  typedef std::vector<DominanceNode *>::iterator iterator;
  typedef std::vector<DominanceNode *>::const_iterator const_iterator;
  iterator begin() { return children.begin(); }
  iterator end() { return children.end(); }
  const_iterator begin() const { return children.begin(); }
  const_iterator end() const { return children.end(); }

  // 5. Utility methods (minimal).
  void set_idom(DominanceNode *new_idom) {
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

  DominanceNode *add_child(DominanceNode *child) {
    children.push_back(child);
    return child;
  }

  bool compare(DominanceNode *other) {
    if (children.size() != Other->children.size())
      return true;

    llvm::SmallPtrSet<llvm::BasicBlock *, 4> other_children;
    for (iterator i = other->begin(), e = other->end(); i != e; ++i) {
      llvm::BasicBlock *nd = (*i)->block;
      other_children.insert(nd);
    }

    for (iterator i = begin(), e = end(); i != e; ++i) {
      llvm::BasicBlock *n = (*i)->block;
      if (other_children.count(n) == 0)
        return true;
    }
    return false;
  }

  bool dominated_by(DominanceNode *other) {
    return dfs_num_in >= other->dfs_num_in &&
      dfs_num_out <= other->dfs_num_out;
  }  
};

struct DominanceInfoRec {
  // 1. State
  unsigned dfsnum;
  unsigned parent;
  unsigned semi;
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

class DominanceGraph {
public:
  // 1. State.
  typedef llvm::DenseMap<llvm::BasicBlock *, DominanceNode *> node_map_type;
  node_map_type nodes;

  DominatorNode *root_node;
  std::vector<llvm::BasicBlock *> roots;
  const bool is_post;

  bool dfs_info_valid;
  unsigned int slow_queries;

  llvm::DenseMap<llvm::BasicBlock *, llvm::BasicBlock *> idoms;
  std::vector<llvm::BasicBlock *> vertex;
  llvm::DenseMap<llvm::BasicBlock *, DominanceInfoRec> info;

  // 2. Construction.
  DominanceGraph(bool post)
    : is_post(post), dfs_info_valid(false), slow_queries(0) {}

  // 3. Deconstruction.
  ~DominatorTreeBase() {
    reset();
  }

  // 4. Iteration.
  // (none)

  // 5. Utility methods (minimal).
  void reset() {
    for (typename node_map_type::iterator i = nodes.begin(), e = nodes.end();
         i != e; ++i) {
      delete i->second;
    }    
    nodes.clear();

    idoms.clear();
    roots.clear();
    vertex.clear();

    root = 0;
  }

  inline llvm::BasicBlock *get_idom(llvm::BasicBlock *bb) const {
    typename llvm::DenseMap<llvm::BasicBlock*, llvm::BasicBlock*>::const_iterator i =
      idoms.find(bb);
    return i != idoms.end() ? i->second : 0;
  }

  DominanceNode *get_node_for_block(llvm::BasicBlock *block) {
    typename node_map_type::iterator i = nodes.find(block);
    if (i != nodes.end() && i->second)
      return i->second;

    // Haven't calculated this node yet?  Get or calculate the node for the
    // immediate dominator.
    llvm::BasicBlock *idom = get_idom(block);
    assert(idom || nodes[NULL]);   
    DominanceNode *idom_node = get_node_for_block(idom);
    
    // Add a new tree node for this BasicBlock, and link it as a child of
    // IDomNode
    DominanceNode *child = new DominanceNode(block, idom_node);
    return nodes[block] = idom_node->add_child(child);
  }

  void update_dfs_numbers() {
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
}

#endif // end ANALYSIS_H
