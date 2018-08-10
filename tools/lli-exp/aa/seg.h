//===- seg.h - Sparse Evaluation Graph ------------------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#ifndef SEG_H
#define SEG_H

#include "int.h"     // for u32

#include "llvm/Support/raw_ostream.h" // for llvm::raw_ostream

#include <cassert>   // for assert
#include <vector>    // for std::vector
#include <algorithm> // for std::find

class SEGIndexSet {
public:
  std::vector<u32> set;
  typedef std::vector<u32>::iterator iterator;

  iterator begin() { return set.begin(); }
  iterator end() { return set.end(); }

  unsigned size() {
    return set.size();
  }

  bool contains(u32 i) {
    return std::find(set.begin(), set.end(), i) != set.end();
  }

  void insert(u32 i) {
    if (contains(i))
      return;
    
    set.push_back(i);
  }

  void remove(u32 i) {
    iterator it = std::find(set.begin(), set.end(), i);
    if (it != set.end()) {
      *it = set.back();
      set.pop_back();
    }
  }

  void clear() { set.clear(); }

  void destructive_copy(SEGIndexSet& rhs) {
    set.swap(rhs.set);
    rhs.set.clear();
  }
  
  void operator|=(const SEGIndexSet& rhs)
  {
    set.insert(set.end(), rhs.set.begin(), rhs.set.end());
  }
};

enum SEGNodeType {
  PNODE = 0, // (p)reserves state
  MNODE,     // (m)odifies state
};

class SEGNode {
public:
  // Nodes that preceed this node.
  SEGIndexSet pred;

  // Nodes that succeed this node.
  SEGIndexSet succ;

  // Type of the node (e.g. preserving or modifying).
  SEGNodeType type;

  // Depth-first search order of this node.
  u32 dfs_num;

  // Used to find strong-connected-components.
  bool del;

  // The index of the node that represents this node.
  // If this is greater than the number of nodes in the
  // graph, then this node represents itself.
  u32 rep;

  SEGNode(SEGNodeType type) : type(type), rep(MAX_U32) {}

  u32 rank() { return MAX_U32 - rep; }
  
  bool is_rep() { return rep == MAX_U32; }

  bool unreachable() { return rep == 0; }

  std::string to_string(int l = 0) {
    std::string str;
    llvm::raw_string_ostream os(str);

    os.indent(l) << "type: ";
    if (type == PNODE) {
      os << "PNODE";
    } else if (type == MNODE) {
      os << "MNODE";
    } else {
      assert(false && "unknown node type");
    }
    os.indent(l) << "\n";

    os.indent(l) << "dfs_num: " << dfs_num << "\n";
    os.indent(l) << "del: " << del << "\n";

    if (is_rep()) {
      os.indent(l) << "rep: self" << "\n";
    } else {
      os.indent(l) << "rep: " << rep << "\n";
    }
    os.indent(l) << "rank: " << rank() << "\n";

    os.indent(l) << "unreachable: " << unreachable() << "\n";

    if (pred.size() == 0) {
      os.indent(l) << "predecessors {}" << "\n";
    } else {
      os.indent(l) << "predecessors {" << "\n";
      for (SEGIndexSet::iterator i = pred.begin(),
             e = pred.end(); i != e; i++) {
        u32 index = *i;
        os.indent(l + 1) << index << "," << "\n";
      }
      os.indent(l) << "}" << "\n";
    }

    if (succ.size() == 0) {
      os.indent(l) << "successors {}" << "\n";
    } else {
      os.indent(l) << "successors {" << "\n";
      for (SEGIndexSet::iterator i = succ.begin(),
             e = succ.end(); i != e; i++) {
        u32 index = *i;
        os.indent(l + 1) << index << "," << "\n";
      }
      os.indent(l) << "}";
    }
    
    return os.str();
  }
};

class SEG {
public:
  u32 start;
  u32 max_size;

  std::vector<SEGNode *> nodes;
  typedef std::vector<SEGNode *>::iterator iter;
  
  SEG(u32 max_size) : max_size(max_size) {
    assert(max_size < MAX_U32);
    create_node(PNODE);
    start = create_node(PNODE);
  }

  ~SEG() {
    for (iter i = nodes.begin(), e = nodes.end();
         i != e; i++) {
      SEGNode *node = *i;
      delete node;
    }
  }

  iter begin() { return std::next(nodes.begin()); }
  iter end() { return nodes.end(); }

  u32 create_node(SEGNodeType type) {
    u32 index = nodes.size();
    nodes.push_back(new SEGNode(type));
    assert(nodes.size() < max_size);
    return index;
  }

  bool add_edge(u32 src_index, u32 dest_index) {
    validate(src_index);
    validate(dest_index);

    if (src_index == dest_index)
      return false;

    SEGNode *dest = nodes[dest_index];
    dest->pred.insert(src_index);
    return true;
  }

  void rm_edge(u32 src_index, u32 dest_index) {
    validate(src_index);
    validate(dest_index);
    
    SEGNode *node = nodes[dest_index];
    node->pred.remove(src_index);
  }

  void validate(u32 index) {
    assert(index > 0 &&
           "invalid index: 0");
    assert(index < nodes.size() &&
           "invalid index: greater than graph size");

    SEGNode *node = nodes[index]; 
    assert(node->rep > 0 &&
           "invalid index: node was deleted");
    assert(node->rep > max_size &&
           "invalid index: not a set rep");
  }

  bool unreachable(u32 index) {
    return !index || nodes[index]->rep == 0;
  }
  
  SEGNode *get_node(u32 index) {
    validate(index);
    return nodes[index];
  }
  
  u32 get_rep_index(u32 index) {
    validate(index);

    SEGNode *node = nodes[index];
    if (node->is_rep()) {
      return index;
    }

    node->rep = get_rep_index(node->rep);
    return node->rep;
  }

  SEGNode *get_rep_node(u32 index) {
    return get_node(get_rep_index(index));
  }

  void reset() {
    for (u32 i = 1; i < nodes.size(); i++) {
      SEGNode *node = nodes[i];
      node->type = PNODE;
      node->del = false;
      node->dfs_num = 0;
      node->rep = MAX_U32;
      node->succ.clear();
    }    
  }

  void print(llvm::raw_ostream &os, int l = 0) {
    os << "SEG {" << "\n";
    os.indent(l + 1) << "start: " << start << "\n";
    os.indent(l + 1) << "max_size: " << max_size << "\n";
    os.indent(l + 1) << "nodes {" << "\n";
    for (u32 i = 0; i < nodes.size(); i++) {
      SEGNode *node = nodes[i];
      os.indent(l + 2) << i << " {" << "\n";
      os << node->to_string(l + 3) << "\n";
      os.indent(l + 2) << "}" << "\n";
    }
    os.indent(l + 1) << "}" << "\n";
    os << "}" << "\n";
  }
  
  void print(int indent_level = 0) {
    print(llvm::outs(), indent_level);
  }

  //void print(std::ostream &os);
  //bool node_survives_reduction(SEGNode *node);
};

#endif // end SEG_H
