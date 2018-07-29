//===- nodes.h - Nodes in the constraint graph ----------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#ifndef NODES_H
#define NODES_H

#include "llvm/ADT/DenseMap.h" // for llvm::DenseMap
#include "llvm/Module.h"       // for llvm::Function
#include "llvm/Value.h"        // for llvm::Value

#include "bdd.h"    // from libbdd-dev
#include "bitmap.h" // local

#include <vector> // for std::vector
#include <set>    // for std::set

#define MAX_U32				(u32)-1
#define NODE_RANK_MIN	0xf0000000;

enum SpecialNodes {
  NodeNone = 0,           // no node, used for errors?
  NodeUnknownTarget,        // unknown target of pointers cast from int (i2p)
  NodeConstToUnknownTarget, // const pointer to 1 (p_i2p)
  NodeFirst,            // first node representing a real variable
};

class Node {
public:
  // The LLVM value represented by this node, or 0 for artificial nodes
  llvm::Value *val;

  // Metadata about the node:
  //
  // * Number of nodes in the object that starts here (0 if it's not an obj node).
  u32 obj_sz;
  // * The time this node was last visited
  u32 vtime;
  // * If rep < node_rank_min, this node is part of a set of equivalent nodes
  //     and (rep) is another node in that set.
  //   else this is the representative node of the set,
  //     and (rep) is its rank in the union-find structure.
  u32 rep;
  // * True if this node was determined to not point to anything
  bool nonptr;
  // * True if this is an array or is heap-allocated.
  bool weak;

  // Points-to sets.
  //
  // * The nodes in our points-to set.
  bdd points_to;
  // * The points_to set at the start of the last visit to this node.
  bdd prev_points_to;

  // The simple constraint edges:
  //
  bitmap copy_to;
  // * Indices into cplx_cons for load, store, and gep cons.
  bitmap load_to;
  bitmap store_from;
  bitmap gep_to;


 Node(Value *v= 0, u32 s= 0, bool w= 0):
    val(v), obj_sz(s), vtime(0),
    rep(NODE_RANK_MIN), nonptr(0), weak(w) {}

  bool is_rep() const{
    return rep >= NODE_RANK_MIN;
  }    
};

class Nodes {
public:
  u32 next;
  std::vector<Node *> nodes;
  
  llvm::DenseMap<llvm::Value*, u32> value_nodes;
  llvm::DenseMap<llvm::Value*, u32> object_nodes;
  
  llvm::DenseMap<llvm::Function*, u32> ret_nodes;
  llvm::DenseMap<llvm::Function*, u32> vararg_nodes;

  std::vector<u32> const_gep_nodes;
  
  Nodes() {}

  u32 rep(u32 i) {
    u32 &r0= nodes[i]->rep;
    if(r0 >= NODE_RANK_MIN) {
      //If i has a rank, it is the rep.
      return n;
    }
    
    //Recurse on the parent to get the real rep.
    u32 r= this->rep(r0);
    // Set i's parent to the rep
    r0 = r;
    return r;
  }

  u32 find_value_node(llvm::Value *v, bool allow_null = false) const {
    // TODO: implement.
    return 0;
  }
  
  u32 find_object_node(llvm::Value *v, bool allow_null = false) const {
    // TODO: implement.
    return 0;
  }
  
  u32 find_ret_node(llvm::Function *f) const {
    // TODO: implement.
    return 0;    
  }
  
  u32 find_vararg_node(llvm::Function *f) const {
    // TODO: implement.
    return 0;
  }
  
  void add_double_object_node(llvm::Value *v) {
    // TODO: implement.
    return;
  }
  
  u32 merge(u32 a, u32 b) {
    // TODO: implement.
    return a;
  }

  void validate() {
    // TODO: implement.
    return;
  }

  u32 pe(llvm::Value* v){
    u32 n = this->find_value_node(v, 1);
    if(!n)
      return MAX_U32;
    return this->rep(n);
  }

  u32 pe(u32 n){
    assert(n && n < this->nodes.size() && "node ID out of range");
    return this->rep(n);
  }  
};

typedef llvm::DenseMap<u32, u32> NodeMap;

#endif // end NODES_H
