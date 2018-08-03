//===- nodes.h - Nodes in the constraint graph ----------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#ifndef NODES_H
#define NODES_H

#include "bitmap.h" // local
#include "int.h"    // u32

#include "llvm/ADT/DenseMap.h" // for llvm::DenseMap
#include "llvm/Module.h"       // for llvm::Function
#include "llvm/Value.h"        // for llvm::Value

#include "bdd.h"    // from libbdd-dev

#include <vector> // for std::vector
#include <set>    // for std::set

// Remap to a convenient type name.
typedef unsigned int u32;

#define NODE_RANK_MIN	0xf0000000

enum SpecialNodes {
  NodeNone = 0,             // no node, used for errors?
  NodeUnknownTarget,        // unknown target of pointers cast from int (i2p)
  NodeConstToUnknownTarget, // const pointer to 1 (p_i2p)
  NodeFirst,                // first node representing a real variable
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

 Node(llvm::Value *v = 0, u32 s = 0, bool w = 0) :
    val(v), obj_sz(s), vtime(0),
    rep(NODE_RANK_MIN), nonptr(0), weak(w) {}

  bool is_rep() const{
    return rep >= NODE_RANK_MIN;
  }    
};

#define FUNC_NODE_OFF_RET 1
#define FUNC_NODE_OFF_ARG0 2

class Nodes {
public:
  u32 next;
  std::vector<Node *> nodes;

  // Map values to their node ID.
  llvm::DenseMap<llvm::Value*, u32> value_nodes;

  // Map values to the first node ID of its object,
  // (if it has an object).
  llvm::DenseMap<llvm::Value*, u32> object_nodes;
  
  llvm::DenseMap<llvm::Function*, u32> ret_nodes;
  llvm::DenseMap<llvm::Function*, u32> vararg_nodes;
  
  Nodes() {}

  u32 rep(u32 i) {
    u32 &r0= nodes[i]->rep;
    if(r0 >= NODE_RANK_MIN) {
      //If i has a rank, it is the rep.
      return r0;
    }
    
    //Recurse on the parent to get the real rep.
    u32 r= this->rep(r0);

    // Set i's parent to the rep
    r0 = r;
    return r;
  }

  void add_unreachable(llvm::Value *v, u32 obj_sz = 0, bool weak = false) {
    nodes.push_back(new Node(v, obj_sz, weak));    
  }

  u32 add_value(llvm::Value *v, u32 obj_sz = 0, bool weak = false) {
    u32 id = nodes.size();
    nodes.push_back(new Node(v, obj_sz, weak));
    value_nodes[v] = id;
    return id;
  }

  u32 add_object(llvm::Value *v, u32 obj_sz = 0, bool weak = false) {
    u32 id = nodes.size();
    nodes.push_back(new Node(v, obj_sz, weak));
    object_nodes[v] = id;
    return id;
  }

  u32 add_ret(llvm::Function *f, u32 obj_sz = 0, bool weak = false) {
    u32 id = nodes.size();
    nodes.push_back(new Node(f, obj_sz, weak));
    ret_nodes[f] = id;
    return id;
  }

  u32 add_vararg(llvm::Function *f, u32 obj_sz = 0, bool weak = false) {
    u32 id = nodes.size();
    nodes.push_back(new Node(f, obj_sz, weak));
    vararg_nodes[f] = id;
    return id;
  }

  Node *find_node(u32 id) {
    assert(id > 0 && id < nodes.size());
    return nodes[id];
  }

  u32 find_value_node(llvm::Value *v, bool allow_null = false) const {
    assert(v);

    llvm::DenseMap<llvm::Value *, u32>::const_iterator i =
      value_nodes.find(v);
    if (i == value_nodes.end()) {
      if (allow_null) {
        return 0;
      } else {      
        assert(false && "Failed to find value");
      }
    }

    assert(i->second && "Failed to find non-zero value");
    return i->second;
  }
  
  u32 find_object_node(llvm::Value *v, bool allow_null = false) const {
    assert(v);
    
    llvm::DenseMap<llvm::Value *, u32>::const_iterator i =
      object_nodes.find(v);
    if (i == value_nodes.end()) {
      if (allow_null) {
        return 0;
      } else {      
        assert(false && "Failed to find value");
      }
    }

    assert(i->second && "Failed to find non-zero value");
    return i->second;
  }
  
  u32 find_ret_node(llvm::Function *f) const {
    assert(f);

    const llvm::Type *typ = f->getFunctionType()->getReturnType();
    if (!llvm::isa<llvm::PointerType>(typ))
      return 0;
   
    llvm::DenseMap<llvm::Function*, u32>::const_iterator i =
      ret_nodes.find(f);
    if (i == ret_nodes.end()) {
      u32 obj_node_id = find_object_node(f, true);
      assert(obj_node_id && "Missing ret_nodes entry");
      return obj_node_id + FUNC_NODE_OFF_RET;
    }

    assert(i != ret_nodes.end());
    assert(i->second && "Failed to find non-zero ret node");
    return i->second;
  }
  
  u32 find_vararg_node(llvm::Function *f) const {
    assert(f);

    if (!f->getFunctionType()->isVarArg())
      return 0;
   
    llvm::DenseMap<llvm::Function*, u32>::const_iterator i =
      vararg_nodes.find(f);
    if (i == vararg_nodes.end()) {
      u32 obj_node_id = find_object_node(f, true);
      assert(obj_node_id && "Missing varargs_nodes entry");
      return obj_node_id + nodes[obj_node_id]->obj_sz - 1;
    }

    assert(i != vararg_nodes.end());
    assert(i->second && "Failed to find non-zero vararg node");
    return i->second;
  }
  
  u32 pe(llvm::Value* v) {
    u32 n = find_value_node(v, true);
    if (!n)
      return MAX_U32;
    
    return rep(n);
  }

  u32 pe(u32 n) {
    assert(n && n < nodes.size() && "node ID out of range");
    return rep(n);
  }  
  
  u32 merge(u32 a, u32 b) {
    // TODO: implement.
    return a;
  }

  void validate() {
    // TODO: implement.
    return;
  }

};

typedef llvm::DenseMap<u32, u32> NodeMap;

#endif // end NODES_H
