//===- traits.h - Specializations of GraphTraits -------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/DepthFirstIterator.h"
#include "llvm/ADT/GraphTraits.h"

template <>
struct GraphTraits<DominanceNode *> {
  typedef DominanceNode NodeType;
  typedef NodeType::iterator  ChildIteratorType;

  static NodeType *getEntryNode(NodeType *N) {
    return N;
  }
  static inline ChildIteratorType child_begin(NodeType *N) {
    return N->begin();
  }
  static inline ChildIteratorType child_end(NodeType *N) {
    return N->end();
  }

  typedef df_iterator<DominanceNode *> nodes_iterator;

  static nodes_iterator nodes_begin(DominanceNode *node) {
    return df_begin(getEntryNode(node));
  }

  static nodes_iterator nodes_end(DominanceNode *node) {
    return df_end(getEntryNode(node));
  }
};

template <>
struct GraphTraits<DominanceGraph *> : public GraphTraits<DominanceNode *> {
  static NodeType *getEntryNode(DominanceGraph *g) {
    return g->root_node;
  }

  static nodes_iterator nodes_begin(DominanceGraph *g) {
    return df_begin(getEntryNode(g));
  }

  static nodes_iterator nodes_end(DominanceGraph *g) {
    return df_end(getEntryNode(g));
  }
};
