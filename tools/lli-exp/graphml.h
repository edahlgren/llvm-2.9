//===- graphml.h - GraphML types ------------------------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#ifndef GRAPHML_H
#define GRAPHML_H

#include "cfg.h"

#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/graphml.hpp>

// Define properties for vertices.
struct VertexProperties {
  std::string name;
  bool root;
  unsigned int refs;
};

// Define properties for edges.
struct EdgeProperties {
  int order;
  bool unconditional;
};

// Define the graph.
typedef boost::adjacency_list<
  // The kind of container will be used to represent the vertex set, here
  // vecS maps to std::vector.
  //
  // I'm not sure whether this matters when used just to emit GraphML. In
  // the future it may be interesting to dig into setS versus listS, etc.
  boost::vecS,
  // The kind of container will be used to store the out-edges (and possibly
  // in-edges) for each vertex in the graph. Also maps to std::vector.
  //
  // Otherwise same as above.
  boost::vecS,
  // Selecting directedS or bidirectionalS choose a directed graph, whereas
  // undirectedS selects the representation for an undirected graph.
  boost::directedS,
  // This is the set of properties attached to each vertex.
  VertexProperties,
  // This is the set of properties attached to each edge.
  EdgeProperties
  > GraphML;

// Define iterators over vertices and edges.
typedef boost::graph_traits<GraphML>::vertex_iterator v_iterator;
typedef boost::graph_traits<GraphML>::edge_iterator e_iterator;

GraphML *make_function_graphml(FunctionGraph *fg);
void function_graphml_dynamic_properties(GraphML *g, boost::dynamic_properties *dp);

#endif // end GRAPHML_H
