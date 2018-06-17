//===- graphml.cpp - GraphML utilities ------------------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#include "analysis.h"

#include "llvm/Analysis/CallGraph.h"
#include "llvm/Support/raw_ostream.h"

#include <iostream>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/graphml.hpp>

// Define properties for vertices.
struct VertexProperties {
  std::string name;
  bool root;
  int refs;
};

// Define properties for edges.
struct EdgeProperties {
  int weight;
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
  EdgeProperties,
  > Graph;

// Define iterators over vertices and edges.
typedef boost::graph_traits<Graph>::vertex_iterator v_iterator;
typedef boost::graph_traits<Graph>::edge_iterator e_iterator;

Graph *make_boost_graph(FunctionGraph *fg) {
  // Create the graph.
  Graph *g = new Graph();

  // Keep a cached of vertices that we've created so that we can add
  // edges to them as we find connections.
  std::unordered_map<uintptr_t, Graph::vertex_descriptor> functions;

  // Iterate through all of the functions.
  for (FunctionsMap::iterator i = fg->functions.begin(), e = fg->functions.end(); i != e; ++i) {
    // Get the source function.
    llvm::Function *f = i->second->getFunction();

    // Get the ID of te function and check if we've seen it before.
    uintptr_t id = reinterpret_cast<std::uintptr_t>(f);
    auto cached_source = functions.find(id);

    // Get a pointer to the source vertex.
    Graph::vertex_descriptor source;
    if (cached_source != functions.end()) {
      // Grab the vertex out of the cache.
      source = cached_source->second;
    } else {
      // Otherwise add the vertex.
      VertexProperties vp = {
        .name = f->getName(),
        .root = f == fg->root->getFunction : true ? false,
        .refs = f->getNumReferences(),
      };
      source = boost::add_vertex(vp, g);
      functions[id] = source;
    }

    // Iterate through callees (targets).
    for (llvm::CallGraphNode::const_iterator i = node->begin(), e = node->end(); i != e; ++i) {
      // Get a target function.
      llvm::Function *fi = i->second->getFunction();

      // Get a pointer to the target.
      Graph::vertex_descriptor target;
      if (!fi) {
        // This a dud. We don't know where it points to because it's likely outside
        // of this module. We'll use a placeholder vertex with a ?? name.
        VertexProperties vp = {
          .name = "??",
          .root = false,
          .refs = 1,
        };
        target = boost::add_vertex(vp, g);        
      } else {
        // Same as above, see if it's cached.
        uintptr_t id = reinterpret_cast<std::uintptr_t>(fi);
        auto cached_target = functions.find(id);

        // Get a pointer to the target vertex.
        if (cached_target != functions.end()) {
          // Grab the vertex out of the cache.
          target = cached_target->second;
        } else {
          // Otherwise add the vertex.
          VertexProperties vp = {
            .name = fi->getName(),
            .root = fi == fg->root->getFunction : true ? false,
            .refs = fi->getNumReferences(),
          };
          target = boost::add_vertex(vp, g);
          functions[id] = target;
        }
      }

      // Finally add an edge between each caller and callee.
      boost::add_edge(source, target, g);
    }
  }

  return g;
}

void print_graphml(FunctionGraph *fg, std::ostream &os) {
  Graph *g = make_boost_graph(fg);

  boost::dynamic_properties dp;
  dp.property("name", get(&VertexProperties::name, *g));
  dp.property("root", get(&VertexProperties::root, *g));
  dp.property("refs", get(&VertexProperties::refs, *g));
  dp.property("weight", get(&EdgeProperties::weight, *g));

  boost::write_graphml(os, *g, dp, true);

  delete g;
}
