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

#include <cstdint>
#include <iostream>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/graphml.hpp>

#ifdef BOOST_NO_EXCEPTIONS
void
boost::throw_exception(std::exception const& ex)
{
  std::cout << ex.what() << std::endl;
  abort();
}
#endif

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
  //typedef std::unordered_map<std::string,double>::const_iterator functions_iterator;
  
  // Iterate through all of the functions.
  for (FunctionsMap::iterator i = fg->functions.begin(), e = fg->functions.end(); i != e; ++i) {
    // Get the source function.
    llvm::CallGraphNode *node = i->second; 
    llvm::Function *f = node->getFunction();

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
      VertexProperties vp = {};
      vp.name = f->getName();
      vp.root = f == fg->root->getFunction() ? true : false;
      vp.refs = node->getNumReferences();

      source = boost::add_vertex(vp, *g);
      functions[id] = source;
    }

    // Iterate through callees (targets).
    int order = 0;
    for (llvm::CallGraphNode::const_iterator j = node->begin(), e = node->end(); j != e; ++j) {
      // Get a target function.
      llvm::CallGraphNode *ni = j->second;
      llvm::Function *fi = ni->getFunction();

      // Get a pointer to the target.
      Graph::vertex_descriptor target;
      if (!fi) {
        // This a dud. We don't know where it points to because it's likely outside
        // of this module. We'll use a placeholder vertex with a ?? name.
        VertexProperties vp = {};
        vp.name = "??";
        vp.root = false;
        vp.refs = 1;

        target = boost::add_vertex(vp, *g);        
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
          VertexProperties vp = {};
          vp.name = fi->getName();
          vp.root = fi == fg->root->getFunction() ? true : false;
          vp.refs = ni->getNumReferences();

          target = boost::add_vertex(vp, *g);
          functions[id] = target;
        }
      }

      // Create the edge metadata.
      EdgeProperties ep = {};
      
      // This is the call order according the order of the BasicBlocks
      // in the source function.
      //
      // FIXME: We should probably order calls a bit differently to handle
      // conditionals.
      ep.order = order++;

      // This determines whether we can unconditionally get from the
      // BasicBlock containing the source function to the BasicBlock
      // containing the target function. If we can't, then we need to mark
      // this function as such.
      //
      // Duds are always unconditionally called because we can't see
      // inside the source to know any different. This is also the only
      // case where the Instruction pointer should be null.
      if (!fi) {
        ep.unconditional = true;
      } else {      
        // Grab the Instruction pointer.
        llvm::Instruction *inst = j->first;
        assert(inst && "Instruction is bogus");
        
        // Get the basic block containing the target function.
        llvm::BasicBlock *bb = inst->getParent();

        // Compute whether the basic block can be unconditionally reached
        // inside of f.
        ep.unconditional = unconditional_path(f, bb);
      }
      
      // Finally add the edge.
      boost::add_edge(source, target, ep, *g);
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
  dp.property("order", get(&EdgeProperties::order, *g));
  dp.property("unconditional", get(&EdgeProperties::unconditional, *g));

  boost::write_graphml(os, *g, dp, true);

  delete g;
}