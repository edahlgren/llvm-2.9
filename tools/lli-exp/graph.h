//===- graph.h - Graph templates ------------------------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#ifndef GRAPH_H
#define GRAPH_H

#include "llvm/ADT/GraphTraits.h"
#include "llvm/Support/DOTGraphTraits.h"
#include "llvm/Support/raw_ostream.h"

static std::string escape_string(const std::string &Label) {
  std::string Str(Label);
  for (unsigned i = 0; i != Str.length(); ++i)
    switch (Str[i]) {
    case '\n':
      Str.insert(Str.begin()+i, '\\');  // Escape character...
      ++i;
      Str[i] = 'n';
      break;
    case '\t':
      Str.insert(Str.begin()+i, ' ');  // Convert to two spaces
      ++i;
      Str[i] = ' ';
      break;
    case '\\':
      if (i+1 != Str.length())
        switch (Str[i+1]) {
        case 'l': continue; // don't disturb \l
        case '|': case '{': case '}':
          Str.erase(Str.begin()+i); continue;
        default: break;
        }
    case '{': case '}':
    case '<': case '>':
    case '|': case '"':
      Str.insert(Str.begin()+i, '\\');  // Escape character...
      ++i;  // don't infinite loop
      break;
    }
  return Str;
}

namespace llvm {
  
template<typename GraphType>
static bool get_edge_source_labels(const GraphType &g, raw_ostream &os,
                                   DOTGraphTraits<GraphType> dtraits,
                                   typename GraphTraits<GraphType>::NodeType *node) {
  typedef typename GraphTraits<GraphType>::ChildIteratorType child_iterator;
  bool has_labels = false;

  child_iterator ei = GraphTraits<GraphType>::child_begin(node);
  child_iterator ee = GraphTraits<GraphType>::child_end(node);

  for (unsigned i = 0; ei != ee && i != 64; ++ei, ++i) {
    std::string label = dtraits.getEdgeSourceLabel(node, ei);

    if (label.empty())
      continue;

    has_labels = true;

    if (i)
      os << "|";

    os << "<s" << i << ">" << escape_string(label);
  }

  if (ei != ee && has_labels)
    os << "|<s64>truncated...";
  
  return has_labels;
}

template<typename GraphType>
static void write_header(const GraphType &g, raw_ostream &os, DOTGraphTraits<GraphType> dtraits, const std::string &title = "") {
  std::string graph_name = dtraits.getGraphName(g);

  if (!title.empty())
    os << "digraph \"" << escape_string(title) << "\" {\n";
  else if (!graph_name.empty())
    os << "digraph \"" << escape_string(graph_name) << "\" {\n";
  else
    os << "digraph unnamed {\n";

  if (dtraits.renderGraphFromBottomUp())
    os << "\trankdir=\"BT\";\n";

  if (!title.empty())
    os << "\tlabel=\"" << escape_string(title) << "\";\n";
  else if (!graph_name.empty())
    os << "\tlabel=\"" << escape_string(graph_name) << "\";\n";

  os << dtraits.getGraphProperties(g);
  os << "\n";  
}

template<typename GraphType>
static void write_nodes(const GraphType &g, raw_ostream &os,
                        DOTGraphTraits<GraphType> dtraits) {
  typedef typename GraphTraits<GraphType>::nodes_iterator node_iterator;
  
  for (node_iterator i = GraphTraits<GraphType>::nodes_begin(g),
         e = GraphTraits<GraphType>::nodes_end(g); i != e; ++i) {
    typename GraphTraits<GraphType>::NodeType *node = i;
    if (!dtraits.isNodeHidden(node)) {
      write_node(g, os, dtraits, node);
    }
  }
}

template<typename GraphType>
static void write_node(const GraphType &g, raw_ostream &os,
                       DOTGraphTraits<GraphType> dtraits,
                       typename GraphTraits<GraphType>::NodeType *node) {
  std::string attrs = dtraits.getNodeAttributes(node, g);
  std::string label = dtraits.getNodeLabel(node, g);
  bool renderBottomUp = dtraits.renderGraphFromBottomUp();
  bool has_addr_label = dtraits.hasNodeAddressLabel(node, g);

  os << "\tNode" << static_cast<const void*>(node) << " [shape=record,";
  if (!attrs.empty())
    os << attrs << ",";
  os << "label=\"{";

  if (!renderBottomUp) {
    os << escape_string(label);

    // If we should include the address of the node in the label, do so now.
    if (has_addr_label)
      os << "|" << (void*)node;
  }

  std::string edge_source_labels;
  raw_string_ostream estream(edge_source_labels);
  bool has_edge_source_labels = get_edge_source_labels(g, estream, dtraits, node);
  
  if (has_edge_source_labels) {
    if (!renderBottomUp)
      os << "|";

    os << "{" << estream.str() << "}";

    if (renderBottomUp)
      os << "|";
  }

  if (renderBottomUp) {
    os << escape_string(label);

    // If we should include the address of the node in the label, do so now.
    if (has_addr_label)
      os << "|" << (void*)node;
    
    bool has_edge_dest_labels = dtraits.hasEdgeDestLabels();
    if (has_edge_dest_labels) {
      os << "|{";

      unsigned i = 0, e = dtraits.numEdgeDestLabels(node);
      for (; i != e && i != 64; ++i) {
        if (i)
          os << "|";
        std::string label = dtraits.getEdgeDestLabel(node, i);
        os << "<d" << i << ">" << label;
      }
      
      if (i != e)
        os << "|<d64>truncated...";

      os << "}";
    }
  }

  os << "}\"];\n";   // Finish printing the "node" line
    
  // Output all of the edges now
  typedef typename GraphTraits<GraphType>::ChildIteratorType child_iterator;
  child_iterator ei = GraphTraits<GraphType>::child_begin(node);
  child_iterator ee = GraphTraits<GraphType>::child_end(node);
  for (unsigned i = 0; ei != ee && i != 64; ++ei, ++i) {
    bool hidden = dtraits.isNodeHidden(*ei);
    if (!hidden)
      write_edge(g, os, dtraits, node, i, ei);
  }
  for (; ei != ee; ++ei) {
    bool hidden = dtraits.isNodeHidden(*ei);
    if (!hidden)
      write_edge(g, os, dtraits, node, 64, ei);
  }
}

/// emit_edge - Output an edge from a simple node into the graph...
static void emit_edge(raw_ostream &os, const void *src_id, int src_port,
               const void *dest_id, int dest_port,
               const std::string &attrs, bool has_labels) {
  if (src_port  > 64)
    return;             // Eminating from truncated part?
    
  if (dest_port > 64)
    dest_port = 64;  // Targetting the truncated part?

  os << "\tNode" << src_id;
  if (src_port >= 0)
    os << ":s" << src_port;

  os << " -> Node" << dest_id;
  if (dest_port >= 0 && has_labels)
    os << ":d" << dest_port;

  if (!attrs.empty())
    os << "[" << attrs << "]";
  
  os << ";\n";
}

template<typename GraphType>
static void write_edge(const GraphType &g, raw_ostream &os,
                       DOTGraphTraits<GraphType> dtraits,
                       typename GraphTraits<GraphType>::NodeType *node,
                       unsigned edge_idx,
                       typename GraphTraits<GraphType>::ChildIteratorType ei) {
  if (typename GraphTraits<GraphType>::NodeType *target = *ei) {
    int dest_port = -1;
    if (dtraits.edgeTargetsEdgeSource(node, ei)) {
      typename GraphTraits<GraphType>::ChildIteratorType target_it = dtraits.getEdgeTarget(node, ei);
      
      // Figure out which edge this targets...
      unsigned offset = (unsigned)std::distance(GraphTraits<GraphType>::child_begin(target), target_it);
      dest_port = static_cast<int>(offset);
    }
    
    if (dtraits.getEdgeSourceLabel(node, ei).empty())
      edge_idx = -1;
    
    emit_edge(os, static_cast<const void*>(node), edge_idx,
              static_cast<const void*>(target), dest_port,
              dtraits.getEdgeAttributes(node, ei, g),
              dtraits.hasEdgeDestLabels());
  }
}

static void write_footer(raw_ostream &os) {
  os << "}\n";
}

template<typename GraphType>
void write_dot_graph(const GraphType &g, raw_ostream &os,
                 bool short_names = false,
                 const std::string &title = "") {
  DOTGraphTraits<GraphType> dtraits(short_names);
  
  write_header(g, os, dtraits, title);
  write_nodes(g, os, dtraits);
  // The GraphTraits interface needs to be changed here to take the
  // simpler os value.
  // dtraits.addCustomGraphFeatures(g, os);
  write_footer(os);  
}

} // End llvm namespace
 
#endif // end GRAPH_H
