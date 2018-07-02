//===- graph.h - Graph templates ------------------------------------------===//
//
//===----------------------------------------------------------------------===//
//
// This file uses templates to serialize a generic GraphType to an ostream.
// The ostream is simply written to, not flushed or checked for errors.
//
// It exports the following public interfaces:
//
//   write_dot_graph - Serialize a GraphType to the dot language.
//
// It works by decorating function dependencies of write_*_graph methods with
// this template:
//
//   template<typename GraphType>
//
// Functions decorated by this template should accept a first parameter which
// is the bare 'const GraphType &g'. For example:
//
//   template<typename GraphType>
//   static void foo(const GraphType &g, ...) {
//     ...
//   }
//
// This allows the compiler to find a concrete type for 'g', which is replaced
// with the generic 'GraphType', at which point the entire function is compiled
// concretely.
//
// Other arguments of the function may use templates that specialize GraphType,
// for example:
//
//   template <>
//   struct DOTGraphTraits<Function *> : public DefaultDOTGraphTraits {
//     ...
//   }
//
//   template<typename GraphType>
//   static void foo(const GraphType &g, ..., DOTGraphTraits<GraphType> d, ...) {
//     ...
//   }
//
// But if the template defines a type name, then it must be labeled as such:
//
//   template<typename GraphType>
//   static void foo(const GraphType &g, ...
//                   typename GraphTraits<GraphType>::NodeType *node) {
//     ...
//   }
//
// In both of the cases above, the compiler searches for a template specialization
// that could match g (e.g. for DOTGraphTraits or GraphTraits) and replaces all
// instances of it in the function type signature and body with the concrete
// implementation, and then attempts to compile it.
//
//===----------------------------------------------------------------------===//

#ifndef GRAPH_H
#define GRAPH_H

#include "llvm/ADT/GraphTraits.h"
#include "llvm/Support/DOTGraphTraits.h"
#include "llvm/Support/raw_ostream.h"

// escape_string escapes these characters in label:
//
// How can I comment about something using the comment characters?
//
// At least this is what I *think* it does. I don't really know because
// I never ran it and it's hard to read. It would be useful to do some
// sort of execution on it so that it was easier to test/tell what the
// mapping is. I could write a test for it, or I could do some interesting
// case of symbolic execution.
static std::string escape_string(const std::string &label) {
  std::string str(label);
  for (unsigned i = 0; i != str.length(); ++i)
    switch (str[i]) {
    case '\n':
      str.insert(str.begin()+i, '\\');  // Escape character...
      ++i;
      str[i] = 'n';
      break;
    case '\t':
      str.insert(str.begin()+i, ' ');  // Convert to two spaces
      ++i;
      str[i] = ' ';
      break;
    case '\\':
      if (i+1 != str.length())
        switch (str[i+1]) {
        case 'l': continue; // don't disturb \l
        case '|': case '{': case '}':
          str.erase(str.begin()+i); continue;
        default: break;
        }
    case '{': case '}':
    case '<': case '>':
    case '|': case '"':
      str.insert(str.begin()+i, '\\');  // Escape character...
      ++i;  // don't infinite loop
      break;
    }
  return str;
}

namespace llvm {

// get_dot_edge_source_labels writes labels for relationships between the
// node and its successors (i.e. "children") to the ostream. It returns
// true if it wrote any labels.
template<typename GraphType>
static bool get_dot_edge_source_labels(const GraphType &g, raw_ostream &os,
                                   DOTGraphTraits<GraphType> dtraits,
                                   typename GraphTraits<GraphType>::NodeType *node) {
  // Initially we have found no non-empty labels.
  bool has_labels = false;

  // Get type used to iterate on nodes.
  typedef typename GraphTraits<GraphType>::ChildIteratorType child_iterator;

  // Find the beginning and the end node to iterate on. Conceptually these
  // are successors of the node.
  child_iterator ei = GraphTraits<GraphType>::child_begin(node);
  child_iterator ee = GraphTraits<GraphType>::child_end(node);

  // Iterate through the children of node, only allowing 64 labels.
  for (unsigned i = 0; ei != ee && i != 64; ++ei, ++i) {
    // Get a label for the node's successor. For example, if this node is
    // a BasicBlock, then its successor is another BasicBlock after its
    // branch, switch, or jump statement. This label tells us what sort
    // of relationship this node has to its child (e.g. the child is the
    // "true" branch of a conditional).
    std::string label = dtraits.getEdgeSourceLabel(node, ei);
    if (label.empty()) {
      // There is no label for the relationship, continue searching.
      continue;
    }

    // We found at least one non-empty label.
    has_labels = true;

    // Is this label for the first child?
    if (i) {
      // No, add a "|" delimiter.
      os << "|";
    }

    // Write the index of the label and the label itself.
    os << "<s" << i << ">" << escape_string(label);
  }

  // Did we get cut short because we reached 64 labels?
  if (ei != ee && has_labels) {
    // Write that the labels got truncated.
    os << "|<s64>truncated...";
  }

  // Did we write non-empty labels? Return that here.
  return has_labels;
}

/// emit_dot_edge writes an edge to the ostram.
static void emit_dot_edge(raw_ostream &os, const void *src_id, int src_port,
               const void *dest_id, int dest_port,
               const std::string &attrs, bool has_labels) {
  // Are we trying to write an edge from a truncated part?
  if (src_port  > 64) {
    // Don't write the edge.
    return;
  }

  // Are there too many edges?
  if (dest_port > 64) {
    // Treat this edge as the last one.
    dest_port = 64;
  }
  
  // Write the source node and its id.
  os << "\tNode" << src_id;
  if (src_port >= 0)
    os << ":s" << src_port;

  // Write the destination node and its id.
  os << " -> Node" << dest_id;
  if (dest_port >= 0 && has_labels)
    os << ":d" << dest_port;

  // Write edge attributes if we have any.
  if (!attrs.empty())
    os << "[" << attrs << "]";

  // Finish the edge line.
  os << ";\n";
}

// write_dot_edge serializes an edge to os using the dot language.
// The edge is between node and ei, which is the target.
template<typename GraphType>
static void write_dot_edge(const GraphType &g, raw_ostream &os,
                       DOTGraphTraits<GraphType> dtraits,
                       typename GraphTraits<GraphType>::NodeType *node,
                       unsigned edge_idx,
                       typename GraphTraits<GraphType>::ChildIteratorType ei) {
  // Get a pointer to the target.
  typename GraphTraits<GraphType>::NodeType *target = *ei;
  // Is the target undefined?
  if (!target) {
    // Then do nothing and return. Note that this seems wrong, the target
    // should never be undefined, should it?
    return;
  }
  
  // This is a port number, which will be attached to the Node label of the
  // edge.
  int dest_port = -1;

  // Should this outgoing edge actually target a different edge source? This
  // is a bit weird, but we allow DOTGraphTraits implementations to jump
  // around like this.
  if (dtraits.edgeTargetsEdgeSource(node, ei)) {
    // Get the actual target.
    typename GraphTraits<GraphType>::ChildIteratorType target_it =
      dtraits.getEdgeTarget(node, ei);
      
    // Get the offset between the node and this target.
    unsigned offset = (unsigned)std::distance(GraphTraits<GraphType>::child_begin(target), target_it);
    // The port number of the target is its offset from the source.
    dest_port = static_cast<int>(offset);
  }

  // Is the edge missing a label at the source?
  if (dtraits.getEdgeSourceLabel(node, ei).empty()) {
    // Treat the edge as if it was truncated.
    edge_idx = -1;
  }

  // Write the edge.
  emit_dot_edge(os, static_cast<const void*>(node), edge_idx,
            static_cast<const void*>(target), dest_port,
            dtraits.getEdgeAttributes(node, ei, g),
            dtraits.hasEdgeDestLabels());
}

// write_dot_node serializes a node to os using the dot language. 
template<typename GraphType>
static void _write_dot_node(const GraphType &g, raw_ostream &os,
                       DOTGraphTraits<GraphType> dtraits,
                       typename GraphTraits<GraphType>::NodeType *node) {
  // Get an attributes of this node.
  std::string attrs = dtraits.getNodeAttributes(node, g);

  // Get any label for this node.
  std::string label = dtraits.getNodeLabel(node, g);

  // Should the graph be rendered bottom-up, rather than the default of
  // of top-down?
  bool render_bottom_up = dtraits.renderGraphFromBottomUp();

  // Should the address of the node be added to its label? By default
  // this returns false.
  bool has_addr_label = dtraits.hasNodeAddressLabel(node, g);

  // Start the node line.
  os << "\tNode" << static_cast<const void*>(node) << " [shape=record,";

  // If there are attributes, add them.
  if (!attrs.empty())
    os << attrs << ",";

  // Start the labels.
  os << "label=\"{";

  // Are we rendering top-down?
  if (!render_bottom_up) {
    // Add the node label and the node address (if needed) *before* writing
    // the edge labels (see below).
    os << escape_string(label);
    if (has_addr_label)
      os << "|" << (void*)node;
  }

  // Extract the labels from this node to other nodes.
  std::string edge_source_labels;
  raw_string_ostream estream(edge_source_labels);
  bool has_edge_source_labels = get_dot_edge_source_labels(g, estream, dtraits, node);

  // Did we find any?
  if (has_edge_source_labels) {
    // Are we rendering top-down?
    if (!render_bottom_up) {
      // Then add a delimiter first.
      os << "|";
    }

    // Write the edge labels.
    os << "{" << estream.str() << "}";

    // Are we rendering bottom-up?
    if (render_bottom_up) {
      // Then add a delimiter after writing the edge labels.
      os << "|";
    }
  }

  // Are we rendering bottom-up?
  if (render_bottom_up) {
    // Then add the node label and node address (if needed) *after* writing
    // the edge labels.
    os << escape_string(label);
    if (has_addr_label)
      os << "|" << (void*)node;

    // Are there any labels for the targets? It seems that we skip this
    // entirely if the graph is rendered top-down. 
    bool has_edge_dest_labels = dtraits.hasEdgeDestLabels();
    if (has_edge_dest_labels) {
      // Yes, then start enclosing them.
      os << "|{";

      // Iterate through all of the target labels, allowing up to 64, similar
      // to how we wrote source labels in get_dot_source_labels (see above).
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

  // Finish printing the "Node" line.
  os << "}\"];\n";
    
  // Prepare to write the edges.
  typedef typename GraphTraits<GraphType>::ChildIteratorType child_iterator;
  child_iterator ei = GraphTraits<GraphType>::child_begin(node);
  child_iterator ee = GraphTraits<GraphType>::child_end(node);

  // Iterate through the first 64 edges.
  for (unsigned i = 0; ei != ee && i != 64; ++ei, ++i) {
    // Is this node hidden?
    bool hidden = dtraits.isNodeHidden(*ei);
    if (!hidden) {
      // No, it's visible, write its edge.
      write_dot_edge(g, os, dtraits, node, i, ei);
    }
  }

  // Iterate through the rest of the edges, if there are any past the
  // 64th edge.
  for (; ei != ee; ++ei) {
    // Is this node hidden?
    bool hidden = dtraits.isNodeHidden(*ei);
    if (!hidden) {
      // No, it's visible, write it but always use an edge index of 64.
      //
      // CHECKME: what is the effect of this?
      write_dot_edge(g, os, dtraits, node, 64, ei);
    }
  }
}

template<typename GraphType>
static void write_dot_node(const GraphType &g, raw_ostream &os,
                       DOTGraphTraits<GraphType> dtraits,
                       typename GraphTraits<GraphType>::NodeType& node) {
  write_dot_node(g, os, dtraits, &node);
}

template<typename GraphType>
static void write_dot_node(const GraphType &g, raw_ostream &os,
                       DOTGraphTraits<GraphType> dtraits,
                       typename GraphTraits<GraphType>::NodeType *const *node) {
  write_dot_node(g, os, dtraits, *node);
}
 
template<typename GraphType>
static void write_dot_node(const GraphType &g, raw_ostream &os,
                       DOTGraphTraits<GraphType> dtraits,
                       typename GraphTraits<GraphType>::NodeType *node) {
    if (!dtraits.isNodeHidden(node)) {
      _write_dot_node(g, os, dtraits, node);
    }
}

// write_dot_header writes to the ostream in a format specific to the
// the dot language.
//
// 1. A specialized graph name from DOTGraphTraits.getGraphName, or
//    "unnamed" if DOTGraphTraits.getGraphName returns "".
//    DOTGraph
// 2. Whether the graph should be rendered from the bottom-up, from
//    DOTGraphTraits.renderGraphFromBottomUp.
// 3. Any graph properties, from DOTGraphTraits.getGraphProperties.
template<typename GraphType>
static void write_dot_header(const GraphType &g, raw_ostream &os, DOTGraphTraits<GraphType> dtraits, const std::string &alt_title) {
  // Select a graph name.
  std::string graph_name = "";
  if (!alt_title.empty()) {
    graph_name = alt_title;
  } else {
    graph_name = dtraits.getGraphName(g);
  }

  // Write the graph name.
  if (!graph_name.empty())
    os << "digraph \"" << escape_string(graph_name) << "\" {\n";
  else
    os << "digraph unnamed {\n";

  // Encode whether graph should be rendered "bottom-to-top". Default is
  // "TB" which means "top-to-bottom". See this for the dot defaults:
  // https://www.graphviz.org/doc/info/attrs.html
  if (dtraits.renderGraphFromBottomUp())
    os << "\trankdir=\"BT\";\n";

  // Label the graph with the graph name, if specified.
  if (!graph_name.empty())
    os << "\tlabel=\"" << escape_string(graph_name) << "\";\n";

  // Write any graph properties.
  os << dtraits.getGraphProperties(g);

  // End the header.
  os << "\n";
}

// write_dot_nodes iterates through all of the nodes in g, serializing
// each one that is not hidden.
template<typename GraphType>
static void write_dot_nodes(const GraphType &g, raw_ostream &os,
                            DOTGraphTraits<GraphType> dtraits) {
  // Iterate through all nodes, as they are ordered in the GraphTraits.
  typedef typename GraphTraits<GraphType>::nodes_iterator node_iterator;  
  for (node_iterator i = GraphTraits<GraphType>::nodes_begin(g),
         e = GraphTraits<GraphType>::nodes_end(g); i != e; ++i) {
    // Serialize this node to os.
    write_dot_node(g, os, dtraits, *i);
  }
}


// write_dot_footer closes the header.
static void write_dot_footer(raw_ostream &os) {
  os << "}\n";
}

// write_dot_graph writes a generic GraphType to ostream using the dot
// language. It writes the "long" complete names of nodes and always
// uses DOTGraphTraits<GraphType> for the graph name. To understand how
// the generic GraphType works, see the file comment above.
template<typename GraphType>
void write_dot_graph(const GraphType &g, raw_ostream &os,
                     const std::string alt_title = "") {
  // Initialize behavior specific to dot graphs.
  DOTGraphTraits<GraphType> dtraits(false);

  // Write a header.
  write_dot_header(g, os, dtraits, alt_title);

  // Serialize each of the nodes and edges.
  write_dot_nodes(g, os, dtraits);
  
  // The GraphTraits interface needs to be changed here to take the
  // simpler os value, which I think is better anyway.
  // dtraits.addCustomGraphFeatures(g, os);

  // Close the header.
  write_dot_footer(os);  
}

} // End llvm namespace
 
#endif // end GRAPH_H
