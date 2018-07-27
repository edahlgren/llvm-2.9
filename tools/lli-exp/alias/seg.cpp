#include "seg.h"
#include "transform.h"

bool SEG::node_survives_reduction(SEGNode *node) {
  return node->uses_relevant_def || (!node->has_const_transfer_func && !node->pnode);    
}

typedef std::map<u32, std::vector<llvm::Function*> > FunctionMap;

// Assumes "main" which is dumb.
FunctionMap add_interprocedural_edges(llvm::Module *m, AnalysisSet *as,
                                      Processor *proc, SEG *seg) {

  llvm::Function *main = m->getFunction("main");
  assert(main &&
         proc->func_start_nodes.count(main) &&
         proc->func_start_nodes[main] < seg->nodes.size());

  seg->connect_nodes(seg->start, proc->func_start_nodes[main]);

  std::set<u32> has_ext, is_alloc;
  std::map<u32, std::pair<u32, u32>> factor;

  FunctionMap tgts;
  for (ConstraintInstMap::iterator i = as->indirect_calls,
         e = as->indirect_calls; i != e; i++) {
    llvm::CallInst *ci = i->first;
    u32 n = i->second;

    assert(proc->callsite_succ.count(n));
    u32 succ = proc->callsite_succ[n];

    u32 fp = as->find_value_node(ci->getCalledValue());
    u32 rep = as->nodes->pe(fp);
    bool seen = tgts.count(rep);

    if (!seen) {
      std::vector<llvm::Function *> &ffp = tgts[rep];

      std::vector<u32> pts = *points_to_set(rep);
      for (std::vector<u32>::iterator it = pts.begin(), ei = pts.end();
           it != ie; it++) {
        u32 val = as->nodes->nodes[*it]->val;
        llvm::Function *f = llvm::dyn_cast_or_null<llvm::Function>(val);
        if (!f) {
          continue;
        }

        if (!as->ext_info->is_ext(f)) {
          fpp.push_back(f);
          continue;
        }

        has_ext.insert(rep);
        if (as->ext_info->is_alloc(f)) {
          is_alloc.insert(rep);
        }
      }

      std::sort(ffp.begin(), ffp.end());
      std::vector<llvm::Function *>::iterator e =
        std::unique(ffp.begin(), ffp.end());
      ffp.erase(e. ffp.end());

      u32 cll = seg->insert_new_node(false);
      u32 ret = seg->insert_new_node(false);

      for (std::vector<llvm::Function *> it = ffp.begin(), ie = ffp.end();
           it != ie; it++) {
        seg->connect_node(cll, proc->func_start_nodes[*it]);
        if (proc->func_ret_nodes.count(*it)) {
          seg->connect_node(proc->func_ret_nodes[*it], ret);
        }
      }

      factor[rep] = std::pair<u32, u32>(cll, ret);
    }

    if (is_alloc.count(rep) &&
        as->nodes->find_value_node(ci, true)) {
      u32 src = as->nodes->find_object_node(ci);
      u32 dest = as->nodes->find_value_node(ci);
      add_constraint(ConstraintAddrOf, dest, src);
    }

    assert(factor.count(rep));
    std::pair<u32, u32> &fact = factor[rep];

    seg->connect_node(n, fact.first);
    seg->connect_node(fact.second, succ);

    if (!has_ext.count(rep)) {
      seg->erase_edge(n, succ);
    }    
  }

  has_ext.clear();
  is_alloc.clear();
  as->indirect_calls.clear();

  for (FunctionMap::iterator i = proc->func_callsites.begin(),
         e = proc->func_callsites.end(); i != e; i++) {
    u32 clr = i->first;
    std::vector<llvm::Function *> &fs = i->second;

    assert(proc->callsite_succ.count(clr));
    u32 succ = proc->callsite_succ[clr];
    seg->erase_edge(clr, succ);

    for (std::vector<llvm::Function *> it = fs.begin(), ei = fs.end();
         it != ie; it++) {
      assert(proc->func_start_nodes.count(*it));
      seg->connect_nodes(clr, proc->func_start_nodes[*it]);
      if (proc->func_ret_nodes.count(*it)) {
        seg->connect_nodes(proc->func_ret_nodes[*it], succ);
      }
    }
  }

  proc->func_callsites.clear();
  proc->func_ret_nodes.clear();
  proc->func_start_nodes.clear();
  proc->callsite_succ.clear();

  return std::move(tgts);
}

u32 process_indirect_constraints(AnalysisSet *as, FunctionMap &tgts) {
  std::map<std::pair<u32,u32>, u32> tmps;
  typedef std::map<std::pair<u32,u32>, u32>::iterator tmps_iterator;

  std::vector<Constraint> new_cons;
  u32 num_tmp;
  
  for (ConstraintInstMap::iterator i = as->indirect_constraints.begin(),
         e = as->indirect_constraints.end(); i != e; i++) {
    Constraint &c = as->constraints[*i];

    u32 fp = 0, cle = 0, tv = 0, el = 0;
    u32 *src_p = 0, *dst_p = 0;

    if (c.src == NodeConstToUnknownTarget) {
      continue;
    }

    if (c.type == ConstraintStore) {
      assert(c.off > 1);
      fp = points_to_set(c.dest);
      src_p = &tv;
      dst_p = &el;
    } else {
      assert(c.type == ConstraintLoad && c.off == 1);
      fp = points_to_set(c.src);
      src_p = &el;
      dst_p = &tv;
    }

    std::pair<u32, u32> p(fp, c.off);
    tmps_iterator j = tmps.find(p);

    if (j != tmps.end()) {
      tv = j->second;
    } else {
      tv = as->nodes->next++;
      num_tmp++;

      tmps[p] = tv;

      assert(tgts.count(fp));
      std::vector<llvm::Function *> &callees = tgts[fp];

      for (std::vector<llvm::Function *>::iterator it = callees.begin(),
             ie = callees.end(); it != ie; it++) {
        cle = as->nodes->find_object_node(*it);

        u32 cle_val = as->nodes->nodes[cle]->val;
        assert(llvm::isa<llvm::Function>(cle_val));

        if (as->nodes->nodes[cle]->obj_sz > c.off) {
          el = cle + c.off;

          u32 el_val = as->nodes->nodes[el]->val;
          if (c.off > 1 && !is_pointer(el_val)) {
            continue;
          }

          new_cons.push_back(Constraint(ConstraintCopy, *dst_p, *src_p));
        }
      }
    }

    if (c.off == 1) {
      src_p = &tv;
      dst_p = &c.dst;
    } else {
      src_p = &c.src;
      dst_p = &tv;
    }

    // Why is this happening again?
    new_cons.push_back(Constraint(ConstraintCopy, *dst_p, *src_p));
  }

  std::sort(new_cons.begin(), new_cons.end());
  std::vector<Constraint>::iterator e = std::unique(new_cons.begin(),
                                                    new_cons.end());
  constraints.insert(as->constraints.end(), new_cons.begin(), e);

  for (ConstraintInstMap::iterator it = as->indirect_constraints.begin(),
         ie = as->indirect_constraints.end(); it != ie; it++) {
    as->constraints[*it].off = MAX_U32;
  }
  as->indirect_constraints.clear();

  return num_tmp;
}

void remove_redundant_nodes(FlowAnalysisSet *as, SEG *seg) {
  std::map<u32, u32> redir;
  std::vector<SEGNode *> nodes;

  u32 n = 1;
  for (int i = 1; seg->nodes.size(); i++) {
    SEGNode *node = seg->get_node(i);
    if (node->is_rep() && node->rep) {
      redir[i] = n++;
    }
  }

  nodes.assign(n, new SEGNode());

  for (std::map<u32, u32>::iterator i = redir.begin(), e = redir.end();
       i != e; i++) {
    SEGNode &N = seg->nodes[i->first];
    SEGNode &X = nodes[i->second];

    for (std::vector<u32>::iterator it = N.pred.begin(), ie = N.pred.end();
         it != ie; it++) {
      u32 p = seg->get_rep_index(*it);
      if (p == it->first) {
        continue;
      }

      std::map<u32, u32>::iterator ii = redir.find(p);
      if (ii != redir.end()) {
        X.pred.insert(ii->second);
      }
    }

    X.pred.unique();
  }

  u32 rep_start = seg->get_rep_index(seg->start);
  std::map<u32, u32>::iterator ii = redir.find(rep_start);

  assert(ii != redir.end());
  seg->start = ii->second;

  for (std::map<u32, u32>::iterator i = fas->defs.begin(),
         e = fas->defs.end(); i != e; i++) {
    u32 rep_def = seg->get_rep_index(*i);

    ii = redir.find(rep_def);
    if (*i && (redir.find(rep_def) != redir.end())) {
      *i = ii->second;
    } else {
      *i = 0;
    }
  }

  for (std::map<u32, u32>::iterator i = fas->uses.begin(),
         e = fas->uses.end(); i != e; i++) {
    u32 rep_def = seg->get_rep_index(*i);

    ii = redir.find(rep_def);
    if (*i && (redir.find(rep_def) != redir.end())) {
      *i = ii->second;
    } else {
      *i = 0;
    }
  }

  seg->nodes.swap(nodes);  
}

// This reduces the graph to make it "more sparse". It works by applying
// several well-known transformations and obeying the graph's predicate
// node_survives_reduction for each node that could be reduced.
void SEG::do_reduction() {
  T4 t4(this);
  t4.run();

  T2 t2(this);
  t2.run(t4.torder);

  T6 t6(this);
  t6.run(t4.rdefs);

  T5 t5(this);
  t5.run(t6.pnode_rdefs);
}

SEG *build_constraint_based_seg(llvm::Module *m, u64 max_size = 1000000000) {
  // Step 1.
  //
  // Initialize a new evaluation graph and a processor wrapper that
  // will hold def-use information.
  SEG *graph = new SEG(max_size);
  Processor *proc = new Processor(graph);

  // Step 2.
  //
  // Walk the module to find constraints and fill in def-use information.
  Constraints *cs = build_constraints(m, proc);

  // Step 3.
  //
  // Optimize the constraints so that they'll be easier to solve for.
  cs->optimize();

  // ??
  graph->make_interprocedural(m);
  
  // Step 4.
  //
  // Solve the constraints.
  cs->solve();

  
  
  
  // Reduce the graph to make it more sparse.
  graph->reduce();

  /**

  // Build a data flow graph from the constraints.
  DataFlowGraph *dfg = build_dfg(cs);

  // Do we need the constraints anymore now that they've been
  // incorporated into the dfg?
  delete cs;

  // Partition variables in the dfg. Maybe this can be in dfg_from_constraints
  // above.
  dfg->partition();

  // Incorporate interprocedural edges from the dfg.
  graph->extend(dfg);

  // Get rid of the dfg, we don't need it anymore.
  delete dfg;
  
  // Return the final sparse representation.
  return graph;
  **/
}

// Write the graph in the dot language to os.o
void SEG::print(std::ostream &os) {
  // Start the graph wrapper.
  os << "strict digraph SEG {" << std::endl;

  // Process each node.
  for (SEGIndex i = 1; i < this->nodes.size(); i++) {
    SEGNode *node = this->get_node(i);

    // Does this node not represent itself?
    if (!node->represents_itself()) {
      // Then it's been "squashed" into something else, skip it.
      continue;
    }

    // Has this node been marked deleted?
    if (node->del) {
      // Yes, skip it.
      continue;
    }

    // Start the node header.
    os << "\t" << i << " [label=\"";
    os << i << ":";

    // Is this a preserving node?
    if (node->pnode) {
      // Mark it with a "DEF" label.
      os << "DEF";
    }

    // Would this node survive reduction?
    if (this->node_survives_reduction(node)) {
      // Is it a preserving node?
      if (node->pnode) {
        // Add a separate to distinguish from the label above.
        os << "_";        
      }

      // Mark it with a "USE" label.
      os << "USE";
    }

    // Complete the label.
    os << "\"";

    // Is this the start node?
    if (node == this->start) {
      // Then make it red so that it's easy to distinguish.
      os << ",color=red";
    }

    // Finish the node header.
    os << "];" << std::endl;

    // Write the edges from predecessors to this node.
    for (SEGIndexSet::iter pi = node->pred.begin(), pe = node->pred.end();
         pi != pe; pi++) {
      SEGIndex rep_index = this->get_representative_index(*pi);

      // Does this node point to itself?
      if (rep_index == i) {
        // Yes, skip it.
        continue;
      }

      // Has this predecessor been marked deleted?
      if (this->get_node(rep_index)->del) {
        // Yes, skip it.
        continue;
      }

      // Write the dge.
      os << "\t" << rep_index << " -> " << i << ";" << std::endl;
    }
  }

  // Close the graph wrapper. And we're done.
  os << "}" << std::endl;
}
