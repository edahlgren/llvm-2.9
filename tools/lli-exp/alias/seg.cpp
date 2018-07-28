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

std::pair<u32, u32> mark_relevant_nodes(FlowAnalysisSet *fas, SEG *seg,
                                        ConstraintClasses *cc, Partitions *parts,
                                        bitmap rep_map,
                                        std::vector<u32> &rst) {

  u32 np = 0, r = 0;
  for (bitmap::iterator i = rep_map.begin(), e = rep_map.end();
       i != e; i++) {

    if (*i == 0) {
      for (Partitions::var_part_iterator j = parts->var_part.begin(),
             je = parts->var_part.end(); j != je; j++) {
        assert(fas->global_to_dfg.count(*j));
        cc->global_init.insert(cc->global_init.end(),
                               fas->global_to_dfg[*j].begin(),
                               fas->global_to_dfg[*j].end());
      }

      np++;
      seg->nodes[seg->start].is_pnode = false;
      rst.push_back(seg->start);
      
      continue;
    }

    for (Partitions::cons_part_iterator j = parts->cons_part.begin(),
           je = parts->cons_part.end(); j != je; j++) {
      Constraint &c = fas->dfg->node_cons(*j);

      if (c.type == ConstraintStore) {
        cc->cons_store.push_back(*j);

        u32 st_idx = fas->dfg->st_idx(*j, false);
        u32 n = fas->defs[st_idx];

        np++;
        seg->nodes[n].is_pnode = false;

        if (np <= 1 || r <= 1) {
          rst.push_back(n);
        }

        if (cc->cons_strong.count(*j)) {
          seg->nodes[n].has_const_transfer_func = true;
        }

        continue;
      }

      assert(c.type == ConstraintLoad);
      cc->cons_load.push_back(*j);

      u32 ld_idx = fas->dfg->ld_idx(*j, false);
      u32 n = fas->uses[ld_idx];

      r++;
      seg->nodes[n].uses_relevant_def = true;

      if (np <= 1 || r <= 1) {
        rst.push_back(n);
      }
    }    
  }

  return std::make_pair(np, r);
}

void organize_partitions(FlowAnalysisSet *fas, SEG *seg,
                         ConstraintClasses *cc, Partitions *parts,
                         std::map<u32, std::vector<u32> > &n2p,
                         std::map<u32, bitmap> &n2g, SqueezeMap *sm,
                         u32 partition, u32 num_np, u32 num_relevant) {

  if (num_np == 1 && num_relevant > 1) {
    process_1store(partition);
  }

  if (num_np > 0 && num_relevant == 1) {
    process_1load(partition);
  }
  
  for (std::vector<u32>::iterator i = cc->cons_load.begin(),
         e = cc->cons_load.end(); i != e; i++) {
    n2p[*i].push_back(partition);
  }
      
  for (std::vector<u32>::iterator i = cc->cons_store.begin(),
         e = cc->cons_store.end(); i != e; i++) {
    n2p[*i].push_back(partition);
  }

  if (num_np <= 1) {
    for (std::vector<u32>::iterator i = cc->cons_load.begin(),
           e = cc->cons_load.end(); i != e; i++) {
      n2g[*i].set(sm->squeeze(1, i));      
    }
    for (std::vector<u32>::iterator i = cc->cons_store.begin(),
           e = cc->cons_store.end(); i != e; i++) {
      n2g[*i].set(sm->squeeze(1, i));
    }
  } else {    
    u32 k = 0;    
    for (std::vector<u32>::iterator i = cc->cons_load.begin(),
           e = cc->cons_load.end(); i != e; i++) {
      n2g[*i].set(sm->squeeze(k, i));
      k++;
    }
    for (std::vector<u32>::iterator i = cc->cons_store.begin(),
           e = cc->cons_store.end(); i != e; i++) {
      n2g[*i].set(sm->squeeze(k, i));
      k++;
    }
  }

  for (std::vector<u32>::iterator i = rst.begin(), e = rst.end();
       i != e; i++) {
    SEGNode *node = seg->nodes[*i];
    node->is_pnode = true;
    node->uses_relevant_def = false;
    node->has_const_transfer_func = false;
  }
}

void map_nodes_load_store_constraints(FlowAnalysisSet *fas, SEG *seg,
                                      ConstraintClasses *cc, Partitions *parts,
                                      std::map<u32, u32> &pass_defs,
                                      std::map<u32, std::vector<u32> > &pass_uses,
                                      std::map<u32, std::vector<u32> > &n2p,
                                      u32 partition, bitmap rep_map) {

  for (bitmap::iterator i = rep_map.begin(), e = rep_map.end();
       i != e; i++) {
    if (*i == 0) {
      continue;
    }

    for (std::vector<u32>::iterator j = parts->cons_part[i].begin(),
           je = parts->cons_part[i].end(); j != je; j++) {
      
      Constraints &c = fas->dfg->node_cons(*j);
      if (c.type == ConstraintStore) {

        u32 def = fas->defs[fas->dfg->st_idx(*j, false)];
        u32 n = seg->get_rep_index(def);

        if (!n || seg->nodes[n]->rep) {
          continue;
        }

        pass_defs[n] = *j;
        n2p[*j].push_back(partition);
        
        continue;
      }

      u32 use = fas->uses[fas->dfg->ld_idx(*j, false)];
      u32 n = seg->get_rep_index(use);

      if (!n || seg->nodes[n]->rep) {
        continue;
      }

      pass_uses[n].push_back(*j);
      n2p[*j].push_back(partition);
    }    
  }  
}

void process_def_uses(FlowAnalysisSet *fas, SEG *seg,
                      ConstraintClasses *cc, Partitions *parts,
                      std::map<u32, u32> &pass_defs,
                      std::map<u32, std::vector<u32> > &pass_uses,
                      std::map<u32, u32> &pass_node, u32 partition) {

  for (pass_uses_iterator i = pass_uses.begin(), e = pass_uses.end();
       i != e; i++) {
    if (!pass_node.count(i->first)) {
      process_seg(fas, seg, cc, parts, partition, i->second);
    }
  }

  for (pass_defs_iterator i = pass_defs.begin(), e = pass_defs.end();
       i != e; i++) {
    if (!pass_node.count(i->first)) {
      process_seg(fas, seg, cc, parts, partition, i->second);
    }
  }  
}

void determine_shared_points_to_graphs() {
  std::hash_map<bitmap, u32> st;
  std::hash_map<bitmap, std::vector<u32> > ld;
  typedef std::hash_map<bitmap, std::vector<u32> >::iterator ld_iterator;

  for (n2g_iterator i = n2g.begin(), e = n2g.end(); i != e; i++) {    
  }

  for (ld_iterator i = ld.begin(), e = ld.end(); i != e; i++) {    
    std::vector<u32> &n = i->second;

    if (...) {
      for (std::vector<u32>::iterator j = n.begin(); je = n.end();
           j != je; j++) {
      }

      continue;
    }

    for (int j = 1; j < n.size(); j++) {
    }
  }
}

void modify_edges_from_nodes_to_reps() {
  for (tp_it i = fas->dfg.tp_begin(), e = fas->dfg.tp_end(); i != e; ++i) {
    for (std::vector<u32>::iterator j = i->succ.begin(), je = i->succ.end();
         j != je; j++) {      
    }
  }

  typedef vector<pair<u32,u32> >::iterator pmap_it;
  
  for (ld_it i = fas->dfg.ld_begin(), e = fas->dfg.ld_end(); i != e; ++i) {
    for (pmap_it j = i->part_succ.begin(), je = i->part_succ.end();
         j != je; j++) {
    }    
  }

  for (st_it i = fas->dfg.st_begin(), e = fas->dfg.st_end(); i != e; ++i) {
    for (pmap_it j = i->part_succ.begin(), je = i->part_succ.end();
         j != je; j++) {
    }    
  }

  for (np_it i = fas->dfg.np_begin(), e = fas->dfg.np_end(); i != e; ++i) {
    for (std::vector<u32> j = i->succ.begin(), je = i->succ.end();
         j != je; j++) {
    }
  }
}

void prep_points_to_sets_for_dfg_nodes() {
  for (int i = 1; i < parts->var_part.size(); i++) {
    for (bitmap::iterator j = parts->var_part[i].begin(),
           je = parts->var_part[i].end(); j != je; j++) {
    }
  }

  for (n2g_iterator i = n2g.begin(), e = n2g.end(); i != e; i++) {
    for (std::vector<u32> j = i->second.begin(), je = i->second.end();
         j != je; j++) {      
    }
  }  
}

typedef map<u32, bitmap>::iterator n2g_iterator;
typedef std::map<u32, u32>::iterator pass_defs_iterator;
typedef std::map<u32, std::vector<u32> >::iterator pass_uses_iterator;
typedef std::map<u32, u32>::iterator pass_node_iterator;

void dfg_compute_seg(FlowAnalysisSet *fas, SEG *seg,
                     ConstraintClasses *cc, Partitions *parts) {

  std::vector<u32> rst;
  std::vector<SEGNode *> savedSEG(seg->nodes);

  std::map<u32, std::vector<u32> > n2p;
  std::map<u32, bitmap> n2g;
  
  std::map<u32, u32> pass_defs;
  std::map<u32, std::vector<u32> > pass_uses;
  std::map<u32, u32> pass_node;

  SqueezeMap *sm = new SqueezeMap();
  
  T4 *t4 = new T4(seg);
  T2 *t2 = new T2(seg);
  T6 *t6 = new T6(seg);
  T5 *t5 = new T5(seg);
  
  for (int i = 1; i < parts->var_part.size(); i++) {
    u32 rep = parts->var_part[i].find_first();
    u32 num_np = 0, num_relevant = 0;

    bitmap rep_map = parts->obj_to_cons_part[rep];
    std::pair<u32, u32> r1 = mark_relevant_nodes(fas, seg, cc, parts,
                                                 rep_map, rst);
    num_np += r1->first;
    num_relevant += r1->second;

    if (num_np <= 1 || num_relevant <= 1) {
      organize_partitions(fas, seg, cc, parts,
                          n2p, n2g, sm, i,
                          num_np, num_relevant);

      rst.clear();
      cc->global_init.clear();
      cc->cons_load.clear();
      cc->cons_store.clear();

      continue;
    }

    t4->run();
    t2->run(t4->torder);
    t6->run(t4->rdefs);
    t5->run(t6->pnode_rdefs);

    map_nodes_load_store_constraints(fas, seg, cc, parts,
                                     pass_defs, pass_uses,
                                     n2p, i, rep_map);

    process_def_uses(fas, seg, cc, parts,
                     pass_defs, pass_uses, pass_nodes,
                     partition);

    cc->global_init.clear();
    pass_defs.clear();
    pass_uses.clear();
    pass_node.clear();
    
    reset_all_nodes(seg);

    for (std::vector<u32>::iterator j = t4->torder.begin(), je = t4->torder.end();
         j != je; j++) {
      seg->nodes[*j].pred = savedSEG[*j].pred;
    }

    for (std::vector<u32>::iterator j = t5->new_reps.begin(), je = t5->new_reps.end();
         j != je; j++) {      
      seg->nodes[*j].pred = savedSEG[*j].pred;
    }

    t4->reset();
    t2->reset();
    t6->reset();
    t5->reset();
  }

  delete sm;
  delete t4;
  delete t2;
  delete t6;
  delete t5;
  fas->defs.clear();
  fas->uses.clear();
  fas->global_to_dfg.clear();
  cc->cons_part.clear();
  cc->cons_strong.clear();

  determine_shared_points_to_graphs();
  
  modify_edges_from_nodes_to_reps();

  prep_points_to_sets_for_dfg_nodes();
}

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
