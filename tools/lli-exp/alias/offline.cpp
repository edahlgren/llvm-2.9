u32 OfflineGraph::merge(u32 a, u32 b) {
  assert(a && a < this->nodes.size() && "invalid node 1");
  assert(b && b < this->nodes.size() && "invalid node 2");
  assert(a != b && "trying to merge a node with itself");

  OfflineNode *n1 = &this->nodes[a];
  OfflineNode *n2 = &this->nodes[b];
  assert(n1->dfs_num && n2->dfs_num && "trying to merge unvisited nodes");

  u32 r1 = n1->rep, r2 = n2->rep;
  assert(r1 >= NODE_RANK_MIN && r2 >= NODE_RANK_MIN && "only rep nodes can be merged");

  // Make n1 the parent of n2. 
  if (r1 < r2) {
    std::swap(a, b);
    std::swap(n1, n2);
  }
  if (r1 == r2) {
    ++n1->rep;
  }
  n2->rep = n1;

  // Move n2's edges and labels into n1.
  n1->edges |= n2->edges;
  n1->impl_edges |= n2->impl_edges;
  n1->ptr_eq |= n2->ptr_eq;
  n1->indirect |= n2->indirect;
  
  n2->edges.clear();
  n2->impl_edges.clear();
  n2->ptr_eq.clear();
  
  return a;
}

void HR::run() {
  u32 prev = 0, curr = this->as->constraints.size();

  do {
    {
      HVN(this->as, this->do_union, this->last_obj);
      hvn.run();
    }
    
    prev = curr;
    curr = this->as->constraints.size();
    
  } while (prev - curr >= this->threshold);  
}

static void add_constraint_edges(HCD *hcd) {
  DPUTS("***** Adding offline constraint edges\n");
  u32 n_copy= 0, n_load= 0, n_store= 0, n_impl_addr= 0, n_impl_copy= 0;
  FORN(i, constraints.size()){
    const Constraint &C= constraints[i];
    //This may fail if the source of an addr_of is a non-rep (which is
    //  allowed but hasn't happened yet).
    assert(nodes[C.dest]->is_rep() && nodes[C.src]->is_rep());
    u32 od= main2off[C.dest], os= main2off[C.src];
    if(!od){
      //A few constraints will have an obj_node for the dest.
      if(nodes[C.dest]->obj_sz){
        continue;
      }
      assert(!"no offline node for dest");
    }
    assert(os || nodes[C.src]->obj_sz && "no offline node for non-obj src");
    switch(C.type){
      case addr_of_cons:
        if(!hcd){
          //D = &S: impl. *D -> S.
          //Also add the actual points-to edge to the label set.
          //  Because of SSA form, there is only one addr_of_cons per dest.
          //  node, so all initial label sets will be singletons or empty,
          //  and HVN mode (do_union = 0) will work correctly.
          off_nodes[od].ptr_eq.set(C.src);
          //Note that S is often a non-AFP obj_node, which we ignore.
          if(os){
            off_nodes[REF(od)].impl_edges.set(os);
            ++n_impl_addr;
          }
        }
        break;
      case copy_cons:
        //D = S: edge D -> S, impl. *D -> *S.
        if(os){
          off_nodes[od].edges.set(os);
          ++n_copy;
          if(!hcd){
            off_nodes[REF(od)].impl_edges.set(REF(os));
            ++n_impl_copy;
          }
        }else{
          //Copying from an obj_node not in the graph makes dest. indirect.
          if(!hcd){
            off_nodes[od].indirect= 1;
          }
        }
        break;
      case load_cons:
        //Note: we don't handle load/store with offset as part of the HVN.
        //  These are only used for indirect calls, so handling them
        //  would not help much.
        if(C.off){
          //D = *S + k: D indirect
          if(!hcd){
            off_nodes[od].indirect= 1;
          }
        }else{
          //D = *S: edge D -> *S
          assert(os && "no offline node for src");
          off_nodes[od].edges.set(REF(os));
          ++n_load;
        }
        break;
      case store_cons:
        //*D + k = *S: ignored
        if(!C.off){
          //*D = S: edge *D -> S
          assert(os && "no offline node for src");
          off_nodes[REF(od)].edges.set(os);
          ++n_store;
        }
        break;
      case gep_cons:
        //D = gep S k: D is pre-labeled with the ID of its
        //  (S, k) pair. This works because in SSA form, the LHS of a GEP
        //  cannot be assigned any other value.
        if(!hcd){
          u32 pe;
          pair<u32, u32> R(C.src, C.off);
          hash_map<pair<u32, u32>, u32>::const_iterator i_g2p= gep2pe.find(R);
          if(i_g2p == gep2pe.end()){
            gep2pe[R]= pe= next_ptr_eq++;
          }else{
            pe= i_g2p->second;
          }
          off_nodes[od].ptr_eq.set(pe);
        }
        break;
      default:
        assert(!"unknown constraint type");
    }
  }
}

static void do_dfs_at(HCD *hcd, u32 index) {
  assert(n);
  DEBUG(fprintf(stderr, "  hcd_dfs %u\n", n));
  OffNode *N= &off_nodes[n];
  assert(!N->scc_root && N->is_rep());
  u32 our_dfs= curr_dfs++;
  N->dfs_id= our_dfs;

  //Look for SCCs using normal edges only.
  for(bitmap::iterator it= N->edges.begin(), ie= N->edges.end();
      it != ie; ++it){
    const OffNode *N2= &off_nodes[*it];
    assert(N2->is_rep());
    //Skip this neighbor if it's in an already processed SCC.
    if(N2->scc_root){
      continue;
    }
    //If it's unvisited, continue the DFS.
    if(!N2->dfs_id){
      hcd_dfs(*it);
    }
    //Set our dfs_id to the minimum reachable ID.
    if(N2->dfs_id < N->dfs_id){
      N->dfs_id= N2->dfs_id;
    }
  }
  assert(N->is_rep());

  //Is N the root of an SCC?
  if(N->dfs_id == our_dfs){
    //Record all nodes in our SCC (the root is not on the stack).
    vector<u32> scc(1, n);
    DEBUG(fprintf(stderr, "    HCD SCC: %u", n));
    //The VAR (non-REF) nodes in this SCC (may be several since we don't run
    //  HR to convergence).
    vector<u32> var;
    if(n < firstREF)
      var.push_back(n);
    while(!dfs_stk.empty()){
      u32 n2= dfs_stk.top();
      assert(n2 != n);
      //Anything visited before us should remain on the stack.
      if(off_nodes[n2].dfs_id < our_dfs){
        break;
      }
      dfs_stk.pop();
      scc.push_back(n2);
      DEBUG(fprintf(stderr, " %u", n2));
      if(n2 < firstREF){
        DEBUG(putc('*', stderr));
        var.push_back(n2);
      }
    }
    DEOL;
    //Singleton SCCs are ignored.
    if(scc.size() == 1){
      N->scc_root= 1;
      return;
    }
    assert(var.size() && "no VAR node in SCC");
    //Replace the offline VARs by the corresponding main nodes,
    //  then merge all of those.
    //Note that this will collapse any remaining VAR-only SCCs.
    u32 var_rep= off_nodes[var[0]].main_node;
    for(u32 i= 1, ie= var.size(); i != ie; ++i){
      var_rep= merge_nodes(var_rep, off_nodes[var[i]].main_node);
      INC_STAT(hcd_var_merge);
    }
    //Now process the entire SCC.
    FORN(i, scc.size()){
      u32 n= scc[i];
      assert(n);
      OffNode *N= &off_nodes[n];
      //Label N as a "root" (since it should be considered deleted but
      //  is not collapsed into the root).
      N->scc_root= 1;
      if(n >= firstREF){
        //Map the main node of N to the vars' rep.
        hcd_var[N->main_node]= var_rep;
      }
    }
  }else{
    dfs_stk.push(n);
  }
}

static void merge_equal_pointers(HCD *hcd) {
  //Update constraints and icall_cons to refer to the reps,
  //  because we merged some of the VARs.
  vector<Constraint> old_cons;
  old_cons.swap(constraints);
  DenseSet<Constraint> cons_seen;
  FORN(i, old_cons.size()){
    const Constraint &C0= old_cons[i];
    u32 dest= get_node_rep(C0.dest), src= C0.src;
    if(C0.type != addr_of_cons)
      src= get_node_rep(src);
    Constraint C(C0.type, dest, src, C0.off);
    //Ignore (copy X X) and duplicates.
    if((C.type != copy_cons || C.dest != C.src) && !cons_seen.count(C)){
      cons_seen.insert(C);
      constraints.push_back(C);
    }
  }
  vector<pair<Constraint, set<Instruction*> > > old_icall_cons;
  for(DenseMap<Constraint, set<Instruction*> >::iterator
      it= icall_cons.begin(), ie= icall_cons.end(); it != ie; ++it){
    old_icall_cons.push_back(*it);
  }
  icall_cons.clear();
  FORN(i, old_icall_cons.size()){
    Constraint &C= old_icall_cons[i].first;
    C.dest= get_node_rep(C.dest);
    C.src= get_node_rep(C.src);
    const set<Instruction*> &I= old_icall_cons[i].second;
    icall_cons[C].insert(I.begin(), I.end());
  }
}

void HCD::run() {
  add_constraint_edges(this);

  OfflineGraph *og = this->offline_graph;
  for (int i = 0; i < og->offsets.size(); i++) {
    u32 index = og->offsets[i];
    if (index) {
      og->nodes[index].offset = index;

      u32 ref = og->ref(index);
      og->nodes[ref].offset = index;
    }
  }

  u32 start = og->first_func_param_node;
  u32 end = og->first_dereference_node + og->num_dereference_nodes;

  for (u32 i = start; i < end; i++) {
    if (!og->nodes[i].dfs_num) {
      do_dfs_at(this, i);
    }
  }

  assert(this->dfs_stack.empty());

  merge_equal_pointers(this);
}

typedef u32 Src;
typedef u32 Dest;
typedef u32 Off;

static const u32 factor_min_sz= 2;
static llvm::DenseMap<Constraint, u32> factored_cons;

void merge_load_constraint(AnalysisSet *as,
                           Src src,
                           Off off,
                           std::set<Dest> dest_nodes,
                           llvm::DenseMap<Constraint, u32> &factored) {
  Constraint c(ConstraintLoad, src, src, off);

  if (dest_nodes.size() < FACTOR_MIN_SZ) {
    for (std::set<Dest>::const_iterator i = dest_nodes.begin(),
           e = dest_nodes.end(); i != e; i++) {
      c.dest = *i;
      as->constraints.push_back(c);
    }    

    return;
  }

  //Add (T = *V + k) or (*V + k = T).
  u32 t = as->nodes->nodes.size();
  as->nodes->nodes.push_back(new Node);

  c.dest = t;
  as->constraints.push_back(c);
  
  //Add (A = T) or (T = B).
  Constraint b = c;
  c.type = ConstraintCopy;
  c.off = 0;
  c.src = t;

  for (std::set<Dest>::const_iterator i = dest_nodes.begin(),
         e = dest_nodes.end(); i != e; i++) {
    b.dest = c.dest = *i;
    // This seems weird, why are we adding it twice?
    as->constraints.push_back(c);
    factored[b] = t;
  }
}

void merge_store_constraints(AnalysisSet *as,
                             Dest dest,
                             Off off,
                             std::set<Src> src_nodes,
                             llvm::DenseMap<Constraint, u32> &factored) {
  Constraint c(ConstraintStore, dest, dest, off);

  if (src_nodes.size() < FACTOR_MIN_SZ) {
    for (std::set<Dest>::const_iterator i = src_nodes.begin(),
           e = src_nodes.end(); i != e; i++) {
      c.src = *i;
      as->constraints.push_back(c);
    }    

    return;
  }

  //Add (T = *V + k) or (*V + k = T).
  u32 t = as->nodes->nodes.size();
  as->nodes->nodes.push_back(new Node);

  c.src = t;
  as->constraints.push_back(c);
  
  //Add (A = T) or (T = B).
  Constraint b = c;
  c.type = ConstraintCopy;
  c.off = 0;
  c.dest = t;

  for (std::set<Dest>::const_iterator i = dest_nodes.begin(),
         e = dest_nodes.end(); i != e; i++) {
    b.src = c.dest = *i;
    // This seems weird, why are we adding it twice?
    as->constraints.push_back(c);
    factored[b] = t;
  }
}

void run_factor_load_store(AnalysisSet *as) {
  // Map the source node and offset of each load constraint to the
  // set of destination nodes.
  typedef std::hash_map<std::pair<Src, Off>, std::set<Dest>> LoadMap;
  LoadMap loads;

  // Map the destination node and offset of each store constraint
  // to the set of source nodes.
  typedef std::hash_map<std::pair<Dest, Off>, std::set<Src>> StoreMap;
  StoreMap stores;

  std::vector<Constraint> old;
  old.swap(as->constraints);

  for (int i = 0; i < old.size(); i++) {
    const Constraint &c = old[i];

    switch (c.type) {
    case ConstraintLoad:
      loads[std::make_pair(c.src, c.off)].insert(c.dest);
    case ConstraintStore:
      stores[std::make_pair(c.dest, c.off)].insert(c.src);
    default:
      as->constraints.push_back(c);
    }
  }
  
  old.clear();

  llvm::DenseMap<Constraint, u32> factored;
  
  for (LoadMap::const_iterator i = loads.begin(), e = loads.end();
       i != e; i++) {
    Src src = i->first.first, Off off = i->first.second;
    std::set<Dest> dest_nodes = i->second;
    
    merge_load_constraint(as, src, off, dest, factored);
  }

  for (StoreMap::const_iterator i = stores.begin(); e = stores.end();
       i != e; i++) {
    Dest dest = i->first.first, Off off = i->first.second;
    std::set<Src> src_nodes = i->second;

    merge_store_constraint(as, dest, off, src_nodes, factored);
  }

  std::vector<ConstraintInstSet> old_indirect;
  for (ConstraintIntMap::iterator i = as->indirect_constraints.begin(),
         e = as->indirect_constraints.end(); i != e; i++) {
    old_indirect.push_back(*i);
  }

  as->indirect_constraints.clear();

  for (int i = 0; i < old_indirect.size(); i++) {
    Constraint &c = old_indirect[i].first;

    llvm::DenseMap<Constraint, u32>::iterator j = factored.find(c);
    if (f != this->constraints.end()) {
      if (c.type == ConstraintLoad)
        c.dest = j->second;
      else
        c.src = j->second;
    }

    const std::<llvm::Instruction *> &ins = old_indirect[i].second;
    as->indirect_constraints[c].insert(ins.begin(), ins.end());
  }

  factored.clear();
}
