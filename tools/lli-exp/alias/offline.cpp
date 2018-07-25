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

typedef u32 Src;
typedef u32 Dest;
typedef u32 Off;

static const u32 factor_min_sz= 2;
static llvm::DenseMap<Constraint, u32> factored_cons;

static void merge_load_constraint(AnalysisSet *as,
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

static void merge_store_constraints(AnalysisSet *as,
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

void factor_load_store_constraints(AnalysisSet *as) {
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
