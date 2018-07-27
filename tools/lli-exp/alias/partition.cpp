class SqueezeMap {
  typedef std::map<std::pair<u32,u32>, u32> Map;
  typedef std::map<u32, std::pair<u32,u32>> Unmap;
  
  Map sq_map;
  Unmap sq_unmap;

  u32 squeeze(u32 p, u32 o, bool save = false) {
    static u32 idx = 1;
    std::pair<u32, u32> x(p,o);

    Map::iterator i = sq_map.find(x);
    if (i == sq_map.end()) {
      sq_map[x] = idx++; assert(idx < MAX_U32);
      if (save) { sq_unmap[idx-1] = x; }
      return idx-1;
    } else {
      return i->second;
    }
  }

  std::pair<u32,u32> unsqueeze(u32 n) { 
    Unmap::iterator i = sq_unmap.find(n);
    assert(i != sq_unmap.end());
    return i->second;
  }
};

Partitions *partition_variables(FlowAnalysisSet *fas,
                               ConstraintClasses *cc,
                               AndersSolution *as) {

  Partitions *ps = new Partitions();
  SqueezeMap *sm = new SqueezeMap(); 

  for (int i = 0; i < fas->defs.size(); i++) {
    if (!fas->defs[i]) {
      continue;
    }

    u32 index = fas->dfg->st_idx(i, true);
    Constraint &c = fas->dfg->node_cons(index);
    assert(c.type == ConstraintStore);

    u32 squeezed = sm->squeeze(as->pe(c.dest), c.off, true);
    ps->cons_part[squeezed].push_back(index);

    if (as->is_single(c.dest, c.off)) {
      std::vector<u32> &pts = *(as->points_to_set(c.dest, c.off));

      u32 only = pts.front();
      if (fas->strong[only]) {
        cc->strong_cons.insert(index);
      }
      continue;
    }
    
    if (as->is_null(c.dest, c.off)) {
        cc->strong_cons.insert(index);
    }
  }

  for (int i = 0; i < fas->uses.size(); i++) {
    if (!fas->uses[i]) {
      continue;
    }

    u32 index = fas->dfg->ld_idx(i, true);
    Constraint &c = fas->dfg->node_cons(index);
    assert(c.type == ConstraintLoad);

    u32 squeezed = sm->squeeze(as->pe(c.src), c.off, true);
    ps->cons_part[squeezed].push_back(index);
  }

  for (Partitions::cons_part_iterator i = ps.cons_part.begin();
       e = ps.cons_part.end(); i != e; i++) {
    u32 squeezed = i->first;
    
    std::pair<u32, u32> acc = sm->unsqueeze(squeezed);
    std::vector<u32> &pts = *(as->points_to_set(acc.first, acc.second));

    for (std::vector<u32>::iterator it = pts.begin(); ie = pts.end();
         it != ie; it++) {
      ps->obj_to_cons_part[*it].set(squeezed);
    }
  }

  delete sm;

  assert(!ps->cons_part.count(0));

  for (FlowAnalysisSet::global_to_dfg_iterator i = global_to_dfg.begin(),
         e = global_to_dfg.end(); i != e; i++) {
    u32 global = i->first;
    if (ps->obj_to_cons_part.count(global)) {
      ps->obj_to_cons_part[global].set(0);
    }
  }

  // This could be put in its own function.
  std::hash_map<bitmap, u32> eq;
  ps->var_part.push_back(bitmap());

  for (Partitions::obj_to_cons_part_iterator i = ps.obj_to_cons_part.begin(),
         e = obj_to_cons_part.end(); i != e; i++) {
    u32 cl = eq[i->second];

    if (!cl) {
      ps->var_part.push_back(bitmap());
      cl = ps.var_part.size() - 1;

      eq[i->second] = cl;
    }

    ps->var_part[cl].set(i->first);
  }

  eq.clear();

  ps->obj_to_part.assign(as->as->last_obj() + 1, 0);

  for (int i = 1; i < ps->var_part.size(); i++) {
    for (bitmap::iterator it = ps->var_part[i].begin(),
           ie = ps->var_part[i].end(); it != ie; it++) {
      ps->obj_to_part[*it] = i;
    }
  }  

  return ps;
}
