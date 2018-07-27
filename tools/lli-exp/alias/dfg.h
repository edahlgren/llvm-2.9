class DataFlowGraph {
public:
  vector<TpNode> tp_nodes; // {alloc,copy,gep} instructions
  vector<NpNode> np_nodes; // noop instructions
  vector<LdNode> ld_nodes; // load instructions
  vector<StNode> st_nodes; // store instructions
    
  // indices >= tp_base and < ld_base refer to tp_nodes; >= ld_base
  // and < st_base refer to ld_nodes; etc for st and np
  //
  u32 tp_base, ld_base, st_base, np_base;
  
  DataFlowGraph() : tp_base(0), ld_base(0), st_base(0), np_base(0) {}
    
  // insert a constraint into the DFG, creating a node for it;
  // return the index of the node in the particular class it belongs
  // to (ie, Tp/Ld/St/Np), rather than in the overall index space
  //
  u32 insert(const Constraint& C) {
    switch (C.type) {
    case addr_of_cons:
    case copy_cons:
    case gep_cons:
      tp_nodes.push_back(TpNode(C));
      return tp_nodes.size()-1;
    case load_cons:
      ld_nodes.push_back(LdNode(C));
      return ld_nodes.size()-1;
    case store_cons:
      st_nodes.push_back(StNode(C));
      return st_nodes.size()-1;
    default: assert(0 && "unknown constraint type");
    }
  }
    
  // call this once all {Tp,Ld,St}Nodes have been inserted; it will
  // set the overall index namespace and create the top-level
  // def-use chains (ie, those involving top-level variables)
  //
  void finalize_insert() {
    tp_base = 0;
    ld_base = tp_nodes.size();
    st_base = ld_base + ld_nodes.size();
    np_base = st_base + st_nodes.size();

    map<u32,vector<u32> > v2d;
    FORN(i,tp_nodes.size()) {v2d[tp_nodes[i].inst.dest].push_back(tp_base+i);}
    FORN(i,ld_nodes.size()) {v2d[ld_nodes[i].inst.dest].push_back(ld_base+i);}
      
    FORN(i,tp_nodes.size()) {
      Constraint& C = tp_nodes[i].inst;
      if (C.type != addr_of_cons && v2d.count(C.src)) {
        vector<u32>& def = v2d[C.src];
        FOREACH(uv_it,j,def) { add_edge(*j,i); }
      }
    }

    FORN(i,ld_nodes.size()) {
      Constraint& C = ld_nodes[i].inst;
      vector<u32>& def = v2d[C.src];
      FOREACH(uv_it,j,def) { add_edge(*j,ld_base+i); }
    }

    FORN(i,st_nodes.size()) {
      Constraint& C = st_nodes[i].inst;
      vector<u32> &d1 = v2d[C.dest], &d2 = v2d[C.src];
      FOREACH(uv_it,j,d1) { add_edge(*j,st_base+i); }
      FOREACH(uv_it,j,d2) { add_edge(*j,st_base+i); }
    }
  }
    
  // insert a NpNode into the DFG; we assume this is done after
  // finalize_insert() has been called
  //
  u32 insert_nop() {
    np_nodes.push_back(NpNode());
    return (np_base + np_nodes.size()-1);
  }
    
  // return the type of node the index is for
  //
  bool is_tp(u32 i) { return (i < ld_base); }
  bool is_ld(u32 i) { return (i >= ld_base && i < st_base); }
  bool is_st(u32 i) { return (i >= st_base && i < np_base); }
  bool is_np(u32 i) { return (i >= np_base && i-np_base < np_nodes.size()); }
  
  node_type type(u32 i) const {
    if (i < ld_base) { return IS_TP; }
    if (i < st_base) { return IS_LD; }
    if (i < np_base) { return IS_ST; }
    assert(i < np_base + np_nodes.size());
    return IS_NP;
  }

  // return a node of the appropriate type
  //
  TpNode& get_tp(u32 i) { assert(is_tp(i)); return tp_nodes[i]; }
  LdNode& get_ld(u32 i) { assert(is_ld(i)); return ld_nodes[i-ld_base]; }
  StNode& get_st(u32 i) { assert(is_st(i)); return st_nodes[i-st_base]; }
  NpNode& get_np(u32 i) { assert(is_np(i)); return np_nodes[i-np_base]; }
  
  // translate an index for {st,ld}_nodes into the overall index
  // namespace (if d == true) or vice-versa (if d == false)
  //
  u32 st_idx(u32 i, bool d) const {
    assert((d && i < st_nodes.size()) || (!d && i >= st_base && i < np_base));
    return d ? i+st_base : i-st_base;
  }
    
  u32 ld_idx(u32 i, bool d) const { 
    assert((d && i < ld_nodes.size()) || (!d && i >= ld_base && i < st_base));
    return d ? i+ld_base : i-ld_base; 
  }
    
  // return the constraint associated with the given node
  //
  Constraint& node_cons(u32 i) {
    if (i < ld_base) { return tp_nodes[i].inst; }
    if (i < st_base) { return ld_nodes[i-ld_base].inst; }
    if (i < np_base) { return st_nodes[i-st_base].inst; }
    assert(0 && "noop nodes have no constraints");
  }

  // given a node with a points-to graph, fill it with the set of
  // all vars it may contain
  //
  void prep_node(u32 n, vector<u32>& vars) {
    switch (type(n)) {
    case IS_LD:
      get_ld(n).pts.init(vars);
      break;
    case IS_ST:
      StNode& N = get_st(n);
      N.in.init(vars);
      N.out.init(vars);
      break;
    case IS_NP:
      get_np(n).pts.init(vars);
      break;
    default: assert(0 && "unexpected type");
    }
  }

  // set an LdNode's rep
  //
  void set_rep(u32 ld, u32 rep) {
    assert(is_ld(ld));
    ld_nodes[ld-ld_base].rep = rep;
  }

  // return the number of shared nodes (ie, the number of LdNodes
  // with a non-zero rep)
  //
  u32 num_shared() {
    u32 cnt = 0;
    FORN(i,ld_nodes.size()) {	if (ld_nodes[i].rep) { cnt++; }}
    return cnt;
  }

  typedef vector<TpNode>::iterator tp_it;
  typedef vector<LdNode>::iterator ld_it;
  typedef vector<StNode>::iterator st_it;
  typedef vector<NpNode>::iterator np_it;
    
  // iterate over the TpNodes
  //
  tp_it tp_begin() { return tp_nodes.begin(); }
  tp_it tp_end()   { return tp_nodes.end();   }

  // iterator over the LdNodes
  //
  ld_it ld_begin() { return ld_nodes.begin(); }
  ld_it ld_end()   { return ld_nodes.end();   }
  
  // iterator over the StNodes
  //
  st_it st_begin() { return st_nodes.begin(); }
  st_it st_end()   { return st_nodes.end();   }

  // iterator over the NpNodes
  //
  np_it np_begin() { return np_nodes.begin(); }
  np_it np_end()   { return np_nodes.end();   }
    
  // insert an edge into the DFG
  //
  void add_edge(u32 src, u32 dst, u32 part = 0) {
    switch (type(src)) {
    case IS_TP: tp_nodes[src].succ.push_back(dst); break;
    case IS_LD:
      if (!part) { ld_nodes[src-ld_base].tl_succ.push_back(dst);     }
      else       { ld_nodes[src-ld_base].part_succ.insert(dst,part); }
      break;
    case IS_ST:
      assert(!is_tp(dst) && part);
      st_nodes[src-st_base].part_succ.insert(dst,part);
      break;
    case IS_NP:
      assert(!is_tp(dst) && src < np_base + np_nodes.size());
      np_nodes[src-np_base].succ.push_back(dst);
      break;
    default: assert(0 && "unknown type");
    }
  }
    
  u32 num_tp() const { return tp_nodes.size(); }
  u32 num_ld() const { return ld_nodes.size(); }
  u32 num_st() const { return st_nodes.size(); }
  u32 num_np() const { return np_nodes.size(); }
  u32 num_nodes() const { return np_base + np_nodes.size(); } 

  void stats(vector<bitmap>& vp) {
    assert(o2p.size());

    map<u32,u32> np2p, p2ne;
    u32 t1 = 0, t3 = 0, t4 = 0;

    FOREACH(tp_it,i,tp_nodes) { t1 += i->succ.size(); }
    FOREACH(ld_it,i,ld_nodes) { 
      t1 += i->tl_succ.size();

      FOREACH(pmap_it,j,i->part_succ) {
        if (!j->second) { continue; }
        assert(j->second < vp.size() && !vp[j->second].empty());
        p2ne[j->second]++;
        t3++; t4 += vp[j->second].count();
        if (is_np(j->first)) { np2p[j->first] = j->second; }
      }
    }
    FOREACH(st_it,i,st_nodes) {
      FOREACH(pmap_it,j,i->part_succ) {
        if (!j->second) { continue; }
        assert(j->second < vp.size() && !vp[j->second].empty());
        p2ne[j->second]++;
        t3++; t4 += vp[j->second].count();
        if (is_np(j->first)) { np2p[j->first] = j->second; }
      }
    }

    u32 cnt = 0, old;

    do{
      old = cnt;

      FOREACH(np_it,i,np_nodes) {
        u32 idx = np_base+(i-np_nodes.begin());
        if (!np2p.count(idx)) { continue; }
        FOREACH(uv_it,j,i->succ) {
          if (is_np(*j) && !np2p.count(*j)) { np2p[*j] = np2p[idx]; cnt++; }
        }
      }
    } while (cnt > old);

    FOREACH(np_it,i,np_nodes) {
      u32 idx = np_base+(i-np_nodes.begin());
      if (!np2p.count(idx)) { continue; }

      p2ne[np2p[idx]] += i->succ.size();
      t3 += i->succ.size();
      t4 += i->succ.size() * vp[np2p[idx]].count();
    }

    cerr << "num edges w/out ae, w/ top == " << t4+t1 << endl
         << "num edges with  ae, w/ top == " << t3+t1 << endl
         << "num edges w/out ae, no top == " << t4 << endl
         << "num edges with  ae, no top == " << t3 << endl;

    u32 tot = 0, num = 0;

    FOREACH(u2u_it,i,p2ne) {
      u32 nv = vp[i->first].count();
      num += nv;
      tot += (nv * i->second);
    }

    cerr << "avg edges/var == " << (double)tot/num << endl;
  }
};
