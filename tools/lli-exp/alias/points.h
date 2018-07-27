class PtsSet {
public:    
  bdd pts;

  PtsSet() {}
  PtsSet(bdd p) : pts(p) {}
    
  bool insert(u32 x) {
    bdd old = pts;
    pts |= ::get_bdd(x);
    return (pts != old);
  }

  PtsSet& operator=(const PtsSet& rhs) {
    pts = rhs.pts;
    return (*this);
  }
    
  bool operator|=(const PtsSet& rhs) {
    bdd old = pts;
    pts |= rhs.pts;
    return (pts != old);
  }
    
  PtsSet operator+(u32 off) const {
    if (off == 0) { return *this; }
    assert(off < bdd_off.size() && bdd_off[off] != bddfalse);
    return PtsSet(bdd_replace(bdd_relprod(pts,bdd_off[off],pdom),g2p));
  }

  bool operator==(const PtsSet& rhs) {
    return pts == rhs.pts;
  }

  bool operator!=(const PtsSet& rhs) {
    return !(*this == rhs);
  }

  bdd get_bdd() const { return pts; }
};

class change_set {
 public:

  change_set() {}
      
  void insert(u32 x) { S.push_back(x); }

  void uniq() {
    std::sort(S.begin(),S.end());
    uv_it e = std::unique(S.begin(),S.end());
    S.erase(e,S.end());
  }
      
  void clear() { S.clear(); }
  
  bool empty() { return S.empty(); }
  
  bool has(u32 x) {	return binary_search(S.begin(),S.end(),x); }

  typedef vector<u32>::iterator ch_it;

  ch_it begin() { return S.begin(); }
  ch_it end()   { return S.end();   }

 private:
  vector<u32> S;
};


struct pts_comp {
  bool operator()(const pts_el& lhs, const pts_el& rhs) const {
    return (lhs.first < rhs.first);
  }
};

// points-to graph for address-taken variables
//
class PtsGraph {
 public:
    
  change_set change;
  typedef change_set::ch_it ch_it;
    
  typedef pair<u32,PtsSet> pts_el;
  typedef vector<pts_el>::iterator pts_it;
  typedef vector<pts_el>::const_iterator pts_cit;

  vector<pts_el> pts;
    
  PtsGraph() {}
    
  void init(vector<u32>& vars) {
    sort(vars.begin(),vars.end());
    pts.resize(vars.size(),pts_el());
    FORN(i,vars.size()) { pts[i].first = vars[i]; }
  }

  PtsSet operator[](u32 el) {
    pts_cit i = pts_find(el);
    if (i == pts.end()) { return bddfalse; }
    return i->second;
  }
    
  bool operator|=(PtsGraph& rhs) {
    bool c = false;

    FOREACH(ch_it,i,rhs.change) {
      pts_cit k = rhs.pts_find(*i); assert(k != rhs.pts.end());
      pts_it j = pts_find(k->first); assert(j != pts.end());
      if (j->second |= k->second) { c = true; change.insert(j->first); }
    }

    return c;
  }

  bool or_part(PtsGraph& rhs, u32 part) {
    bool c = false;

    FOREACH(ch_it,i,rhs.change) {
      if (o2p[*i] != part) { continue; }
      pts_cit k = rhs.pts_find(*i); assert(k != rhs.pts.end());
      pts_it j = pts_find(k->first); assert(j != pts.end());
      if (j->second |= k->second) { c = true; change.insert(j->first); }
    }

    return c;
  }

  bool or_except(PtsGraph& rhs, u32 el) {
    bool c = false;
      
    FOREACH(ch_it,i,rhs.change) {
      if (*i == el) { continue; }
      pts_cit k = rhs.pts_find(*i); assert(k != rhs.pts.end());
      pts_it j = pts_find(k->first); assert(j != pts.end());
      if (j->second |= k->second) { c = true; change.insert(j->first); }
    }
      
    return c;
  }

  void assign_el(u32 el, const PtsSet& rhs) {
    pts_it i = pts_find(el); assert(i != pts.end());
    if (i->second != rhs) { change.insert(el); }
    i->second = rhs;
  }

  void or_el(u32 el, const PtsSet& rhs) {
    pts_it i = pts_find(el); assert(i != pts.end());
    if (i->second |= rhs) { change.insert(el); }
  }
  
  void or_changed(PtsSet& lhs, const vector<u32>& v) {
    change.uniq();
    FOREACH(uv_cit,i,v) { if (change.has(*i)) { lhs |= (*this)[*i]; }}
  }
    
  bool check() { change.uniq(); return !change.empty();  }
  void rst()   { change.clear(); }

  bool empty() { return pts.empty(); }

  pts_it pts_find(u32 el) {
    return std::lower_bound
      (pts.begin(),pts.end(),pts_el(el,bddfalse),pts_comp());
  }
};
