// Types of constraints.
enum ConstraintType {
  ConstraintAddrOf = 0, // (Base) Address-of:
                        //   d = &s;
  ConstraintCopy,       // (Simple) Copy:
                        //   d = s;
  ConstraintLoad,       // (Complex 1) Load:
                        //   d = *s (+ off);
  ConstraintStore,      // (Complex 2) Store:
                        //  *d (+ off) = s;
  ConstraintGEP,        // (Copy + offset) GEP:
                        //   d = s + off
};


class Constraint {
public:
  ConstraintType type;
  u32 dest, src, off;

  Constraint(ConsType t, u32 d, u32 s, u32 o = 0) :
    type(t), dest(d), src(s), off(o) {}

  bool operator == (const Constraint &b) const{
    return type == b.type && dest == b.dest && src == b.src && off == b.off;
  }
  
  bool operator < (const Constraint &b) const{
    if(type != b.type)
      return type < b.type;
    if(dest != b.dest)
      return dest < b.dest;
    if(src != b.src)
      return src < b.src;
    return off < b.off;
  }
  
  bool operator > (const Constraint &b) const{
    if(type != b.type)
      return type > b.type;
    if(dest != b.dest)
      return dest > b.dest;
    if(src != b.src)
      return src > b.src;
    return off > b.off;
  }
  
  bool operator != (const Constraint &b) const{
    return !(operator==(b));
  }
  
  bool operator >= (const Constraint &b) const{
    return !(operator<(b));
  }
  
  bool operator <= (const Constraint &b) const{
    return !(operator>(b));
  }

  void validate();
};

static void assert_valid_constraint(Constraint &c) {
  switch (c.type) {
  case ConstraintAddrOf:
    assert(c.dest != UnknownTarget);
    assert(!off);
    break;
  case ConstraintCopy:
    assert(src != UnknownTarget
           && dest != UnknownTarget);
    assert(!off);
    break;
  case ConstraintLoad:
    assert(src != UnknownTarget
           && dest != UnknownTarget
           && src != ConstToUnknownTarget);
    break;
  case ConstraintStore:
    assert(src != UnknownTarget
           && dest != UnknownTarget
           && dest != ConstToUnknownTarget);
    break;
  case ConstraintGEP:
    assert(src != UnknownTarget
           && dest != UnknownTarget);
    break;
  default:
      assert(false && "unknown constraint type");
  }
}

class Constraints {
  std::vector<Constraint> constraints;

  Constraints() {}

  bool add(ConsType t, u32 dest, u32 src, u32 off = 0) {
    assert(src && dest);
  
    if (t == ConstraintGEP && !off) {
      t = CopyCons;
    }
    if (t == ConstraintCopy && src == dest) {
      return false;
    }

    Constraint c(t, dest, src, off);
    assert_valid_constraint(c);
    constraints.push_back(c);
    return true;
  }
};
