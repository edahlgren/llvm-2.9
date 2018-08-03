//===- constraints.h - Memory access patterns -----------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#ifndef CONSTRAINTS_H
#define CONSTRAINTS_H

#include "int.h" // for u32

#include "llvm/ADT/DenseMap.h"        // for llvm::DenseMap
#include "llvm/Support/raw_ostream.h" // for llvm::raw_ostream

#include <set>    // for std::set
#include <vector> // for std::vector

// Types of constraints.
enum ConstraintType {
  ConstraintAddrOf = 0, //   d = &s;
  ConstraintCopy,       //   d = s;
  ConstraintLoad,       //   d = *s (+ off);
  ConstraintStore,      //  *d (+ off) = s;
  ConstraintGEP,        //   d = s + off
};

static std::string type_to_string(ConstraintType type) {
  switch (type) {
  case ConstraintAddrOf:
    return "ADDR_OF (d = &s)";
  case ConstraintCopy:
    return "COPY (d = s)";
  case ConstraintLoad:
    return "LOAD (d = *s)";
  case ConstraintStore:
    return "STORE (*d = s)";
  case ConstraintGEP:
    return "GEP (d = s + off)";
  default:
    assert(false && "invalid type");
  }
}

class Constraint {
public:
  ConstraintType type;
  u32 dest, src, off;

  Constraint(ConstraintType t, u32 d, u32 s, u32 o = 0) :
    type(t), dest(d), src(s), off(o) {}

  bool operator == (const Constraint &b) const {
    return type == b.type && dest == b.dest && src == b.src && off == b.off;
  }
  
  bool operator < (const Constraint &b) const {
    if(type != b.type)
      return type < b.type;
    if(dest != b.dest)
      return dest < b.dest;
    if(src != b.src)
      return src < b.src;
    return off < b.off;
  }
  
  bool operator > (const Constraint &b) const {
    if(type != b.type)
      return type > b.type;
    if(dest != b.dest)
      return dest > b.dest;
    if(src != b.src)
      return src > b.src;
    return off > b.off;
  }
  
  bool operator != (const Constraint &b) const {
    return !(operator==(b));
  }
  
  bool operator >= (const Constraint &b) const {
    return !(operator<(b));
  }
  
  bool operator <= (const Constraint &b) const {
    return !(operator>(b));
  }

  void assert_valid() {
    // TODO: implement
    return;
  }

  std::string to_string() {
    std::string str;
    llvm::raw_string_ostream os(str);

    os << "constraint {" << "\n";
    os.indent(1) << "type:\t" << type_to_string(type) << "\n";
    os.indent(1) << "(d)est:\t" << dest << "\n";
    os.indent(1) << "(s)rc:\t" << src << "\n";
    os.indent(1) << "offset:\t" << off << "\n";
    os << "}";
    
    return os.str();
  }
};

class Constraints {
 public:
  llvm::DenseSet<Constraint> constraints;
  typedef llvm::DenseSet<Constraint>::iterator iterator;
  
  Constraints() {}

  bool add(ConstraintType t, u32 dest, u32 src, u32 off = 0) {
    assert(src && dest);
  
    if (t == ConstraintGEP && !off) {
      t = ConstraintCopy;
    }
    if (t == ConstraintCopy && src == dest) {
      return false;
    }

    Constraint c(t, dest, src, off);
    c.assert_valid();
    
    std::pair<iterator, bool> result = constraints.insert(c);
    return result->second;
  }

  void print(llvm::raw_ostream &os) {
    os << "Constraints {" << "\n";
    for (iterator i = constraints.begin(), e = constraints.end();
         i != e; i++) {
      os.indent(1) << i.to_string() << "\n";
    }
    os << "}" << "\n";
  }

  void print() {
    print(llvm::out());
  }
};

namespace llvm{
  template<> struct DenseMapInfo<Constraint> {
    static Constraint getEmptyKey(){
      return Constraint(ConstraintAddrOf, 0, 0, 0);
    }
    
    static Constraint getTombstoneKey(){
      return Constraint(ConstraintCopy, 0, 0, 0);
    }
    
    static unsigned getHashValue(const Constraint &x){
      return ((u32)x.type<<29) ^ (x.dest<<12) ^ x.src ^ x.off;
    }
    
    static unsigned isEqual(const Constraint &x, const Constraint &y){
      return x == y;
    }
  };
}

typedef std::pair<Constraint, std::set<llvm::Instruction *>> ConstraintInstSet;
typedef llvm::DenseMap<Constraint, std::set<llvm::Instruction *>> ConstraintInstMap;

#endif // end CONSTRAINTS_H
