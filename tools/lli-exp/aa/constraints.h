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
#include "seg.h" // for SEG

#include "llvm/ADT/DenseMap.h"        // for llvm::DenseMap
#include "llvm/ADT/DenseSet.h"        // for llvm::DenseSet
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

  std::string type_string() {
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
 
  std::string to_string(int l = 0) {
    std::string str;
    llvm::raw_string_ostream os(str);

    os.indent(l) << "constraint {" << "\n";
    os.indent(l + 1) << "type:\t\t" << type_string() << "\n";
    os.indent(l + 1) << "(d)est:\t" << dest << "\n";
    os.indent(l + 1) << "(s)rc:\t" << src << "\n";
    os.indent(l + 1) << "offset:\t" << off << "\n";
    os.indent(1) << "}";
    
    return os.str();
  }
};

class Constraints {
 public:
  std::vector<Constraint> constraints;
  //llvm::DenseSet<Constraint> constraints;
  typedef std::vector<Constraint>::iterator iterator;
  
  Constraints() {}

  iterator begin() { return constraints.begin(); }
  iterator end() { return constraints.end(); }

  Constraint *find(u32 index) {
    return &constraints[index];
  }

  u32 size() {
    return constraints.size();
  }

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

    // We don't care about duplicate constraints in this case,
    // if we did, we'd do:
    //
    //  typedef llvm::DenseSet<Constraint>::iterator iterator;
    //  std::pair<iterator, bool> result = constraints.insert(c);
    //  return result.second;
    //
    // Check back on this.
    constraints.push_back(c);
    return true;
  }

  void print(llvm::raw_ostream &os, int l = 0) {
    os.indent(l) << "Constraints {" << "\n";
    for (iterator i = begin(), e = end(); i != e; ++i) {
      Constraint c = *i;
      os << c.to_string(l + 1) << "\n";
    }
    os.indent(l) << "}" << "\n";
  }

  void print(int indent_level = 0) {
    print(llvm::outs(), indent_level);
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

struct ConstraintGraphMetadata {
  // Start nodes for BasicBlocks
  std::map<llvm::BasicBlock *, u32> bb_start;
  typedef std::map<llvm::BasicBlock *, u32>::iterator bb_iterator;

  // (fun_start)
  // Start nodes for Functions  
  std::map<llvm::Function *, u32> func_start_nodes;

  // (fun_ret)
  // function -> return node
  std::map<llvm::Function *, u32> func_ret_nodes;

  // (fun_cs)
  // callsite -> function targets
  std::map<u32, std::vector<llvm::Function *> > func_callsites;

  // (call_succ)
  // callsite -> local successor
  std::map<u32, u32> callsite_succ;

  // (idr_cons)
  // indices of constraints from idr calls
  std::vector<u32> indirect_call_cons;

  // (idr_calls)
  // <idr call, callsite> pairs
  std::vector<std::pair<llvm::CallInst *, u32> > indirect_call_pairs;

  // (icall_cons)
  llvm::DenseMap<Constraint, std::set<llvm::Instruction*>> ret_arg_call_cons;

  // (ind_calls)
  std::set<u32> indirect_call_func_nodes;
};

const u32 default_graph_max_size = 1000000000;

class ConstraintGraph : public SEG {
 public:
  // Maps a store constraint to an SEG node, e.g.:
  //
  // i = store constraint index;
  // defs[i] -> SEG node index.
  std::vector<u32> defs;
  
  // Maps a load constraint to an SEG node, e.g.:
  //
  // i = load constraint index;
  // defs[i] -> SEG node index.
  std::vector<u32> uses;

  llvm::DenseMap<u32, bool> uses_relevant_def;

  // Temporary state.
  // =============================================
  ConstraintGraphMetadata *meta;

   ConstraintGraph(u32 size, u32 max_size) : SEG(max_size) {
    defs.assign(size);
    uses.assign(size);
    meta = new ConstraintGraphMetadata();
  }
};

typedef std::pair<Constraint, std::set<llvm::Instruction *>> ConstraintInstSet;
typedef llvm::DenseMap<Constraint, std::set<llvm::Instruction *>> ConstraintInstMap;

#endif // end CONSTRAINTS_H
