typedef std::pair<Constraint, std::set<llvm::Instruction *>> ConstraintInstSet;
typedef llvm::DenseMap<Constraint, std::set<llvm::Instruction *> > ConstraintInstMap;

class AnalysisSet {
  Nodes *nodes;
  Constraints *constraints;
  ExtInfo ext_info;
  Structs *structs;

  std::set<u32> indirect_calls;
  ConstraintInstMap indirect_constraints;

  NodeMap deref_to_var_nodes;
};
