typedef llvm::DenseMap<Constraint, std::set<llvm::Instruction *> > ConstraintInstMap;
  
class AnalysisSet {
  Nodes *nodes;
  Constraints *constraints;
  ConstraintInstMap indirect_constraints;
};
