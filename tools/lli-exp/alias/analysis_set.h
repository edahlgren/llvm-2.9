typedef std::pair<Constraint, std::set<llvm::Instruction *>> ConstraintInstSet;
typedef llvm::DenseMap<Constraint, std::set<llvm::Instruction *> > ConstraintInstMap;

class AnalysisSet {
 public:
  Nodes *nodes;
  Constraints *constraints;
  ExtInfo ext_info;
  Structs *structs;

  std::set<u32> indirect_calls;
  ConstraintInstMap indirect_constraints;

  NodeMap deref_to_var_nodes;
};

AnalysisSet *init_analysis_set(llvm::Module *m);

class FlowAnalysisSet {
 public:
  std::vector<PtsSet> top;
  std::vector<bool> strong;
  std::vector<u32> priority;

  DataFlowGraph *dfg;

  bdd pdom;
  bddPair *g_to_p;
  std::vector<bdd> bdd_off;
  std::vector<bdd> bdd_xlt;
  std::map<u32, std::vector<u32> > global_to_dfg;

  std::vector<u32> defs;
  std::vector<u32> uses;  
};

FlowAnalysisSet *init_flow_analysis_set(AnalysisSet *as);
