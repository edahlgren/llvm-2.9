#define PARENT_ENTRY_BLOCK MAX_U32

struct MutableFunctionState {
  bool owns_nodes;
  Nodes *nodes;

  bool owns_constraints;
  Constraints *constraints;

  bool owns_constraint_graph;
  ConstraintGraph *constraint_graph;

  MutableFunctionState(Nodes *nodes,
                       Constraints *constraints,
                       ConstraintGraph *constraint_graph)
    : owns_nodes(true),
      nodes(nodes),
      owns_constraints(true),
      constraints(constraints),
      owns_constraint_graph(true),
      constraint_graph(constraint_graph) {}

  ~MutableFunctionState() {
    if (owns_nodes)
      delete nodes;

    if (owns_constraints)
      delete constraints;

    if (owns_constraint_graph)
      delete constraint_graph;
  }
  
  Nodes *acquire_nodes() {
    owns_nodes = false;
    return nodes;
  }

  Constraints *acquire_constraints() {
    owns_constraints = false;
    return constraints;
  }

  ConstraintGraph *acquire_constraint_graph() {
    owns_constraint_graph = false;
    return constraint_graph;
  }
};

typedef unsigned Opcode;
typedef void (*InstHandler)(FunctionState *, BlockState *, llvm::Instruction *);
typedef std::map<Opcode, InstHandler> InstHandlers;

class FunctionState {
public:
  const AnalysisSet *global_state;
  const InstHandlers handlers;

  bool owns_mutable_state;
  Nodes *nodes;
  Constraints *constraints;
  ConstraintGraph *constraint_graph;

  BlockSet block_cache;  
  
  FunctionState(const AnalysisSet *as,
                const InstHandlers handlers)
    : global_state(as),
      handlers(handlers),
      owns_mutable_state(true) {
    
    nodes = new Nodes();
    constraints = new Constraints();
    constraint_graph = new ConstraintGraph();
  }

  MutableFunctionState acquire_mutable_state() {
    owns_mutable_state = false;
    return MutableFunctionState(nodes, constraints, constraint_graph);
  }
  
  ~FunctionState() {
    if (owns_mutable_state) {
      delete nodes;
      delete constraints;
      delete constraint_graph;
    }
  }

  void process(const llvm::Function *f);
  void process_block(llvm::BasicBlock *bb, u32 parent_index);
};

const InstHandlers default_instruction_handlers();
