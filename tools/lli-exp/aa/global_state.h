
class ModuleState {
 public:
  Structs structs;
  ExtInfo ext_info;
  std::string entry_point;

  ModuleState() : entry_point("main") {}
  ModuleState(std::string entry_point) : entry_point(entry_point) {}
}

class IDSet {
 public:
  llvm::DenseMap<u32, u32> cache;

  void add(u32 id, u32 num_fields) {
    cache[id] = num_fields;
  }

  bool lookup(u32 id, u32 *num_fields = 0) {
    llvm::DenseMap<u32, u32>::iterator i = cache.find(id);
    if (i == cache.end()) {
      return false;
    }
    if (num_fields)  {
      *num_fields = i->second;
    }
    return true;
  }
};

class GlobalState {
 public:
  ModuleState *module;
  IDSet done_set;
  Nodes *nodes;
  Constraints *constraints;  
  llvm::DenseSet<llvm::Value *> addr_taken_args;
  
  GlobalState(const ModuleState *module) : module(module) {
    nodes = new Nodes();
    constraints = new Constraints();
  }

  void process_function_signature(const llvm::Function *f);
  void process_global_signature(const llvm::GlobalVariable *g);
  void process_global_value(const llvm::GlobalVariable *g);
};
