class ConstraintClasses {
  std::set<u32> cons_strong;
  std::vector<u32> cons_store;
  std::vector<u32> cons_load;  
};

class Partitions {
  std::vector<u32> obj_to_part;

  std::map<u32, std::vector<u32> > cons_part;
  typedef std::map<u32, std::vector<u32> >::iterator cons_part_iterator;

  std::vector<bitmap> var_part;  
  typedef std::vector<bitmap>::iterator var_part_iterator;

  std::map<u32, bitmap> obj_to_cons_part;
  typedef std::map<u32, bitmap>::iterator obj_to_cons_part_iterator;

  std::map<u32, std::vector<u32> > dfg_to_part;
  typedef std::map<u32, std::vector<u32> >::iterator dfg_to_part_iterator;
};

Partitions *partition_variables(FlowAnalysisSet *fas,
                                ConstraintClasses *cc,
                                AndersSolution *as);
