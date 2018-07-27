class AndersSolution {
 public:
  AnalysisSet *as;
  BDDSets *bdds;

 AndersSolution(AnalysisSet *as, BDDSets *bdds) :
  as(as), bdds(bdds) {}

  bool is_single(u32 index, u32 off);
  bool is_null(u32 index, u32 off);
  
  std::vector<u32> *points_to_set(u32 index, u32 offset);
  std::vector<u32> *points_to_set(llvm::Value *v, u32 offset);

  u32 pe(u32 index);
  u32 pe(llvm::Value *v);
};
