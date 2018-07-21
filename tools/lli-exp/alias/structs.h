class Structs {
  // min_struct: 
  //   When there are no structs, max_struct is assigned
  //   the smallest type.
  // max_struct:
  //   The struct type with the most fields (or min_struct
  //   if no structs are found).
  // max_struct_sz:
  //   The # of fields in max_struct (0 for min_struct).
  const llvm::Type *const min_struct;
  const llvm::Type* max_struct;
  u32 max_struct_sz;
  
  // Every struct type is mapped to vectors S (first) and O (second):
  //
  // * If field [i] in the expanded struct type begins an embedded struct,
  //   then S[i] is the # of fields in the largest such struct, else S[i] = 1.
  // * S[0] is always the size of the expanded struct T, since a pointer to
  //   the first field of T can mean all of T.
  // * If a field has index (j) in the original struct, it has index O[j] in
  //   the expanded struct.
  typedef llvm::DenseMap<const llvm::StructType*,
    std::pair<std::vector<u32>, std::vector<u32> >> StructInfoMap;
  StructInfoMap struct_info_map;

  Structs() : min_struct(llvm::Type::Int8Ty),
    max_struct(llvm::Type::Int8Ty), max_struct_sz(0) {}
  
  void analyze(const llvm::StructType *st);
};

