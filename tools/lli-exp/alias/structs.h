class Structs {
  // min_struct: 
  //   When there are no structs, max_struct is assigned
  //   the smallest type.
  // max_struct:
  //   The struct type with the most fields (or min_struct
  //   if no structs are found).
  // max_struct_sz:
  //   The # of fields in max_struct (0 for min_struct).
  const llvm::Type *const min_struct= llvm::Type::Int8Ty;
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

  void analyze(const llvm::StructType *st);
};

static void analyze_struct(const llvm::StructType *st) {
  assert(st);
  
  if (struct_info_map.count(st)) {
    // Skip if the struct type is already present.
    return;
  }
  
  u32 num_fields = 0;
  std::vector<u32> sz, off;
  
  for (llvm::StructType::element_iterator i = st->element_begin(),
         e = st->element_end(); i != e; ++i){
    const llvm::Type *et= *i;

    // Treat an array field as a single element of its type.
    while (const llvm::ArrayType *at = llvm::dyn_cast<llvm::ArrayType>(et))
      et = at->getElementType();

    // The offset is where this element will be placed in the exp. struct.
    off.push_back(num_fields);
    
    // Process nested struct.
    if (const llvm::StructType *nst = llvm::dyn_cast<llvm::StructType>(et)) {
      StructInfoMap::iterator nst_it = struct_info_map.find(nst);
      if (nst_it == struct_info_map.end()) {
        // Recurse.
        analyze_struct(nst);
        nst_it = struct_info_map.find(nst);
      }      
      
      const std::vector<u32> &sz_element = nst->second.first;
      num_fields += sz_element.size();
      
      // Copy the nested struct's info, whose element 0 is the size of the
      // nested struct itself.
      for (u32 j = 0; j < sz_element.size(); j++) {
        sz.push_back(sz_element[j]);
      }      
    } else{
      // Process a simple type.
      sz.push_back(1);
      num_fields++;
    }
  }

  // Record the size of the complete struct and update max_struct.
  sz[0]= num_fields;  
  if(num_fields > max_struct_sz){
    max_struct = st;
    max_struct_sz= num_fields;
  }
  
  struct_info_map[st] = std::make_pair(sz, off);
}
