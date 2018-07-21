#include "structs.h"

void Structs::analyze(const llvm::StructType *st) {
  assert(st);
  
  if (this->struct_info_map.count(st)) {
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
      StructInfoMap::iterator nst_it = this->struct_info_map.find(nst);
      if (nst_it == this->struct_info_map.end()) {
        // Recurse.
        this->analyze(nst);
        nst_it = this->struct_info_map.find(nst);
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
  sz[0] = num_fields;  
  if(num_fields > max_struct_sz){
    this->max_struct = st;
    this->max_struct_sz= num_fields;
  }
  
  this->struct_info_map[st] = std::make_pair(sz, off);
}
