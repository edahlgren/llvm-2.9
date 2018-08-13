//===- structs.h - Structure analysis -------------------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#ifndef STRUCTS_H
#define STRUCTS_H

#include "predicates.h" // for struct_type, const_int
#include "int.h"        // for u32

#include "llvm/ADT/DenseMap.h"    // for llvm::DenseMap
#include "llvm/Assembly/Writer.h" // for llvm::WriteAsOperand
#include "llvm/DerivedTypes.h"    // for llvm::StructType
#include "llvm/LLVMContext.h"     // for llvm::getGlobalContext
#include "llvm/Support/GetElementPtrTypeIterator.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Type.h"            // for llvm::Type
#include "llvm/User.h"            // for llvm::User

#include <vector>  // for std::vector
#include <utility> // for std::pair, std::make_pair

// StructEmbeddedInfo contains information about embedded
// structures.
//
// Note: The expanded version of a structure is where all of
// the embedded fields are raised up (i.e. "flattened") into
// the struct.
struct StructEmbeddedInfo {
  // For each field i in the struct, num_embedded_elements[i]
  // is the number of fields embedded in field i. For example:
  //
  //   + If field i is a struct:
  //       Then num_embedded_elements[i] is the number of fields of
  //       the largest structure embedded here (might be the
  //       struct at field i or another struct it contains).
  //
  //   + If field i is an array:
  //       Then num_embedded_elements[i] is 1. This treats arrays
  //       as a single elements of its type.
  //
  //   + If field i is anything else:
  //       Then num_embedded_elements[i] is 1 because the field is
  //       a simple type.
  //
  // Special case: field 0
  //
  //   A pointer to the first field of a struct can mean a pointer
  //   to all of the struct. So num_embedded_fields[0] always
  //   contains the number of elements of the expanded version of
  //   the struct.
  std::vector<u32> num_embedded_fields;

  // For each field i in the struct, offset[i] is the offset of
  // field i in the expanded version of the structure.
  std::vector<u32> offsets;

  std::string to_string(int l = 0) {
    std::string str;
    llvm::raw_string_ostream os(str);

    os.indent(l) << "info {" << "\n";    
    os.indent(l + 1) << "embedded fields: {" << "\n";
    for (u32 i = 0; i < num_embedded_fields.size(); i++) {
      os.indent(l + 2) << "field: " << i << ", embedded: " << num_embedded_fields[i] << "\n";
    }
    os.indent(l + 1) << "}" << "\n";

    os.indent(l + 1) << "offsets: {" << "\n";
    for (u32 i = 0; i < offsets.size(); i++) {
      os.indent(l + 2) << "field: " << i << ", offset: " << offsets[i] << "\n";
    }
    os.indent(l + 1) << "}" << "\n";
    os.indent(l) << "}";

    return os.str();
  }
};

// Structs lazily builds and caches StructEmbeddedInfo.
class Structs {
 public:
  // The expanded struct type with the most number of fields.
  const llvm::Type* max_struct;

  // The number of fields in the expanded version of max_struct.
  u32 max_struct_sz;
  
  // This maps structure types to information about their embedded
  // structures.
  typedef llvm::DenseMap<const llvm::StructType *, StructEmbeddedInfo>
    StructInfoMap;
  StructInfoMap struct_info_map;

  Structs() :
    max_struct(llvm::Type::getInt8Ty(llvm::getGlobalContext())),
    max_struct_sz(0) {}

  u32 size() {
    return struct_info_map.size();
  }

  // Returns the number of embedded fields for each field in st.
  const std::vector<u32> get_sz(const llvm::StructType *st){
    return get_info(st).num_embedded_fields;
  }

  // Returns the offset of each field in the expanded version
  // of st.
  const std::vector<u32> get_off(const llvm::StructType *st){
    return get_info(st).offsets;
  }
  
  // Return and cache the StructEmbeddedInfo of st.
  const StructEmbeddedInfo get_info(const llvm::StructType *st) {
    assert(st);

    // Is it cached?
    StructInfoMap::iterator cached_it = struct_info_map.find(st);
    if (cached_it != struct_info_map.end()) {
      return cached_it->second;
    }

    StructEmbeddedInfo info;
    u32 expanded_num_fields = 0;

    for (llvm::StructType::element_iterator i = st->element_begin(),
           e = st->element_end(); i != e; ++i){
      const llvm::Type *et= *i;

      // The offset of this element is after all of the previous
      // expanded fields.
      info.offsets.push_back(expanded_num_fields);
      
      // Handle arrays.
      while (const llvm::ArrayType *at = llvm::dyn_cast<llvm::ArrayType>(et))
        et = at->getElementType();

      // Handle structs.
      const llvm::StructType *nst = llvm::dyn_cast<llvm::StructType>(et);
      if (nst) {
        // Get cached info or process struct.
        const StructEmbeddedInfo nested_info = get_info(nst);

        // Increase expanded version.
        u32 nested_num_fields = nested_info.num_embedded_fields.size();
        expanded_num_fields += nested_num_fields;

        // Add embedded fields.
        for (u32 j = 0; j < nested_num_fields; j++) {
          u32 num_embedded = nested_info.num_embedded_fields[j];
          info.num_embedded_fields.push_back(num_embedded);
        }

        continue;
      }

      // Handle simple types.
      expanded_num_fields++;
      info.num_embedded_fields.push_back(1);
    }

    // Associate first field with expanded structure.
    info.num_embedded_fields[0] = expanded_num_fields;

    // Update maximum expanded structure if necessary.
    if (expanded_num_fields > max_struct_sz) {
      max_struct = st;
      max_struct_sz = expanded_num_fields;
    }

    // Cache and return.
    struct_info_map[st] = info;
    return std::move(info);
  }

  std::string entry_to_string(const llvm::StructType *st,
                              StructEmbeddedInfo info,
                              int l = 0) {
    std::string str;
    llvm::raw_string_ostream os(str);

    os.indent(l) << "struct {" << "\n";
    os.indent(l + 1) << "type: ";
    st->print(os);
    os << "\n";
    os << info.to_string(l + 1) << "\n";
    os.indent(l) << "}";

    return os.str();
  }

  void print(llvm::raw_ostream &os, int l = 0) {
    os.indent(l) << "Structs {" << "\n";
    for (StructInfoMap::iterator i = struct_info_map.begin(),
           e = struct_info_map.end(); i != e; i++) {
      os << entry_to_string(i->first, i->second, l + 1) << "\n";
    }
    os.indent(l) << "}" << "\n";
  }

  void print(int indent_level = 0) {
    print(llvm::outs(), indent_level);
  }
};

inline u32 gep_struct_off(Structs &structs, const llvm::User *u) {
  assert(u);

  u32 off = 0;
  for (llvm::gep_type_iterator i = llvm::gep_type_begin(*u),
         e = llvm::gep_type_end(*u); i != e; i++) {

    const llvm::StructType *st = struct_type(*i);
    if (!st) {
      continue;
    }

    const llvm::ConstantInt *op = const_int(i.getOperand());
    u32 index = op ? op->getZExtValue() : 0;
      
    const std::vector<u32> offsets = structs.get_off(st);
    assert(index < offsets.size());

    off += offsets[index];
  }

  return off;
}

#endif // end STRUCTS_H
