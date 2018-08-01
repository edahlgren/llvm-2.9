set (CMAKE_CXX_STANDARD 11)

set(LLVM_REQUIRES_RTTI 1)

set(LLVM_LINK_COMPONENTS ipa mcjit jit interpreter nativecodegen bitreader asmparser selectiondag)

include_directories(${CMAKE_CURRENT_SOURCE_DIR})

add_subdirectory(aa)

add_llvm_tool(lli-exp
  lli-exp.cpp
  analysis.cpp
  cfg.cpp
  dominator.cpp
  graphml.cpp
  loop.cpp
  loop_new.cpp
  trace.cpp
  write.cpp
)

target_link_libraries(lli-exp lli-alias)
