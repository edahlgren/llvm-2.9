set (CMAKE_CXX_STANDARD 11)

set(LLVM_REQUIRES_RTTI 1)

set(LLVM_LINK_COMPONENTS ipa mcjit jit interpreter nativecodegen bitreader asmparser selectiondag)

add_llvm_tool(lli-exp
  lli-exp.cpp
  analysis.cpp
  trace.cpp
  cfg.cpp
  dominator.cpp
  graphml.cpp
)
