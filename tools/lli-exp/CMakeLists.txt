set(LLVM_LINK_COMPONENTS ipa mcjit jit interpreter nativecodegen bitreader asmparser selectiondag)

add_llvm_tool(lli-exp
  lli-exp.cpp
)
