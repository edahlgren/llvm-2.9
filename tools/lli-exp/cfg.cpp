//===- cfg.cpp - Control Flow utilities -----------------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#include "analysis.h"
#include "write_dot.h"
#include "dot_traits.h"

#include "llvm/Module.h"
#include "llvm/Support/raw_ostream.h"

void print_function_control_flow(llvm::Function *f, llvm::raw_ostream &os) {  
  llvm::write_dot_graph(f, os);
}
