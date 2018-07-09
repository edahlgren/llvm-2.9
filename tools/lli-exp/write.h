//===- write.h - Printing interfaces --------------------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#ifndef WRITE_H
#define WRITE_H

#include "cfg.h"
#include "llvm/Module.h"
#include "llvm/Support/raw_ostream.h"

#include <iostream>

void write_function_control_flow(llvm::Function *f, llvm::raw_ostream &os);
void write_function_dominator_tree(llvm::Function *f, llvm::raw_ostream &os);
void write_function_dominator_tree_old(llvm::Function *f, llvm::raw_ostream &os);
void write_function_loops(llvm::Function *f, llvm::raw_ostream &os);
void write_function_graph(FunctionGraph *fg, std::ostream &os);

#endif // end WRITE_H
