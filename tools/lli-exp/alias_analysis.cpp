//===- alias_analysis.cpp - -----------------------------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

void run_anaysis(llvm::Module *m) {
  AnalysisSet *as = new AnalysisSet(m);
  delete as;
}