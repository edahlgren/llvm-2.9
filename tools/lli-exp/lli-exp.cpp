//===- lli-exp.cpp - Experimental lli tool -------------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#include "analysis.h"

#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/Type.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/IRReader.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/Process.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Signals.h"

#include <cerrno>

#ifdef __CYGWIN__
#include <cygwin/version.h>
#if defined(CYGWIN_VERSION_DLL_MAJOR) && CYGWIN_VERSION_DLL_MAJOR<1007
#define DO_NOTHING_ATEXIT 1
#endif
#endif

using namespace llvm;

namespace {
  cl::opt<std::string>
  InputFile(cl::desc("<input bitcode>"), cl::Positional, cl::init("-"));
}

static void do_shutdown() {
  // Cygwin-1.5 invokes DLL's dtors before atexit handler.
#ifndef DO_NOTHING_ATEXIT
  llvm_shutdown();
#endif
}

static LLVMContext &context() {
  return getGlobalContext();
}

static std::string get_input_filename(int argc, char **argv) {
  // Parse the command line, including the InputFile.
  cl::ParseCommandLineOptions(argc, argv,
                              "llvm interpreter & dynamic compiler\n");
  return InputFile;
}

static Module *load_module(LLVMContext &ctx, std::string filename) {
  SMDiagnostic err;
  Module *m = ParseIRFile(filename, err, ctx);
  if (!m) {
    err.Print(filename.c_str(), errs());
    return m;
  }

  // Load the whole bitcode file eagerly.
  std::string msg;
  if (m->MaterializeAllPermanently(&msg)) {
    errs() << "bitcode didn't read correctly.\n";
    errs() << "Reason: " << msg << "\n";
  }
  return m;
}

//===----------------------------------------------------------------------===//
// main
//
int main(int argc, char **argv, char * const *envp) {
  // Get the llvm context.
  LLVMContext &ctx = context();

  // Call llvm_shutdown() on exit.
  atexit(do_shutdown);

  // Parse the input filename from the commandline arguments.
  std::string filename = get_input_filename(argc, argv);  
  
  // Load the module from the file.
  Module *m = load_module(ctx, filename);
  if (!m) {
    exit(1);
  }

  // Try using our utility library to print the whole module.
  //print_module_call_graph(*m);

  // Construct a custom function graph.
  FunctionGraph *fg = new FunctionGraph();

  // Link the Module's functions into the graph.
  for (Module::iterator i = m->begin(), e = m->end(); i != e; ++i) {
    link_function_to_graph(fg, (Function *)i);
  }

  // Then print what we constructed. This is a very naive map.
  print_graph(fg, outs());
  
  // Delete it when we're done.
  delete fg;

  // Success.
  exit(0);
}
