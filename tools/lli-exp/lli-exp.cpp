//===- lli-exp.cpp - Experimental lli tool -------------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#include "cfg.h"
#include "trace.h"
#include "write.h"

#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/Type.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/IRReader.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Process.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Signals.h"

#include <cerrno>
#include <iostream>

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

  cl::opt<bool>
  PrintGraphML("graphml", cl::desc("Print in GraphML format"), cl::init(true));

  cl::opt<bool>
  PrintControlFlow("flow", cl::desc("Print the control flow graph of each function"), cl::init(false));

  cl::opt<bool>
  PrintDominance("dom", cl::desc("Print the dominance tree of each function"), cl::init(false));
  cl::opt<bool>
  PrintOldDominance("domold", cl::desc("Print the old dominance tree of each function"), cl::init(false));
}

static LLVMContext &context() {
  return getGlobalContext();
}

static std::string parse_args(int argc, char **argv) {
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

static void write_stacktrace_on_crash() {
  // See trace.h and trace.cpp.
  sys::AddSignalHandler(write_stacktrace, 0);
}

//===----------------------------------------------------------------------===//
// main
//
int main(int argc, char **argv, char * const *envp) {
  // Write a stacktrace to stderr if an assertion fails, if
  // we generate a segmentation fault, or for any other signal
  // related death. This is really helpful for debugging.
  //
  // Note that there is another function similar to this one
  // called sys::PrintStackTraceOnErrorSignal but we're not
  // using it for several reasons:
  //
  // 1. Enabling it (e.g. HAVE_BACKTRACE) is a royal pain in the ass.
  // 2. We want to go beyond it's semantics and also contextualize
  //    the file and line numbers of each entry.
  write_stacktrace_on_crash();

  // Get the llvm context.
  LLVMContext &ctx = context();

  // Call llvm_shutdown on exit.
  atexit(llvm_shutdown);

  // Parse the input filename from the commandline arguments.
  std::string filename = parse_args(argc, argv);  
  
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
    if (PrintControlFlow) {
      write_function_control_flow((Function *)i, outs());
    }
    if (PrintDominance) {
      write_function_dominator_tree((Function *)i, outs());
    } else if (PrintOldDominance) {
      write_function_dominator_tree_old((Function *)i, outs());
    }
  }

  if (PrintGraphML) {
    // Print the graph in a GraphML format. This is parseable by
    // many other tools.
    write_function_graph(fg, std::cout);
  }
    
  // Delete the graph when we're done.
  delete fg;

  // Success.
  exit(0);
}
