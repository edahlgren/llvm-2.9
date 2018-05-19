//===- lli-exp.cpp - Experimental lli tool -------------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#include "CallGraphUtils.h"
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

//===----------------------------------------------------------------------===//
// main Driver function
//
int main(int argc, char **argv, char * const *envp) {
  // Get the context.
  LLVMContext &Context = getGlobalContext();
  atexit(do_shutdown);  // Call llvm_shutdown() on exit.

  // Parse the command line, including the InputFile.
  cl::ParseCommandLineOptions(argc, argv,
                              "llvm interpreter & dynamic compiler\n");

  // Load the bitcode.
  SMDiagnostic Err;
  Module *Mod = ParseIRFile(InputFile, Err, Context);
  if (!Mod) {
    Err.Print(argv[0], errs());
    return 1;
  }

  // Load the whole bitcode file eagerly.
  std::string ErrorMsg;
  if (Mod->MaterializeAllPermanently(&ErrorMsg)) {
    errs() << argv[0] << ": bitcode didn't read correctly.\n";
    errs() << "Reason: " << ErrorMsg << "\n";
    exit(1);
  }

  // Try using our utility library.
  PrintCallGraph(*Mod);

  // Success.
  exit(0);
}
