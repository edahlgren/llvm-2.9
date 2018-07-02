//===- trace.cpp - Debugging implementation -------------------------------===//
//
//===----------------------------------------------------------------------===//
//
// Description ...
//
//===----------------------------------------------------------------------===//

#include "trace.h"

#include "llvm/ADT/STLExtras.h"

#include <cxxabi.h>
#include <dlfcn.h>
#include <execinfo.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void write_stacktrace(void *) {
  static void* StackTrace[256];

  int depth = backtrace(StackTrace,
                        static_cast<int>(llvm::array_lengthof(StackTrace)));

  int width = 0;
  for (int i = 0; i < depth; ++i) {
    Dl_info dlinfo;
    dladdr(StackTrace[i], &dlinfo);
    const char* name = strrchr(dlinfo.dli_fname, '/');

    int nwidth;
    if (name == NULL) nwidth = strlen(dlinfo.dli_fname);
    else              nwidth = strlen(name) - 1;

    if (nwidth > width) width = nwidth;
  }

  for (int i = 0; i < depth; ++i) {
    Dl_info dlinfo;
    dladdr(StackTrace[i], &dlinfo);

    fprintf(stderr, "%-2d", i);

    const char* name = strrchr(dlinfo.dli_fname, '/');
    if (name == NULL) fprintf(stderr, " %-*s", width, dlinfo.dli_fname);
    else              fprintf(stderr, " %-*s", width, name+1);

    fprintf(stderr, " %#0*lx",
            (int)(sizeof(void*) * 2) + 2, (unsigned long)StackTrace[i]);

    if (dlinfo.dli_sname != NULL) {
      int res;
      fputc(' ', stderr);
      char* d = abi::__cxa_demangle(dlinfo.dli_sname, NULL, NULL, &res);
      if (d == NULL) fputs(dlinfo.dli_sname, stderr);
      else           fputs(d, stderr);
      free(d);

      fprintf(stderr, " + %tu",(char*)StackTrace[i]-(char*)dlinfo.dli_saddr);
    }
    fputc('\n', stderr);
  }  
}
