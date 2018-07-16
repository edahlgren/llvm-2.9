#include "seg.h"

// This represents the state of a T4 transformation on a
// graph. It is designed (at the moment) to be single-use:
// you can only call run once. Immediately after this the
// rdefs and torder are available to use.
class T4 {
public:
  SEG *graph;
  SEGIndexSet &rdefs;
  SEGIndexSet &torder;
  
  T4(SEG *graph) : graph(graph), dfs_counter(1) {}
  void run();

private:
  int dfs_counter;
  std::stack<SEGIndex> merge_stack;  
};

class T2 {
};

class T6 {
};

class T5 {
};
