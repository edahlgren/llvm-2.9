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

// This represents the state of a T2 transformation on a
// graph. It is designed like the T4 transformation to only
// be run once.
class T2 {
public:
  SEG *graph;
  
  T2(SEG *graph) : graph(graph) {}

  void run(SEGIndexSet &torder);
};

class T6 {
  SEG *graph;  
  SEGIndexSet &pnode_neg;

  T6(SEG *graph) : graph(graph) {}

  void run(SEGIndexSet &rdefs);
};

class T5 {
  SEG *graph;  
  SEGIndexSet &new_reps;

  T5(SEG *graph) : graph(graph) {}

  void run(SEGIndexSet &pnode_neg);
};
