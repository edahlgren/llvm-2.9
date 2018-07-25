const u64 max_size = 1000000000;

AnalysisResult *run_analysis(llvm::Module *m) {
  // Step 1.
  //
  //
  SEG *seg = new SEG(max_size);

  // Step 2.
  //
  //
  AnalysisSet *as = init_analysis_set(m);

  // Step 3.
  //
  //
  for (llvm::Module::iterator i = m->begin(); e = m->end(); i != e; i++) {
    llvm::Function *f = *i;

    if (!as->is_external(f)) {
      process_function(as, seg, f);
    }
  }  

  // Step 4.
  //
  //
  u32 last_obj = shuffle_addr_taken_nodes(as);

  // Step 5.
  //
  //
  do_hvn(as, last_obj);

  // Step 6.
  //
  //
  do_hru(as, last_obj);

  // Step 7.
  //
  //
  do_hcd(as, last_obj);

  // Step 8.
  //
  //
  factor_load_store_constraints(as);

  solve_anderson_points_to_sets(as);

  {
    HU hu(as);
    hu.run();

    merge_equivalent_nodes(as, hu.offline_graph);
  }

  // ...

  {
    T4 t4(seg);
    t4.run();

    T2 t2(seg);
    t2.run(t4.torder);

    T6 t6(seg);
    t6.run(t4.rdefs);

    T5 t5(seg);
    t5.run(t6.pnode_rdefs);
  }

  delete as;
  
  seg->compute(dfg);  
}
