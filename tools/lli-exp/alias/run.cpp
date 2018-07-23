const u64 max_size = 1000000000;

AnalysisResult *run_analysis(llvm::Module *m) {
  SEG *seg = new SEG(max_size);

  AnalysisSet *as = init_analysis_set(m);

  for (llvm::Module::iterator i = m->begin(); e = m->end(); i != e; i++) {
    llvm::Function *f = *i;

    if (!as->is_external(f)) {
      process_function(as, seg, f);
    }
  }  

  u32 last_obj = shuffle_addr_taken_nodes(as);

  do_hvn(as, last_obj);

  

  run_factor_load_store(as);
  
  {
    FactorLoadStore fls(as);
    rls.run();
  }

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
