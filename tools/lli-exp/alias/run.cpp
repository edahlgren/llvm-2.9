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

  // Step 9.
  //
  // Solve for anderson points-to sets.
  //
  //   Anders::pts_init();
  //   Anders::solve_init();
  //   Anders::solve();

  // Step 10.
  //
  // Optimize the constraints again.
  //
  //   SFS::cons_opt();
  //     hu();  
  //   SFS::cons_opt_wrap();

  // Step 11.
  //
  // Add interprocedural edges.
  //
  //   SFS::icfg_inter_edges();

  // Step 12.
  //
  // Process constraints from indirect calls.
  //
  //   process_idr_cons();

  // Step 13.
  //  
  // Make the data flow graph and supporting
  // structures.
  //
  //   SFS::sfs_prep();

  // Step 14.
  //
  // Destroy the AnalysisSet.

  // Step 15.
  //
  // Reduce the SEG.  
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

  // Step 16.
  //
  // Remove redundant nodes and edges in the SEG.
  //
  //   clean_G()

  // Step 17.
  //
  // Partition variables into equivalence classes.
  //
  //   SFS::partition_vars()

  // Step 18.
  //
  // Release all remaining memory structures not
  // needed by the solver.

  // Step 19.
  //
  // Reduce the SEG into the data flow grph.
  //
  //   SFS::compute_seg()

  // Step 20.
  //
  // Solve the DFG
  //
  //   SFS::solve();

  // Step 21.
  //
  // Write the points-to sets to disk so that they can be
  // inspected, visualized, and queried.
}
