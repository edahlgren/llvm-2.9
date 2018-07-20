void OfflineGraph::merge(u32 a, u32 b) {
  assert(n1 && n1 < off_nodes.size() && "invalid node 1");
  assert(n2 && n2 < off_nodes.size() && "invalid node 2");
  assert(n1 != n2 && "trying to merge a node with itself");
  OffNode *N1= &off_nodes[n1], *N2= &off_nodes[n2];
  assert(N1->dfs_id && N2->dfs_id && "trying to merge unvisited nodes");
  u32 rank1= N1->rep, rank2= N2->rep;
  assert(rank1 >= node_rank_min && rank2 >= node_rank_min &&
      "only rep nodes may be merged");
  //Make n1 the parent.
  if(rank1 < rank2){
    swap(n1, n2);
    swap(N1, N2);
  }else if(rank1 == rank2){
    ++N1->rep;
  }
  N2->rep= n1;
  DEBUG(fprintf(stderr, "    merge %u <= %u\n", n1, n2));

  //Move n2's edges and labels into n1. In HVN mode, if both nodes were
  //  pre-labeled, n1 may have >1 label, but hvn_label() will be called
  //  on n1 as soon as the SCC is collapsed, so it will have only 1 label
  //  after hvn_dfs() returns.
  N1->edges |= N2->edges;
  N1->impl_edges |= N2->impl_edges;
  N1->ptr_eq |= N2->ptr_eq;
  N2->edges.clear();
  N2->impl_edges.clear();
  N2->ptr_eq.clear();
  //The entire SCC should be indirect if any node in it is.
  N1->indirect |= N2->indirect;
  //If either node was pre-labeled, the merged node should get the same label.
  N1->ptr_eq |= N2->ptr_eq;
  return n1;
}

static void add_constraint_edges(HVN *hvn) {
  u32 num_copy = 0, num_load = 0, num_store = 0;
  u32 num_impl_addr = 0, n_impl_copy = 0;

  AnalysisSet *as = hvn->as;
  OfflineGraph *og = hvn->offline_graph;
  
  for (int i = 0; i < as->constraints.size(); i++) {
    const Constraint &c = as->constraints[i];

    assert(as->nodes[c.dest]->is_rep());
    assert(as->nodes[c.src]->is_rep());

    u32 dest = og->offsets[c.dest];
    u32 src = og->offsets[c.src];

    if (!dest) {
      assert(as->nodes[c.dest]->obj_sz);
      continue;      
    }

    assert(src || nodes[c.src]->obj_sz);

    switch (c.type) {
    case ConstraintAddrOf:
      OfflineNode &dest_node = og->nodes[dest];
      dest_node.ptr_eq.set(c.src);

      if (src) {
        u32 dest_ref = og->ref(dest);
        OfflineNode &dest_ref_node = graph->nodes[dest_ref];
        dest_ref_node.impl_edges.set(src);
      }

      break;
    case ConstraintCopy:
      if (!src) {
        OfflineNode &dest_node = og->nodes[dest];
        dest_node.indirect = true;

        break;
      }

      OfflineNode &dest_node = og->nodes[dest];
      dest_node.edges.set(object_src_node);
      num_copy++;

      u32 dest_ref = og->ref(dest);
      u32 src_ref = og->ref(src);
      
      OfflineNode &dest_ref_node = og->nodes[dest_ref];
      dest_ref_node.impl_edges.set(src_ref);
      num_impl_copy++;
      
      break;
    case ConstraintLoad:
      if (c.off) {
        OfflineNode &dest_node = og->nodes[dest];
        dest_node.indirect = true;

        break;
      }

      assert(src);

      u32 src_ref = og->ref(src);
      OfflineNode &dest_node = og->nodes[dest];
      dest_nodes.edges.set(src_ref);
      num_load++;

      break;
    case ConstraintStore:
      if (!c.off) {
        assert(src);

        u32 dest_ref = og->ref(dest);
        OfflineNode &dest_ref_node = og->nodes[dest_ref];
        dest_ref_node.edges.set(src);
        num_store++;
      }

      break;
    case ConstraintGEP:
      std::pair<u32, u32> r(c.src, c.off);
      std::hash_map<std::pair<u32, u32, u32>::const_iterator iter =
                    og->gep_labels.find(r);

      u32 pe;
      if (iter != og->gep_label.end()) {
        pe = iter->second;
      } else {
        og->gep_labels[r] = pe = og->next_ptr_eq++;
      }

      OfflineNode &dest_node = og->nodes[dest];
      dest_node.ptr_eq.set(pe);

      break;
    default:
      assert(false && "unknown constraint type");
    }
  }
}

static void label_node_at(HVN *hvn, u32 index) {
  OffNode *N= &off_nodes[n];
  bitmap &pe= N->ptr_eq;
  assert(N->is_rep() && N->scc_root);
  //All indirect nodes get new labels.
  if(N->indirect){
    //Remove pre-labeling, in case a direct pre-labeled node was
    //  merged with an indirect one.
    pe.clear();
    pe.set(next_ptr_eq++);
    return;
  }
  //Collect all incoming labels into the current node.
  for(bitmap::iterator it= N->edges.begin(), ie= N->edges.end();
      it != ie; ++it){
    u32 n2= get_off_rep(*it);
    if(n2 == n){
      continue;
    }
    bitmap &pe2= off_nodes[n2].ptr_eq;
    assert(!pe2.empty() && "unlabeled neighbor");
    //Ignore non-ptr neighbors.
    if(!pe2.test(0)){
      pe |= pe2;
    }
  }
  //If a direct node has no incoming labels, it's a non-pointer.
  if(pe.empty()){
    pe.set(0);
  //If there was >1 incoming label, replace the set by its ID.
  }else if(!pe.single_bit_p()){
    hash_map<bitmap, u32>::const_iterator i_l2p= lbl2pe.find(pe);
    if(i_l2p == lbl2pe.end()){
      lbl2pe[pe]= next_ptr_eq;
      pe.clear();
      pe.set(next_ptr_eq++);
    }else{
      pe.clear();
      pe.set(i_l2p->second);
    }
  }
  //If there was only 1, keep it.
  assert(N->ptr_eq.single_bit_p());  
}

void check_edge(HVN *hvn, u32 index, u32 dest);

static void do_dfs_at(HVN *hvn, u32 index) {
  assert(index);
  
  AnalysisSet *as = hvn->as;
  OfflineGraph *og = hvn->offline_graph;

  OfflineNode *node = &og[index];
  assert(!node->scc_root && node->is_rep());

  u32 orig_dfs = hvn->curr_dfs++;
  node->dfs_num = dfs;

  // Look for SCCs.
  for (bitmap::iterator i = node->edges.begin(), e = node->edges.end();
       i != e; i++) {
    // This recurses into do_dfs.
    check_edge(hvn, index, *i);
  }
  for (bitmap::iterator i = node->impl_edges.begin(), e = node->impl_edges.end();
       i != e; i++) {
    check_edge(hvn, index, *i);
  }
  assert(node->is_rep());

  // Check if this node is the root of an scc.
  if (node->dfs_num != orig_dfs) {
    // Nope.
    hvn->dfs_stack.push(index);
    return;
  }

  // Find the root.
  while (!hvn->dfs_stack.empty()) {
    u32 top = hvn->dfs_stack.top();

    if (og->nodes[top].dfs_num < orig_dfs) {
      // Keep anything visited before on the stack.
      break;
    }

    hvn->dfs_stack.pop();
    index = og->merge(index, top);
  }

  assert(index);
  node = &og->nodes[index];

  // Label it as such.
  assert(node->is_rep());
  node->scc_root = true;
  
  label_node_at(hvn, index);    
}

void check_edge(HVN *hvn, u32 index, u32 dest) {
  OffNode *N= &off_nodes[n];
  assert(N->is_rep());
  //dest comes from a bitmap entry, so it may be out of date.
  u32 n2= get_off_rep(dest);
  const OffNode *N2= &off_nodes[n2];
  //Skip this neighbor if it was merged into N or is a collapsed SCC.
  if(n2 == n || N2->scc_root){
    return;
  }
  //If it's unvisited, continue the DFS.
  if(!N2->dfs_id){
    hvn_dfs(n2, do_union);
  }
  //Set our dfs_id to the minimum reachable ID.
  if(N2->dfs_id < N->dfs_id){
    N->dfs_id= N2->dfs_id;
  }
  assert(N->is_rep());
}

static void merge_all_equal_pointers(HVN *hvn) {
  u32 nn= nodes.size();
  //The first node (of the main graph) with the given ptr_eq.
  hash_map<bitmap, u32> pe2node;
  FORN(i, nn){
    u32 on= main2off[i];
    //If this node has no offline version, it's not pointer-equivalent.
    if(!on){
      continue;
    }
    bitmap &pe= off_nodes[get_off_rep(on)].ptr_eq;
    assert(!pe.empty());
    //Non-ptr nodes should be deleted from the constraints.
    if(pe.test(0)){
      assert(pe.single_bit_p());
      nodes[i]->nonptr= 1;
      continue;
    }
    //Anything previously marked as non-ptr cannot have another label.
    assert(!nodes[i]->nonptr);
    hash_map<bitmap, u32>::iterator i_p2n= pe2node.find(pe);
    if(i_p2n == pe2node.end()){
      //This PE was not seen yet, so (i) is its first node.
      assert(nodes[i]->is_rep());
      pe2node[pe]= i;
    }else{
      u32 en= i_p2n->second;
      //Merge (i) into the node representing (pe).
      INC_STAT(hvn_merge);
      i_p2n->second= merge_nodes(en, i);
    }
  }

  vector<Constraint> old_cons;
  old_cons.swap(constraints);
  DenseSet<Constraint> cons_seen;
  FORN(i, old_cons.size()){
    Constraint &C= old_cons[i];
    //Ignore this constraint if either side is a non-ptr.
    if(nodes[C.dest]->nonptr || nodes[C.src]->nonptr)
      continue;
    C.dest= get_node_rep(C.dest);
    //Don't replace the source of addr_of by the rep: the merging
    //  done in HVN/HCD is based only on pointer equivalence,
    //  so non-reps may still be pointed to.
    if(C.type != addr_of_cons)
      C.src= get_node_rep(C.src);
    //Ignore (copy X X) and duplicates.
    if((C.type != copy_cons || C.dest != C.src) && !cons_seen.count(C)){
      cons_seen.insert(C);
      constraints.push_back(C);
    }
  }

  //Also rewrite icall_cons to refer to the rep nodes.
  vector<pair<Constraint, set<Instruction*> > > old_icall_cons;
  for(DenseMap<Constraint, set<Instruction*> >::iterator
      it= icall_cons.begin(), ie= icall_cons.end(); it != ie; ++it){
    old_icall_cons.push_back(*it);
  }
  icall_cons.clear();
  FORN(i, old_icall_cons.size()){
    Constraint &C= old_icall_cons[i].first;
    if(nodes[C.dest]->nonptr || nodes[C.src]->nonptr)
      continue;
    C.dest= get_node_rep(C.dest);
    C.src= get_node_rep(C.src);
    const set<Instruction*> &I= old_icall_cons[i].second;
    icall_cons[C].insert(I.begin(), I.end());
  }
}

OfflineGraph *HVN::make_graph(AnalysisSet *as) {
  u32 nn= nodes.size();
  assert(last_obj_node && "clump_addr_taken is required");
  main2off.assign(nn, 0);
  //Start the graph with only the null node (onn - size of off_nodes).
  u32 onn= 1;
  //Add the AFP nodes first (assuming clump_addr_taken has already moved
  //  them in front of all the value nodes).
  firstAFP= 1;
  //Look for function object nodes.
  for(u32 i= first_var_node; i <= last_obj_node; ){
    const Node *N= nodes[i];
    u32 sz= N->obj_sz;
    assert(sz && "object nodes are not clumped");
    assert(N->get_val() && "obj node has no value");
    Function *F= dyn_cast<Function>(N->get_val());
    //Skip this object if it's not a function.
    if(!F){
      i += sz;
      continue;
    }
    //Go through the retval and all the parameters of F.
    for(u32 j= func_node_off_ret; j < sz; ++j){
      //A rep parameter node gets a new offline node,
      //  but non-rep parm. are not included in any constraints.
      if(nodes[i+j]->is_rep()){
        main2off[i+j]= onn++;
      }
    }
    //Set (i) to the node after the current object.
    i += sz;
  }
  firstVAL= onn;
  nAFP= firstVAL - firstAFP;

  //Now add the value nodes, including p_i2p and temporary (no-value) nodes.
  main2off[p_i2p]= onn++;
  for(u32 i= last_obj_node+1; i < nn; ++i){
    const Node *N= nodes[i];
    assert(!N->obj_sz && "unexpected obj node");
    if(N->is_rep()){
      main2off[i]= onn++;
    }
  }
  firstREF= onn;
  nVAL= firstREF-firstVAL;
  nREF= nVAL+nAFP;
  //Create all the offline nodes, including REF.
  //AFP & VAR start out direct; then indirect REFs are added.
  off_nodes.assign(onn, OffNode());
  off_nodes.insert(off_nodes.end(), nREF, OffNode(1));
  //Mark AFPs indirect.
  for(u32 i= firstAFP; i < firstVAL; ++i){
    off_nodes[i].indirect= 1;
  }
}

void HVN::run() {
  add_constraint_edges(this);

  for (u32 i = this->graph->firstAFP; i < this->graph->firstREF; i++) {
    if (!this->graph->nodes[i].dfs_num) {
      do_dfs(this, i);
    }
  }

  assert(this->dfs_stack.empty());
  merge_all_equal_pointers(this);
}
