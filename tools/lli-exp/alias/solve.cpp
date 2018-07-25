static void __get_nonzero_constraint_offsets(AnalysisSet *as,
                                       std::set<u32> output) {

  for (int i = 0; i < as->constraints.size(); i++) {
    const Constraint &c = as->constraints[i];
    if (c.off) {
      output.insert(c.off);
    }
  }
}

static u32 __map_constraint_offsets_to_max_object(AnalysisSet *as,
                                                  std::set<u32> &nonzero_offsets,
                                                  std::vector<std::set<u32>> &output) {
  
  u32 max_size = 0;
  for (int i = 0; i < as->last_obj() + 1; i++) {
    u32 size = as->nodes->nodes[i]->obj_sz;

    if (size < 2) {
      continue;
    }

    if (max_size < size) {
      max_size = size;
      output.resize(size);
    }

    u32 offset = size - 1;
    while(offset && !nonzero_offsets.count(offset)) {
      offset--;
    }

    if (!offset) {
      continue;
    }

    output[offset].insert(i);
  }

  return max_size;
}

static bdd __get_bdd(std::vector<bdd> set, u32 index) {
  bdd &b = set[index];
  if(b == bddfalse) {
    b = fdd_ithvar(0, index);
  }
  return b;
}

BDDSets(BDDContext *bdd_ctx, AnalysisSet *as) {
  // Step 1.
  //
  //
  this->variable_bdds.assign(bdd_ctx->config.npts, bddfalse);
  
  // Step 2.
  //
  //
  fdd_setpair(this->gep_to_pts, 1, 0);

  // Step 3.
  //
  //
  bvec pts_vec = bvec_varfdd(0), gep_vec = bvec_varfdd(1);
  u32 pts_bits = (u32) vpts.bitnum();

  // Step 4.
  //
  //
  std::set<u32> nonzero;
  __get_nonzero_constraint_offsets(as, nonzero);

  // Step 5.
  //
  //
  std::vector<std::set<u32>> max_objects;
  u32 max_size = __map_constraint_offsets_to_max_objects(as, nonzero, max_objects);

  // Step 6.
  //
  //
  this->gep_bdds.assign(max_size, bddfalse);

  // Step 7.
  //
  //
  bdd om = bddfalse;
  for (std::set<u32>::reverse_iterator i = nonzero.rbegin(),
         ie = nonzero.rend(); i != ie; i++) {
    u32 offset = *i;

    for (std::std<u32>::iterator j = max_objects[offset].begin(),
           je = max_objects[offset].end(); j != je; j++) {
      om |= __get_bdd(this->variable_bdds, *j);
    }

    bvec add = bvec_add(vpts, bvec_con(pts_bits, offset));

    bdd f = bvec_equ(gep_vec, add);

    this->gep_bdds[offset] = f & om;
    this->offset_bdds[offset] = om;    
  }

  // Step 8.
  //
  //
  for (int i = 0; i < bdd_ctx->config.npts; i++) {
    const Node *node = as->nodes->nodes[i];

    if (llvm::Function *f = llvm::dyn_cast_or_null<llvm::Function>(node->val)) {
      if (as->extinfo->is_ext(f)) {
        assert(node->obj_sz == 1);

        this->ext_func_nodes |= __get_bdd(variable_bdds, i);
        this->ext_func_nodes_set.insert(i);

        this->func_node_set.insert(i);

        continue;
      }

      if (node->obj_sz > 1) {
        this->func_node_set.insert(i);
      }
    }
  }
}

Solver(AnalysisSet *as, BDDContext *bdd_ctx) {
  this->bdds = new BDDSets(bdd_ctx, as);
  
  u32 num_pts = as->last_obj() + 1;
  for (int i = 0; i < as->constraints.size(); i++) {
    const Constraint &c = as->constraints[i];

    u32 dest = as->nodes->rep(c.dest);
    u32 src = as->nodes->rep(c.src);

    switch (c.type) {
    case ConstraintAddrOf:
      assert(src < num_pts);

      bdd src_var_bdd = bdds.get_variable_bdd(src);
      as->nodes->nodes[dest]->points_to |= src_var_bdd;

      break;
    case ConstraintCopy:
      assert(src != dest);
      as->nodes->nodes[src]->copy_to.set(dest);
      
      break;
    case ConstraintLoad:
      this->cplx_cons.push_back(c);
      as->nodes->nodes[src]->load_to.set(this->cplx_cons.size());

      break;
    case ConstraintStore:
      this->cplx_cons.push_back(c);
      as->nodes->nodes[dest]->store_from.set(this->cplx_cons.size());

      break;
    case ConstraintGEP:
      this->cplx_cons.push_back(c);
      as->nodes->nodes[src]->gep_to.set(this->cplx_cons.size());

      break;
    default:
      assert(false && "unknown constraint type");      
    }
  }

  u32 num_nodes = as->nodes.size();
  this->worklist = new Worklist(num_nodes);

  for (int i = 0; i < num_nodes; i++) {
    Node *node = as->nodes->nodes[i];
    if (!node->is_rep()) {
      continue;
    }

    node->vtime = 0;

    if (node->points_to == bddfalse) {
      continue;
    }

    if (node->copy_to.empty() &&
        node->load_to.empty() &&
        node->store_from.empty() &&
        node->gep_to.empty()) {
      continue;
    }

    this->worklist->push(i, 0);
  }

  this->worklist->swap_if_empty();
}

void Solver::solve() {
  
}

void solve_anders_constraints(AnalysisSet *as, BDDContext *bdd_ctx) {
  Solver *solver = new Solver(as, bdd_ctx);
  solver->solve();
}
