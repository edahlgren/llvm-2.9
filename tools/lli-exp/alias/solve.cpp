
static std::set<u32> nonzero_constraint_offsets(AnalysisSet *as) {

  std::set<u32> output;
  for (int i = 0; i < as->constraints.size(); i++) {
    const Constraint &c = as->constraints[i];
    if (c.off) {
      output.insert(c.off);
    }
  }
  return std::move(output);
}

static u32 objects_max_size(AnalysisSet *as,
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


struct BDDSets {
  BDDContext *ctx;
  
  std::vector<bdd> variable_bdds;
  bddPair *gep_to_pts;
  std::vector<bdd> offset_bdds;
  std::vector<bdd> gep_bdds;

  bdd ext_func_nodes;
  std::set<u32> ext_func_node_set;
  std::set<u32> func_node_set;

  BDDSets(BDDContext *bdd_ctx, AnalysisSet *as);

  bdd get_variable_bdd(u32 i) {
    bdd &b = variable_bdds[i];
    if(b == bddfalse) {
      b = fdd_ithvar(0, i);
    }
    return b;
  }
};

BDDSets(BDDContext *bdd_ctx, AnalysisSet *as) {
  // Step 0.
  //
  //
  this->ctx = bdd_ctx;
  
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
  std::set<u32> nonzero = nonzero_constraint_offsets(as);

  // Step 5.
  //
  //
  std::vector<std::set<u32>> max_objects;
  u32 max_size = objects_max_size(as, nonzero, max_objects);

  // Step 6.
  //
  //
  this->gep_bdds.assign(max_size, bddfalse);
  this->offset_bdds.assign(max_size, bddfalse);
  
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

struct SolveState {
  BDDSets *bdds;
  std::vector<Constraint> cplx_cons;
  Worklist *worklist;

  u32 num_node_runs;
  u32 last_lcd_run;
  u32 current_vtime;

  u32 curr_lcd_dfs;
  llvm::DenseSet<std::pair<u32, u32> > lcd_edges;
  std::set<u32> lcd_starts;
  std::map<u32,u32> lcd_dfs_id;
  std::stack<u32> lcd_stack;
  std::set<u32> lcd_roots;

  std::set<Constraint> cons_set_scratch;
  std::set<std::pair<llvm::Function*, llvm::Instruction*>> ext_seen;
  std::hash_set<string> ext_failed;
  
  SolveState(AnalysisSet *as, BDDContext *bdd_ctx);
  ~SolveState() {
    delete bdds;
    delete worklist;
  }
};

SolveState(AnalysisSet *as, BDDSets *bdds) {
  this->bdds = bdds;
  
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

  this->curr_lcd_dfs = 1;
  this->num_node_runs = 0;
  this->last_lcd_node_run = 0;
  this->current_vtime = 1;
}

static bool add_copy_edge(AnalysisSet *as, u32 src, u32 dest) {
  if(src == NodeUnknownTarget ||
     dest == NodeUnknownTarget ||
     src == dest) {
    return false;
  }

  Node *src_node = as->nodes->nodes[src];
  if (!src_node->copy_to.test_and_set(dest)) {
    return false;
  }
  
  Node *dest_node = as->nodes->nodes[dest];
  bdd orig_pts = dest_node->points_to;
  dest_node->points_to |= src_node->points_to;
  
  return (dest_node->points_to != orig_pts);
}

static bool is_pointer(llvm::Value *v) {
  return llvm::isa<llvm::PointerType>(v->getType());
}

static void merge_value_object_points_to(AnalysisSet *as, SolveState *ss,
                                         llvm::Instruction *inst) {

  u32 value_node = as->nodes->find_value_node(inst);
  u32 value_rep_node = as->nodes->rep(value_node);

  u32 object_node = as->nodes->find_object_node(inst);
  Node *rep_node = as->nodes->nodes[value_rep_node];

  bdd orig_pts = node->points_to;
  node->points_to |= ss->bdds->get_variable_bdd(object_node);

  if (node->points_to != orig_pts) {
    ss->worklist->push(value_rep_node, node->vtime);
  }
}

static void handle_external_alloc(AnalysisSet *as, SolveState *ss,
                                  llvm::Instruction *inst) {

  if(!is_pointer(inst)) {
    return;
  }
  merge_value_object_points_to(as, ss, inst);
}

static void handle_external_realloc(AnalysisSet *as, SolveState *ss,
                                  llvm::Instruction *inst) {

  llvm::CallSite cs(inst);
  if (cs.arg_size() < 1) {
    return;
  }

  bool const_null_ptr = llvm::isa<llvm::ConstantPointerNull>(cs.getArgument(0));
  if (const_null_ptr) {
    if(!is_pointer(inst)) {
      return;
    }
    
    merge_value_object_points_to(as, ss, inst);
  }
}

static u32 eft_la_num_args(AnalysisSet *as, llvm::Function *f) {
  switch(as->ext_info->get_type(f)) {
  case EFT_L_A0:
    return 0;
  case EFT_L_A1:
    return 1;
  case EFT_L_A2:
    return 2;
  case EFT_L_A8:
    return 8;
  default:
    assert(false && "unexpected external function type");
  }
}

static void handle_external_la(AnalysisSet *as, SolveState *ss,
                               llvm::Instruction *inst, u32 num_args) {

  if (!is_pointer(inst)) {
    return;
  }

  llvm::CallSite cs(inst);
  if (cs.arg_size() <= num_args) {
    return;
  }

  u32 value_node = as->nodes->find_value_node(inst);
  u32 value_rep_node = as->nodes->rep(value_node);

  llvm::Value *src = cs.getArgument(num_args);
  if (is_pointer(src)) {
    u32 src_node = as->nodes->find_value_node(src, true);
    u32 src_rep_node = as->nodes->rep(src_node);

    if (src_rep_node &&
        add_copy_edge(as, src_rep_node, value_rep_node)) {      

      Node *node = as->nodes->nodes[value_rep_node];
      ss->worklist->push(value_rep_node, node->vtime);
    }

    return;
  }

  Node *node = as->nodes->nodes[value_rep_node];
  bdd orig_pts = node->points_to;
  node->points_to |= ss->get_variable_bdd(NodeUnknownTarget);

  if (node->points_to != orig_pts) {
    ss->worklist->push(value_rep_node, node->vtime);
  }
}

static void handle_external_instruction(AnalysisSet *as, SolveState *ss,
                                        llvm::Function *f, llvm::Instruction *inst) {

  assert(as->ext_info->is_ext(f));

  std::pair<llvm::Function *, llvm::Instruction *> arg(f, inst);
  if (ss->ext_seen.count(arg)) {
    return;
  }

  ss->ext_seen.insert(arg);

  llvm::CallSite CS(inst);
  switch(as->ext_info->get_type(f)) {
  case EFT_ALLOC:
  case EFT_NOSTRUCT_ALLOC:
    handle_external_alloc(as, ss, inst);
    break;
    
  case EFT_REALLOC:
    handle_external_realloc(as, ss, inst);
    break;
    
  case EFT_L_A0:
  case EFT_L_A1:
  case EFT_L_A2:
  case EFT_L_A8:
    u32 num_args = eft_la_num_args(as, f);
    handle_external_la(as, ss, inst, num_args);
    break;
    
  case EFT_NOOP:
  case EFT_OTHER:
    break;
    
  default:
    ss->ext_failed.insert(f->getName());
  }
}

static void handle_external_instructions(AnalysisSet *as, SolveState *ss,
                                         llvm::Function *f,
                                         std::set<llvm::Instruction *> inst) {
  for (std::set<llvm::Instruction *>::iterator it = inst->begin(),
         ie = inst->end(); it != ie; ++it){
    handle_external_instruction(as, ss, f, *it);
  }  
}

static void handle_load_generic(AnalysisSet *as, SolveState *ss,
                                bdd delta_points_to, u32 dest) {

  bool changed = false;

  std::vector<u32> *delta_points_to_vec = bdd2vec(delta_points_to);
  u32 *start = &(delta_points_to_vec->at(0));
  u32 *end = start + delta_points_to_vec->size();

  for (const u32 *i = start; i != end; i++) {
    u32 rep = as->nodes->rep(*i);
    if (add_copy_edge(rep, dest)) {
      changed = true;
    }
  }

  if (changed) {
    ss->worklist->push(dest, as->nodes->nodes[dest]->vtime);
  }
}

static void handle_load_offset(AnalysisSet *as, SolveState *ss,
                               bdd delta_points_to,
                               u32 dest, u32 offset,
                               std::set<Instruction *> *inst) {

  bdd offset_bdd = ss->bdds->offset_bdds[offset];
  bdd mask = inst ? offset_bdd | ss->bdds->ext_func_nodes : offset_bdd;

  bdd masked_delta_points_to = delta_points_to & mask;
  if (masked_delta_points_to == bddfalse) {
    return;
  }
  
  std::vector<u32> *delta_points_to_vec = bdd2vec(masked_delta_points_to);
  u32 *start = &(delta_points_to_vec->at(0));
  u32 *end = start + delta_points_to_vec->size();

  for (u32 *i = start; i != end; i++) {
    u32 rep = *i;

    if (inst && !ss->bdd->func_node_set.count(rep)) {
      continue;
    }

    if (inst && ss->bdd->ext_func_node_set.count(rep)) {
      Node *rep_node = as->nodes->nodes[rep];
      llvm::Function *f= llvm::dyn_cast_or_null<llvm::Function>(rep_node->val);

      handle_external_instructions(as, ss, f, inst);
      continue;
    }

    if (!inst && ss->bdd->func_node_set.count(rep)) {
      continue;
    }

    rep = as->nodes->rep(rep + offset);
    if (add_copy_edge(as, rep, dest)) {
      changed = true;
    }
  }

  if (changed) {
    ss->worklist->push(dest, as->nodes->nodes[dest]->vtime);
  }
}

static std::set<llvm::Instruction *> indirect_instructions(AnalysisSet *as,
                                                           u32 dest, u32 src,
                                                           Constraint &c) {

  u32 rep_dest = as->nodes->rep(c.dest);
  u32 rep_src = as->nodes->rep(c.src);

  std::set<Instruction*> *inst= 0;
  
  ConstraintInstMap::iterator i = as->indirect_constraints.find(c);
  if (i != as->indirect_constraints.end()) {
    inst = &(i->second);
    
    if (rep_dest != dest || rep_src != src) {
      std::set<llvm::Instruction *> ii(*i);
      as->indirect_constraints.erase(*inst);
      
      inst = &(as->indirect_constraints[complex]);
      inst->insert(ii.begin(), ii.end());
    }
  }

  return std::move(inst);
}

static void handle_indirect_instructions(AnalysisSet *as, SolveState *ss,
                                         bdd delta_points_to,
                                         std::set<llvm::Instruction *> inst) {

  std::vector<u32> *delta_points_to_vec = bdd2vec(delta_points_to);
  u32 *start = &(delta_points_to_vec->at(0));
  u32 *end = start + delta_points_to_vec->size();

  for (const u32 *i = start; i != end; i++) {
    u32 pindex = *i;
    Node *node = as->nodes->nodes[pindex];
    
    llvm::Function *f = llvm::dyn_cast_or_null<llvm::Function>(node->val);
    if (f && as->ext_info->is_ext(f)) {
      handle_external_instructions(as, ss, f, inst);
    }
  }
}

static bool solve_load_constraint(AnalysisSet *as, SolveState *ss,
                                  u32 index, u32 collapsed_rep,
                                  bdd delta_points_to,
                                  Constraint &c) {

  bool indirect_call = as->indirect_calls.count(index);
  std::set<llvm::Instruction *> *inst = 0;

  Constraint orig = c;
  assert(orig.src == index);

  c.src = index;
  c.dest = as->nodes->rep(orig.dest);

  if (indirect_call) {
    inst = indirect_instructions(as, orig.dest, orig.src, c);
  }

  if (ss->cons_set_scratch.count(complex)) {
    if (inst) {
      handle_indirect_instructions(as, ss, delta_points_to, inst);
    }    
    return true;
  }

  ss->cons_set_scratch.insert(c);

  if (collapsed_rep && !orig.off) {
    if (add_copy_edge(as, collapsed_rep, c.dest)) {
      ss->worklist->push(c.dest, as->nodes->nodes[c.dest]->vtime);
    }
    return true;
  }

  if (offset) {
    handle_load_offset(as, ss, delta_points_to,
                       c.dest, c.off, inst);
    return false;
  }

  handle_load_generic(as, ss, delta_points_to, c.dest);
  return false;
}

static void solve_load_constraints(AnalysisSet *as, SolveState *ss,
                                   Node *node, u32 index,
                                   u32 collapsed_rep,
                                   bdd delta_points_to) {
  ss->cons_set_scratch.clear();

  for (bitmap::iterator i = node->load_to.begin(), e = node->load_to.end();
       i != e; i++) {
    u32 ci = *i;

    if (solve_load_constraint(as, ss, index, collapsed_rep,
                              delta_points_to, ss->cplx_cons[ci])) {
      node->load_to.reset(ci);
    }
  }

  ss->cons_set_scratch.clear();
}

static void handle_store_generic(AnalysisSet *as, SolveState *ss,
                                 bdd delta_points_to, u32 src) {

  std::vector<u32> *delta_points_to_vec = bdd2vec(delta_points_to);
  u32 *start = &(delta_points_to_vec->at(0));
  u32 *end = start + delta_points_to_vec->size();

  for (const u32 *i = start; i != end; i++) {
    u32 rep = as->nodes->rep(*i);
    if (add_copy_edge(src, rep)) {
      ss->worklist->push(rep, as->nodes->nodes[rep]->vtime);
    }
  }
}

static void handle_store_offset(AnalysisSet *as, SolveState *ss,
                                bdd delta_points_to,
                                u32 dest, u32 offset,
                                std::set<Instruction *> *inst) {
  bdd offset_bdd = ss->bdds->offset_bdds[offset];
  bdd mask = inst ? offset_bdd | ss->bdds->ext_func_nodes : offset_bdd;

  bdd masked_delta_points_to = delta_points_to & mask;
  if (masked_delta_points_to == bddfalse) {
    return;
  }
  
  std::vector<u32> *delta_points_to_vec = bdd2vec(masked_delta_points_to);
  u32 *start = &(delta_points_to_vec->at(0));
  u32 *end = start + delta_points_to_vec->size();

  for (u32 *i = start; i != end; i++) {
    u32 rep = *i;

    if (inst && !ss->bdd->func_node_set.count(rep)) {
      continue;
    }

    if (inst && ss->bdd->ext_func_node_set.count(rep)) {
      Node *rep_node = as->nodes->nodes[rep];
      llvm::Function *f= llvm::dyn_cast_or_null<llvm::Function>(rep_node->val);

      handle_external_instructions(as, ss, f, inst);
      continue;
    }

    if (!inst && ss->bdd->func_node_set.count(rep)) {
      continue;
    }

    rep = as->nodes->rep(rep + offset);
    if (inst) {
      llvm::Value *v = as->nodes->nodes[rep]->val;
      if (v && !is_pointer(v)) {
        continue;
      }
    }

    if (add_copy_edge(src, rep)) {
      ss->worklist->push(rep, as->nodes->nodes[rep]->vtime);
    }
  }    
}

static bool solve_store_constraint(AnalysisSet *as, SolveState *ss,
                                  u32 index, u32 collapsed_rep,
                                  bdd delta_points_to,
                                  Constraint &c) {

  bool indirect_call = as->indirect_calls.count(index);
  std::set<llvm::Instruction *> *inst = 0;

  Constraint orig = c;
  assert(orig.dest == index);

  c.dest = index;
  c.src = as->nodes->rep(orig.src);

  if (indirect_call) {
    inst = indirect_instructions(as, orig.dest, orig.src, c);
  }

  if (ss->cons_set_scratch.count(c)) {
    if (inst) {
      handle_indirect_instructions(as, ss, delta_points_to, inst);
    }    
    return true;
  }

  ss->cons_set_scratch.insert(c);

  if (collapsed_rep && !orig.off) {
    if (add_copy_edge(as, c.src, collapsed_rep)) {
      ss->worklist->push(collapsed_rep, as->nodes->nodes[collapsed_rep]->vtime);
    }
    return true;
  }

  if (offset) {
    handle_store_offset(as, ss, delta_points_to,
                        c.src, c.off, inst);
    return false;
  }

  handle_store_generic(as, ss, delta_points_to, c.src);
  return false;  
}

static void solve_store_constraints(AnalysisSet *as, SolveState *ss,
                                    Node *node, u32 index,
                                    u32 collapsed_rep,
                                    bdd delta_points_to) {
  ss->cons_set_scratch.clear();

  for (bitmap::iterator i = node->store_from.begin(),
         e = node->store_from.end(); i != e; i++) {
    u32 ci = *i;

    if (solve_store_constraint(as, ss, index, collapsed_rep,
                               delta_points_to, ss->cplx_cons[ci])) {
      node->store_from.reset(ci);
    }
  }

  ss->cons_set_scratch.clear();
}

static bool solve_gep_constraint(AnalysisSet *as, SolveState *ss,
                                 u32 index, u32 collapsed_rep,
                                 bdd delta_points_to,
                                 Constraint &c) {

  assert(c.type == ConstraintGEP);
  assert(as->nodes->rep(c.dest) == index);

  c.src = index;
  c.dest = as->nodes->rep(c.dest);

  if (ss->cons_set_scratch.count(c)) {
    return true;
  }

  ss->cons_set_scratch.insert(c);

  Node *node = as->nodes->nodes[c.dest];
  assert(c.off < ss->bdds->gep_bdds.size() &&
         ss->bdds->gep_bdds[c.off] != bddfalse);

  bdd orig_pts = node->points_to;
  bdd prod = bdd_relprod(delta_points_to,
                         ss->bdds->gep_bdds[c.off],
                         ss->bdds->ctx->pts_domain);
  node->points_to |= bdd_replace(prod, ss->bdds->gep_to_pts);

  if (node->points_to != orig_pts) {
    ss->worklist->push(c.dest, as->node->node[c.dest]->vtime);
  }

  return false;
}

static void solve_gep_constraints(AnalysisSet *as, SolveState *ss,
                                  Node *node, u32 index,
                                  u32 collapsed_rep,
                                  bdd delta_points_to) {

  ss->cons_set_scratch.clear();

  for (bitmap::iterator i = node->gep_to.begin(),
         e = node->gep_to.end(); i != e; i++) {
    u32 ci = *i;

    if (solve_gep_constraint(as, ss, index, collapsed_rep,
                               delta_points_to, ss->cplx_cons[ci])) {
      node->gep_to.reset(ci);
    }
  }

  ss->cons_set_scratch.clear();
  
}

static void propogate_solutions(AnalysisSet *as, SolveState *ss,
                                u32 index, bdd delta_points_to) {

  Node *node = as->nodes->nodes[index];

  std::set<u32> seen;
  for (bitmap::iterator i = node->copy_to.begin(), e = node->copy_to.end();
       i != e; i++) {
    u32 orig_dest = *i;
    u32 dest = as->nodes->rep(orig_dest);

    if (dest == index || seen.count(dest)) {
      continue;
    }

    seen.insert(dest);
    
    Node *dest_node = as->nodes->nodes[dest];
    std::pair<u32, u32> edge(index, dest);

    if (!ss->lcd_edges.count(e) &&
        node->points_to != bddfalse) {
      if (node->points_to == dest_node->points_to) {
        ss->lcd_edges.insert(edge);
        ss->lcd_start.insert(index);
      }      
    }

    bdd orig_pts = dest_node->points_to;
    dest_node->points_to |= delta_points_to;
    if (dest_node->points_to != orig_pts) {
      ss->worklist->push(dest, as->nodes->nodes[dest]->vtime);
    }
  }
}

static void do_solve_at(AnalysisSet *as, SolveState *ss, u32 index) {
  ss->num_node_runs++;

  Node *node = as->nodes->nodes[index];
  node->vtime = ss->curr_vtime++;

  bdd delta_points_to = node->points_to - node->prev_points_to;
  if (delta_points_to == bddfalse) {
    return;
  }

  std::vector<u32> *delta_points_to_vec = bdd2vec(delta_points_to);
  u32 *start = &(delta_points_to_vec->at(0));
  u32 *end = start + delta_points_to_vec->size();

  node->prev_points_to = node->points_to;

  u32 collapsed_rep = 0;
  llvm::DenseMap<u32, u32>::iterator i = as->deref_to_var_nodes.find(index);
  if (i != as->deref_to_var_nodes.end()) {
    collapsed_rep = as->nodes->rep(i->second);

    for (const u32 *ip = start; ip != end; ip++) {
      u32 rep = as->nodes->rep(*ip);
      if (rep != collapsed_rep && rep != NodeUnknownTarget) {
        collapsed_rep = as->nodes->merge_nodes(collapsed_rep, rep,
                                               as->deref_to_var_nodes);
        changed = true;
      }
    }
  }

  // What if it's still zero though?
  Node *collapsed_node = as->nodes->nodes[collapsed_rep];
  ss->worklist->push(collapsed_rep, collapsed_node->vtime);

  node = as->nodes->nodes[index];
  assert(node->is_rep() || as->nodes->rep(index) == collapsed_rep);

  solve_load_constraints(as, ss, node, index,
                         collapse_rep, delta_points_to);

  solve_store_constraints(as, ss, node, index,
                          collapse_rep, delta_points_to);

  solve_gep_constraints(as, ss, node, index, delta_points_to);
  
  propogate_solutions();
}

static void do_dfs_at(AnalysisSet *as, SolveState *ss, u32 index) {
  assert(i != NodeUnknownTarget);
  
  Node *node = as->nodes->nodes[index];
  assert(node->is_rep());

  u32 orig_dfs_num = ss->curr_lcd_dfs++;
  ss->lcd_dfs_id[index] = orig_dfs_num;

  bitmap del_copy, add_copy;
  for (bitmap::iterator i = node->copy_to.begin(); e = node->copy_to.end();
       i != e; i++) {
    u32 dest = *i;
    assert(dest != index);

    u32 rep_dest = as->nodes->rep(dest);
    if (add_copy.test(rep_dest) || rep_dest == index) {
      del_copy.set(dest);
      continue;
    }

    if (!ss->lcd_roots.count(rep_dest)) {
      if (!ss->lcd_dfs_id.count(rep_dest)) {
        // Recurse.
        do_dfs_at(as, ss, rep_dest);
        rep_dest = as->nodes->rep(rep_dest);
      }

      if (ss->lcd_dfs_id[rep_dest] < ss->lcd_dfs_id[index]) {
        ss->lcd_dfs_id[index] = ss->lcd_dfs_id[rep_dest];
      }
    }

    if (rep_dest != dest) {
      del_copy.set(dest);

      if (add_copy.test(rep_dest)) {
        continue;
      }

      assert(rep_dest != index);
      add_copy.set(rep_dest);
    }
  }

  assert(node->is_rep());
  node->copy_to.insersect_with_complement(del_copy);
  node->copy_to |= add_copy;

  if (ss->lcd_dfs_id[index] != orig_dfs_num) {
    ss->lcd_stack.push(index);
    return;
  }

  NodeMap dummy;  
  bool changed = false;
  while (!ss->lcd_stack.empty()) {
    u32 top = ss->lcd_stack.top();

    if (ss->lcd_dfs_id[top] < orig_dfs_num) {
      break;
    }

    ss->lcd_stack.pop();
    u32 rep_top = as->nodes->rep(top);
    if (rep_top != index) {
      index = as->nodes->merge_nodes(index, rep_top, &dummy);
    }

    changed = true;
  }

  ss->lcd_roots.insert(index);

  if (changed) {
    ss->worklist->push(index, as->nodes->nodes[index]->vtime);
  }
}

static void do_lcd(AnalysisSet *as, SolveState *ss) {
  ss->last_lcd_run = ss->num_node_runs;
  ss->curr_lcd_dfs = 1;
  ss->lcd_dfs_id.clear();
  ss->lcd_roots.clear();

  for (std::set<u32>::const_iterator i = ss->lcd_starts.begin();
       e = ss->lcd_starts.end(); i != e; i++) {
    u32 n = *i;

    Node *node = as->nodes->nodes[i];
    if (node->is_rep() && !ss->lcd_dfs_id.count(n)) {
      do_dfs_at(as, ss, n);
    }
  }

  assert(ss->lcd_stack.empty());
  ss->lcd_starts.clear();
}

const u32 LCD_SIZE = 600, LCD_PERIOD = 999999999;

static bool should_run_lcd(SolveState *ss) {
  return ss->lcd_starts.size() >= LCD_SIZE ||
    ss->num_node_runs - ss->last_lcd_run >= LCD_PERIOD;
}

void solve_anders_constraints(AnalysisSet *as, BDDContext *bdd_ctx) {
  SolveState *ss = new SolveState(as, bdd_ctx);
  
  while (true) {
    if (ss->worklist->swap_if_empty()) {
      if (ss->worklist->empty()) {
        break;
      }
    }

    u32 p;
    u32 i = ss->worklist->pop(&p);

    Node *node = as->nodes->nodes[i];
    if (!node->is_rep()) {
      continue;
    }

    if (ss->worklist->prio() != WLP_ID &&
        p < node->vtime) {
      continue;
    }

    do_solve_at(as, ss, i);

    if (should_run_lcd(ss)) {
      do_lcd(as, ss);
    }
  }

  delete ss;

  return new AndersSolution(as, bdds);
}
