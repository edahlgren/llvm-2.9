bool points_to_set_is_null(AnalysisSet *as, BDDSets *bdds,
                        u32 index, u32 offset) {
  
  assert(index && index < as->nodes->nodes.size());
  
  bdd pts = as->nodes->nodes[as->nodes->rep(index)]->points_to;
  
  if (!offset) {
    return pts == bddfalse;
  }

  assert(offset < bdds->gep_bdds.size() &&
         bdds->gep_bdds[offset] != bddfalse);

  bdd prod = bdd_relprod(pts, bdds->gep_bdds[offset], bdds->ctx->pts_domain);
  bdd gep = bdd_replace(prod, bdds->gep_to_pts);
  return gep == bddfalse;
}

std::vector<u32> *points_to_set(AnalysisSet *set, BDDSets *bdds,
                      u32 index, u32 offset) {

  assert(index && index < as->nodes->nodes.size());

  u32 rep = as->nodes->rep(index);
  if (!offset) {
    return bdd2vec(as->nodes->nodes[rep]->points_to);
  }

  assert(offset < bdds->gep_bdds.size());
  assert(bdds->gep_bdds[offset] != bddfalse);

  bdd prod = bdd_relprod(as->nodes->nodes[rep]->points_to,
                         bdds->gep_bdds[offset],
                         bdds->pts_domain);
  bdd gep = bdd_replace(prod, bdd->gep_to_pts);
  return bdd2vec(gep);
}

std::vector<u32> *points_to_set(llvm::Value *v, u32 offset) {
  return points_to_set(as->nodes->find_value_node(v), offset);
}
