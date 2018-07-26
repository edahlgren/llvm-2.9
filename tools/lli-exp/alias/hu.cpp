static void do_dfs_at(SimpleOfflineGraph *og
                      std::stack<u32> *dfs_stack,
                      u32 index) {

  OfflineNode &node = og->nodes[index];

  u32 orig_dfs_num = og->dfs_num++;
  node.dfs_num = orig_dfs_num;

  for (bitmap::iterator i = node.edges.start(), e = node.edges.end();
       i != e; i++) {
    u32 p = og->rep(*i);
    OfflineNode &point = og->nodes[p];

    if (p == index || point.del) {
      continue;
    }

    if (!point.dfs_num) {
      do_dfs_at(og, dfs_stack, p);
    }

    if (point.dfs_num < node.dfs_num) {
      node.dfs_num = point.dfs_num;
    }
  }

  if (orig_dfs_num != node.dfs_num) {
    dfs_stack.push(index);
    return;
  }
  
  while (!dfs_stack.empty()) {
    u32 top = dfs_stack.top();
      
    if (og->nodes[top].dfs_num < orig_dfs_num) {
      break;
    }

    index = og->merge(index, top);
    dfs_stack.pop();
  }

  node = og->nodes[index];
  node.del = true;
  
  if (node.indirect) {
    node.labels.set(og->next_label++);
  }

  for (bitmap::iterator i = node.edges.start(), e = node.edges.end();
       i != e; i++) {
    u32 p = og->rep(*i);

    if (p == index) {
      continue;
    }

    OfflineNode &point = og->node[p];
    assert(!point.labels.empty());

    if (!point.labels.test(0)) {
      node.labels |= point.labels;
    }
  }

  if (node.labels.empty()) {
    node.labels.set(0);
  }
}

static void merge_equal_pointers(AnalysisSet *as, SimpleOfflineGraph *og) {
  std::hash_map<bitmap, u32> eq;

  NodeMap dummy;
  for (int i = 1; i < og->nodes.size(); i++) {
    Node *node = as->nodes->nodes[i];
    assert(node->is_rep());

    OfflineNode &offline_node = og->nodes[og->rep(i)];

    if (offline_node.labels.empty() ||
        offline_node.labels.test(0)) {
      node->nonptr = true;
      continue;
    }

    bitmap::iterator j = eq.find(offline_node.labels);

    if (j != eq.end()) {
      as->nodes->merge(i, as->nodes->rep(j->second), &dummy);
      continue;
    }

    eq[offline_node.labels] = i;
  }
}

void do_hu(AnalysisSet *as, u32 last_obj) {
  SimpleOfflineGraph offline(as->nodes->nodes, last_obj);

  std::stack<u32> dfs_stack;
  for (u32 i = 1; i < offline.size(); i++) {
    if (!offline.nodes[i].dfs_num) {
      do_dfs_at(&offline, &dfs_stack, i);
    }
  }
  assert(dfs_stack.empty());

  merge_equal_pointers(as, &offline);
}
