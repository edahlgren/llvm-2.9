struct BDDConfig {
  u32 npts;
};

struct BDDContext {
  BDDConfig config;
  bdd pts_domain;
  bdd gep_domain;
  
  BDDContext(BDDConfig &config) : config(config) {
    bdd_init(8000000, 1000);
    bdd_setcacheratio(8);
    bdd_setmaxincrease(1000000);
    bdd_setminfreenodes(40);
    bdd_setmaxnodenum(0);
    bdd_gbc_hook(NULL);
    bdd_disable_reorder();
    
    int domains[]= {config.npts, config.npts};  
    fdd_extdomain(domains, 2);

    pts_domain = fdd_ithset(0);
    gep_domain = fdd_ithset(1);
  }

  ~BDDContext() {
    bdd_done();
  }
};

void solve_anders_constraints(AnalysisSet *as, BDDContext *bdd_ctx);
