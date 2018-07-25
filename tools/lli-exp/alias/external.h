enum ExternalFunctionType {
  EFT_NOOP= 0,      //no effect on pointers
  EFT_ALLOC,        //returns a ptr to a newly allocated object
  EFT_REALLOC,      //like L_A0 if arg0 is a non-null ptr, else ALLOC
  EFT_NOSTRUCT_ALLOC, //like ALLOC but only allocates non-struct data
  EFT_STAT,         //retval points to an unknown static var X
  EFT_STAT2,        //ret -> X -> Y (X, Y - external static vars)
  EFT_L_A0,         //copies arg0, arg1, or arg2 into LHS
  EFT_L_A1,
  EFT_L_A2,
  EFT_L_A8,
  EFT_L_A0__A0R_A1R,  //copies the data that arg1 points to into the location
                      //  arg0 points to; note that several fields may be
                      //  copied at once if both point to structs.
                      //  Returns arg0.
  EFT_A1R_A0R,      //copies *arg0 into *arg1, with non-ptr return
  EFT_A3R_A1R_NS,   //copies *arg1 into *arg3 (non-struct copy only)
  EFT_A1R_A0,       //stores arg0 into *arg1
  EFT_A2R_A1,       //stores arg1 into *arg2
  EFT_A4R_A1,       //stores arg1 into *arg4
  EFT_L_A0__A2R_A0, //stores arg0 into *arg2 and returns it
  EFT_A0R_NEW,      //stores a pointer to an allocated object in *arg0
  EFT_A1R_NEW,      //as above, into *arg1, etc.
  EFT_A2R_NEW,
  EFT_A4R_NEW,
  EFT_A11R_NEW,
  EFT_OTHER         //not found in the list
};

class ExtInfo {
  // Each function name is mapped to its extf_t.
  llvm::StringMap<ExternalFunctionType> info;

  // Cache of is_ext results for all functions.
  std::hash_map<const llvm::Function *, bool> cache;

  ExtInfo() {
    std::set<ExternalFunctionType> seen;
    ExternalFunctionType prev = EFT_NOOP;
    seen.insert(prev);
    for (const ei_pair *p = external_function_database; p->n; ++p) {
      if (p->t != prev) {
        assert(!seen.count(p->t));
        seen.insert(p->t);
        prev = p->t;
      }
      
      assert(!info.count(p->n));
      info[p->n] = p->t;
    }
    
    cache.clear();
  }

  extf_t get_type(const Function *f) const {
    llvm::StringMap<ExternalFunctionType>::const_iterator i =
      info.find(f->getNameStart());
    if (i == info.end())
      return EFT_OTHER;
    else
      return i->second;
  }

  bool has_static(const Function *F) const;
  bool has_static2(const Function *F) const;
  bool is_alloc(const Function *F) const;
  bool no_struct_alloc(const Function *F) const;
  bool is_noop(const Function *F) const;
  bool is_ext(const Function *F) {
    //Check the cache first; everything below is slower.
    std::hash_map<const Function *, bool>::iterator i = this->cache.find(f);
    if (i != this->cache.end()) {
      return i->second;
    }
  
    bool res;
    if(f->isDeclaration() || f->isIntrinsic()) {
      res = true;
    } else {
      ExternalFunctionType t = get_type(f);
      res = t == EFT_ALLOC
        || t == EFT_REALLOC
        || t == EFT_NOSTRUCT_ALLOC
        || t == EFT_NOOP;
    }
    this->cache[f] = res;
    return res;
  }
};

struct ei_pair {
  const char *n;
  extf_t t;
};

static const ei_pair ei_pairs [] = {
  {"accept", EFT_NOOP},
  {"access", EFT_NOOP},
  {"asprintf", EFT_NOOP},
  {"atexit", EFT_NOOP},
  {"atof", EFT_NOOP},
  {"atoi", EFT_NOOP},
  {"atol", EFT_NOOP},
  {"bind", EFT_NOOP},
  {"chdir", EFT_NOOP},
  {"chmod", EFT_NOOP},
  {"chown", EFT_NOOP},
  {"chroot", EFT_NOOP},
  {"closedir", EFT_NOOP},
  {"compress2", EFT_NOOP},
  {"confstr", EFT_NOOP},
  {"connect", EFT_NOOP},
  {"crc32", EFT_NOOP},
  {"creat", EFT_NOOP},
  {"creat64", EFT_NOOP},
  {"dladdr", EFT_NOOP},
  {"dlclose", EFT_NOOP},
  {"execl", EFT_NOOP},
  {"execle", EFT_NOOP},
  {"execlp", EFT_NOOP},
  {"execv", EFT_NOOP},
  {"execve", EFT_NOOP},
  {"execvp", EFT_NOOP},
  {"fclose", EFT_NOOP},
  {"feof", EFT_NOOP},
  {"ferror", EFT_NOOP},
  {"fflush", EFT_NOOP},
  {"fgetc", EFT_NOOP},
  {"fgetpos", EFT_NOOP},
  {"fileno", EFT_NOOP},
  {"flockfile", EFT_NOOP},
  {"fnmatch", EFT_NOOP},
  {"forkpty", EFT_NOOP},
  {"fprintf", EFT_NOOP},
  {"fputc", EFT_NOOP},
  {"fputs", EFT_NOOP},
  {"fread", EFT_NOOP},
  {"free", EFT_NOOP},
  {"free_all_mem", EFT_NOOP},
  {"freeaddrinfo", EFT_NOOP},
  {"frexp", EFT_NOOP},
  {"fscanf", EFT_NOOP},
  {"fseek", EFT_NOOP},
  {"fseeko", EFT_NOOP},
  {"fseeko64", EFT_NOOP},
  {"fsetpos", EFT_NOOP},
  {"fstat", EFT_NOOP},
  {"fstat64", EFT_NOOP},
  {"fstatfs", EFT_NOOP},
  {"fstatvfs64", EFT_NOOP},
  {"ftell", EFT_NOOP},
  {"ftello", EFT_NOOP},
  {"ftello64", EFT_NOOP},
  {"ftok", EFT_NOOP},
  {"funlockfile", EFT_NOOP},
  {"fwrite", EFT_NOOP},
  // You get the picture ..
  {"fopen", EFT_ALLOC},
  {"fopen64", EFT_ALLOC},
  {"malloc", EFT_ALLOC},
  // ...
  {"ctime", EFT_NOSTRUCT_ALLOC},
  {"mmap", EFT_NOSTRUCT_ALLOC},
  {"mmap64", EFT_NOSTRUCT_ALLOC},
  {"sbrk", EFT_NOSTRUCT_ALLOC},
  // ...
  {"gethostbyaddr", EFT_STAT},
  {"gethostbyname", EFT_STAT},
  // ...
  {"getcwd", EFT_REALLOC},
  {"mem_realloc", EFT_REALLOC},
  {"realloc", EFT_REALLOC},
  // ...
  {"memset", EFT_L_A0},
  {"mremap", EFT_L_A0},
  // ...
  {"realpath", EFT_L_A1},
  // ...
  {"freopen", EFT_L_A2},
  {"freopen64", EFT_L_A2},
  // ...
  {"memccpy", EFT_L_A0__A0R_A1R},
  {"memmove", EFT_L_A0__A0R_A1R},
  // ...
  {"strtod", EFT_A1R_A0},
  {"strtof", EFT_A1R_A0},
  // ...
  {"readdir_r", EFT_A2R_A1},
  // ...

  //This must be the last entry.
  {0, EFT_NOOP}  
};
