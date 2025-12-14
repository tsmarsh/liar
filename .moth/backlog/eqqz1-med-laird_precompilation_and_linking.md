# laird - Precompilation & Linking

AOT compilation pipeline for liar projects.

## Features
- Compile each dependency namespace to object file (.o)
- Cache compiled artifacts (hash-based invalidation)
- Link all objects into final executable
- Incremental rebuilds (only recompile changed deps)

## Compilation Flow
```
project.edn 
    → resolve deps (from lock file)
    → for each dep namespace:
        → check cache (hash source files)
        → if miss: liar → lIR → LLVM IR → .o
        → store in cache
    → link all .o files → executable
```

## Cache Structure
```
~/.laird/
├── compiled/
│   ├── {dep-name}-{hash}.o
│   └── {dep-name}-{hash}.lir  # intermediate for debugging
└── git/
    └── {repo-hash}/  # cloned repos
```

## Depends on
- laird resolver (for dep graph)
- liar compiler (for liar → lIR)
- lair (for lIR → LLVM → .o)
- System linker (cc/ld)
