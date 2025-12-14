# laird - CLI Interface

Command-line interface for laird package manager.

## Commands
```bash
laird new <name>      # Create new project with project.edn scaffold
laird fetch           # Download/update dependencies only
laird build           # Compile project and all deps
laird run [args...]   # Build and execute
laird clean           # Remove build artifacts
laird update          # Update lock file to latest compatible versions
laird tree            # Show dependency tree
```

## Flags
```bash
--release            # Optimized build
--verbose / -v       # Detailed output
--lib-path <dir>     # Additional library search path
--offline            # Don't fetch, use cached only
```

## Project Scaffold (laird new)
```
my-project/
├── project.edn
├── src/
│   └── main.liar
└── .gitignore
```

## Depends on
- ldn (parse project.edn)
- laird resolver
- laird compiler/linker
- clap (CLI parsing)
