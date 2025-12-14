# laird - Dependency Resolver & Fetcher

Core dependency management for liar projects.

## Features
- Parse project.edn for dependency declarations
- Resolve dependency graph (detect cycles, version conflicts)
- Fetch from local paths (copy/symlink)
- Fetch from git (clone, checkout sha/tag/branch)
- Generate/update project.lock.edn for reproducibility
- Cache management (~/.laird/ or .laird/)

## project.edn format
```clojure
{:project {:name "my-app"
           :version "0.1.0"
           :entry "src/main.liar"}
 :paths ["src" "lib"]
 :deps {foo {:path "../foo"}
        bar {:git "https://github.com/user/bar" :tag "v1.0"}}}
```

## Dependency Sources
- `:path` - local filesystem
- `:git` + `:sha` - exact commit
- `:git` + `:tag` - tagged release
- `:git` + `:branch` - branch head (locks to sha)

## Depends on
- ldn (for parsing project.edn)
- git2 crate (for git operations)
