# liar VS Code Extension

Syntax highlighting for the liar programming language.

## Installation (Local)

1. Copy this folder to your VS Code extensions directory:
   ```bash
   # macOS/Linux
   cp -r liar-vscode ~/.vscode/extensions/liar-lang-0.1.0
   
   # Windows
   copy liar-vscode %USERPROFILE%\.vscode\extensions\liar-lang-0.1.0
   ```

2. Restart VS Code

3. Open any `.liar` file

## Features

- Syntax highlighting for:
  - Comments (`;`)
  - Strings with escape sequences
  - Keywords (`:keyword`)
  - Numbers (int, float, hex)
  - Constants (`true`, `false`, `nil`)
  - Definitions (`defun`, `def`, `defstruct`, `defprotocol`)
  - Special forms (`let`, `plet`, `fn`, `if`, `match`, etc.)
  - Built-in functions (seq, collection, iterator ops)
  - Threading macros (`->`, `->>`, etc.)
  - Async (`async`, `await`, `pmap`)
  - Atoms (`atom`, `swap!`, `reset!`, `@var`)
  - Memory ops (`share`, `clone`, `drop`)
  - Collections (`[]`, `{}`, `#{}`, `<[]>`, `<{}>`, `<<>>`)

- Bracket matching and auto-close
- Comment toggling with `Cmd+/` / `Ctrl+/`

## File Structure

```
liar-vscode/
├── package.json              # Extension manifest
├── language-configuration.json   # Brackets, comments, indent
├── syntaxes/
│   └── liar.tmLanguage.json  # TextMate grammar
├── sample.liar               # Example file
└── README.md
```

## Future: Language Server

For go-to-definition, autocomplete, and error checking, 
a Language Server Protocol (LSP) implementation is planned.
