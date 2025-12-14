# LDN - Liar Data Notation

EDN-like data format library for liar ecosystem.

## Features
- Parse LDN from strings/files
- Serialize Rust types to LDN
- Data types: nil, bool, int, float, string, keyword, symbol, vector, map, set

## Syntax
```clojure
{:name "my-app"
 :version "0.1.0"
 :deps {foo {:path "../foo"}
        bar {:git "https://..." :sha "abc123"}}}
```

## API
- `ldn::parse(source: &str) -> Result<Value>`
- `ldn::Value` enum with variants for each type
- Serde integration (optional feature)

## Usage
Primary consumer: laird (dependency manager)
Also useful for config files, data interchange
