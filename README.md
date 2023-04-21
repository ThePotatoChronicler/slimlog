# slimlog
slimlog compiles a lisp-like language to [mlog](https://mindustrygame.github.io/wiki/logic/0-introduction/)

## Documentation
Documentation can be found in the [docs](docs) directory
- [Syntax](docs/syntax.md)
- [Commands](docs/commands.md)

## Architecture
slimlog is divided into three distinct parts
- **Parser**
  - Parses the source file
- **Compiler**
  - Compiles statements into instructions
  - May perform compile-time optimisations
- **Translator**
  - Translates instructions to processor instructions
  - May perform translation-time optimisations (transopts)

## Versioning
Versions are whatever until 1.0.0 when all the major TODOs are completed

## Name
Originally called mlogs, but slimlog was suggested by a very important Indian friend of mine,
so I changed it while I still could

## TODOs
At this point, the parser is *functional*, it will parse programs just fine,
but it will not report any kind of syntax error whatsoever, instead handing out a cryptic error message

The compiler is functional, but it will not report the location of compilation errors yet

* [x] Proper parser errors
* [x] All processor instructions
* [x] Documentation
* [ ] Virtual processor
  - Performance analysis
  - Potentially verify optimisation correctness

There is still a lot to do
