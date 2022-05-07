# slimlog
slimlog compiles a lisp-like language to [mlog](https://mindustrygame.github.io/wiki/logic/0-introduction/)

## Architecture
slimlog is divided into three distinct parts
- **Parser**
  - Parses the source file
- **Compiler**
  - Compiles statements into instructions
  - May perform compile-time optimisations
- **Translator**
  - Translates instructions to processor instructions
  - May perform translation-time optimisations

## Versioning
Versions are whatever until 1.0.0 when all the major TODOs are completed

## Name
Originally called mlogs, but slimlog was suggested by a very important Indian friend of mine,
so I changed it while I still could

## TODOs
At this point, the parser is *functional*, it will parse programs just fine,
but it will not report any kind of syntax error whatsoever, instead handing out a cryptic error message

The compiler is *impractically functional*, adding new functions is
just a matter of adding a match arm and a function that returns it's instructions

The translator is missing about half of the processor instructions, but those
are not very difficult to add at all, just doing them one by one as needed

* [ ] Proper parser errors
* [ ] All processor instructions
* [ ] Documentation
* [ ] Optimisations
* [ ] Virtual processor
  - Performance analysis
  - Potentially verify optimisation correctness
* [ ] Preprocessor

There is a lot to do
