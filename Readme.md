This repository is meant to incrementally implement a FreePascal 3 parser and code browser as it is e.g. available in https://github.com/rochus-keller/LisaPascal.

It turned out that there is no formal EBNF grammar for the current FreePascal 3.2.2 version. The closest I found was [this](https://github.com/graemeg/fpGUI/blob/master/docs/fpc_lang_ref.ipf) because of a hint [from here](https://forum.lazarus.freepascal.org/index.php?topic=33853.0), but it turned out to be incomplete and too different from the current language specification; I therefore manually transcribed all syntax diagrams from the latter.

The given syntax was gradually migrated to an LL(1) compatible grammar using [EbnfStudio](https://github.com/rochus-keller/EbnfStudio); from that a parser can be generated. The grammar required ~40 LL(2) prefixes to cope with ambiguities.

The lexer was adapted/extended from the LisaPascal project and successfully tested with the FPC 3.2.2 source tree (even detected issues in the FPC source code). The preprocessor works as far as required by the FP compiler source code.

The parser is automatically generated using the new EbnfStudio C++ generator and pseudo keyword features; all 667 regular *.pas and *.pp files of the FP 3.2.2 compiler source tree can be successfully parsed in 5.4 seconds on my EliteBook (though the following had syntax errors and were fixed: widestr.pas, oglx.pas, symcpu.pas, nppcadd.pas, nppcld.pas and nppcmat.pas).

The parser also generates a syntax tree per file which requires 8.3 seconds for all 667 file of the FP compiler source tree (i.e. ~54% slower than the parser alone).

This is work in progress.
