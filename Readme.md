This repository is meant to incrementally implement a FreePascal 3 parser and code browser as it is e.g. available in https://github.com/rochus-keller/LisaPascal.

It turned out that there is no formal EBNF grammar for the current FreePascal 3.2.2 version. The closest I found was [this](https://github.com/graemeg/fpGUI/blob/master/docs/fpc_lang_ref.ipf) because of a hint [from here](https://forum.lazarus.freepascal.org/index.php?topic=33853.0), but it turned out to be incomplete and too different from the current language specification; I therefore manually transcribed all syntax diagrams from the latter.

The given syntax is gradually migrated to an LL(1) compatible grammar using [EbnfStudio](https://github.com/rochus-keller/EbnfStudio); from that a parser can be generated.

The lexer was adapted/extended from the LisaPascal project and successfully tested with the FPC 3.2.2 source tree (even detected issues in the FPC source code). The preprocessor works as far as required by the FP compiler source code.

This is work in progress.
