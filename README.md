## x86 assembler
This is an implementation of an x86 assembler based on [intel's reference manual](https://www.intel.com/content/www/us/en/developer/articles/technical/intel-sdm.htmlintel's). 
Please note that this is a work in progress and a lot of the design might change.

The project is currently undergoing a rewrite, old versions of the code can be found in the `archive` folder. 
* If you would like to take a look at the front end of the compiler please refer to following files: [Token.h](https://github.com/miloudi98/assembler/blob/master/lib/frontend/token.hh), [lexer.cc](https://github.com/miloudi98/assembler/blob/master/lib/frontend/lexer.cc),
[AST.h](https://github.com/miloudi98/assembler/blob/master/lib/frontend/ast.hh) and finally [parser.cc](https://github.com/miloudi98/assembler/blob/master/lib/frontend/parser.cc).

* The current design of the assembler aims to assign each x86 mnemonic its own type. For instance, [here is how the instruction `AND` is represented](https://github.com/miloudi98/assembler/blob/6e069b98ac78853615ae90204cef94f7c95e807e/archive/old_lib4/backend/x86instructions/x86-instructions-shard-0.hh#L373-L469). You can find the list of all instructions [here](https://github.com/miloudi98/assembler/blob/master/archive/old_lib4/backend/x86instructions/x86-instructions-shard-0.hh). 
