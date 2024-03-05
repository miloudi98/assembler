## x86 assembler
This is an implementation of an x86 assembler based on [intel's reference manual](https://www.intel.com/content/www/us/en/developer/articles/technical/intel-sdm.htmlintel's). 
Please note that this is a work in progress and a lot of the design might change.

The project is currently undergoing a rewrite, old versions of the code can be found in the `archive` folder. 
* If you would like to take a look at the front end of the compiler please refer to following files: [Token.h](https://github.com/miloudi98/assembler/blob/master/lib/frontend/token.hh), [lexer.cc](https://github.com/miloudi98/assembler/blob/master/lib/frontend/lexer.cc),
[AST.h](https://github.com/miloudi98/assembler/blob/master/lib/frontend/ast.hh) and finally [parser.cc](https://github.com/miloudi98/assembler/blob/master/lib/frontend/parser.cc).

* The current design of the assembler aims to assign each x86 mnemonic its own type. For instance, [here is how the instruction `AND` is represented](https://github.com/miloudi98/assembler/blob/6e069b98ac78853615ae90204cef94f7c95e807e/archive/old_lib4/backend/x86instructions/x86-instructions-shard-0.hh#L373-L469). You can find the list of all instructions [here](https://github.com/miloudi98/assembler/blob/master/archive/old_lib4/backend/x86instructions/x86-instructions-shard-0.hh). All the C++ machinery required to make this design happen can be found in [emitters.hh](https://github.com/miloudi98/assembler/blob/master/archive/old_lib4/backend/codegen/emitters.hh).

* Every instruction ([example](https://github.com/miloudi98/assembler/blob/6e069b98ac78853615ae90204cef94f7c95e807e/archive/old_lib4/backend/x86instructions/x86-instructions-shard-0.hh#L373-L469)) has a function [emit](https://github.com/miloudi98/assembler/blob/b70a9c439ee0a978ffe126039ddd0f845ff224b7/archive/old_lib4/backend/codegen/emitters.hh#L392) that takes in a list of `x86 operands`, parsed and semantically checked by the front end 
and then lowered to a convenient IR, and outputs an instance of [X86ByteCode](https://github.com/miloudi98/assembler/blob/b70a9c439ee0a978ffe126039ddd0f845ff224b7/archive/old_lib4/backend/codegen/emitters.hh#L80) which holds the bytes of the encoded instruction as well as additional information concerning
potential patch offsets.

* Another important piece of the puzzle is the [Emitter](https://github.com/miloudi98/assembler/blob/b70a9c439ee0a978ffe126039ddd0f845ff224b7/archive/old_lib4/backend/codegen/emitters.hh#L114). It is a type templated on the Instruction Encoding Format thoroughly explained in [intel's reference manual](https://www.intel.com/content/www/us/en/developer/articles/technical/intel-sdm.htmlintel's).

I'm not currently devoting much time to this project because of my heavy workload at university. I plan to resume once that's completed, which should be in about a month.

Thanks for taking a look at my project.
