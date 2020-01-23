# Tiger ML
A compiler for the Tiger language, written in ML.

From Appel A.W.'s Modern Compiler Implementation in ML.

## Contents

The **compiler** folder contains the source code for the Tiger compiler and will be updated as we progress through the stages of the book.

The **extra** folder contains other book assignments which are not part of the final compiler, but still worthwhile work.

The **tareaX** folders include the homework assignments for the Compilers course.

## Instructions
### Pre-requisites
Install [Standard ML of New Jersey](http://www.smlnj.org/), the ML compiler used for this project.
### Compiling
Run the interactive ML tool:
'''
sml
'''
Invoke ML-Lex to update any changes to the language:
'''
CM.make "sources.com";
'''
Run the parser:
'''
Parse.parse "test.tig";
'''