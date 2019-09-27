#! /bin/sh

#make lex -s < tests/input/$1.txt > tests/lexgiven/$1.txt.lex;
python cheatlex.py < tests/input/$1.txt > tests/lexgiven/$1.txt.lex;
racket mattparse/pyparse_rkt.zo < tests/lexgiven/$1.txt.lex > tests/parsegiven/$1.txt.ast;
racket mattparse/pytrans_rkt.zo < tests/parsegiven/$1.txt.ast > tests/transgiven/$1.txt.hir;
racket mattparse/pydesugar_rkt.zo < tests/transgiven/$1.txt.hir > tests/desugargiven/$1.txt.lir;
racket mattparse/pycps_rkt.zo < tests/desugargiven/$1.txt.lir > tests/cpsgiven/$1.txt.cps
