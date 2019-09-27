#! /bin/sh

#for i in tests/test/*.txt; \
#	do echo "lexing $i";\
#		python cheatlex.py < $i > $i.lex;
#	done;
#	mv tests/test/*.txt.lex tests/lexgiven;

for i in tests/lexgiven/*.txt.lex; \
 	do echo "parsing $i";\
			racket mattparse/pyparse_rkt.zo < $i > $i.ast;\
			mv $i.ast `echo $i.ast | sed 's/\(.*\.\)lex.ast/\1ast/'` ;\
	done;\
		mv tests/lexgiven/*.txt.ast tests/parsegiven/;
		
for i in tests/parsegiven/*.txt.ast; \
		do echo "translating $i";\
			racket mattparse/pytrans_rkt.zo < $i > $i.hir;\
			mv $i.hir `echo $i.hir | sed 's/\(.*\.\)ast.hir/\1hir/'` ;\
	done;\
		mv tests/parsegiven/*.txt.hir tests/transgiven/;

for i in tests/transgiven/*.txt.hir; \
		do echo "desugaring $i";\
			racket mattparse/pydesugar_rkt.zo < $i > $i.lir;\
			mv $i.lir `echo $i.lir | sed 's/\(.*\.\)hir.lir/\1lir/'` ;\
	done;\
		mv tests/transgiven/*.txt.lir tests/desugargiven/;
		
for i in tests/desugargiven/*.txt.lir; \
		do echo "cps converting $i";\
			racket mattparse/pycps_rkt.zo < $i > $i.cps;\
			mv $i.cps `echo $i.cps | sed 's/\(.*\.\)lir.cps/\1cps/'` ;\
	done;\
		mv tests/desugargiven/*.txt.cps tests/cpsgiven/;
		
