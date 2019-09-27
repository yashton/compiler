# Ashton Snelgrove
# u0662114
# snelgrov@eng.utah.edu

DIFF=meld

# name of temp dir to copy files into before compression
SUBMIT_DIR=ashton

# Reference implementation locations
REF_LEX=python matt/cheatlex.py
REF_PARSE=racket matt/pyparse_rkt.zo
REF_TRANS=racket matt/pytrans_rkt.zo
REF_DESUGAR=racket matt/pydesugar_rkt.zo
REF_CPS=racket matt/pycps_rkt.zo
REF_MVE=racket matt/pymve_rkt.zo
REF_CC=racket matt/pycc_rkt.zo
REF_LL=racket matt/pyll_rkt.zo

all: pytrans pyparse pylex pydesugar pycps pymve pycc pyll

	@ echo "compiled"

default: all

run: all
	pylex | ./pyparse | ./pytrans | ./pydesugar | ./pycps | ./pymve | ./pycc | ./pyll

clean:
	rm -v -f *.pyc
	rm -f tests/lex_output/*
	rm -f tests/parse_output/* 
	rm -f tests/trans_output/* 
	rm -f tests/desugar_output/*
	rm -f tests/cps_output/*
	rm -f tests/mve_output/*
	rm -f tests/ll_output/*
	rm -f tests/cc_output/*
	rm -v -f compiled
	rm -rf $(SUBMIT_DIR)
	rm -f compiler.tar.gz
	rm -v -f pyparse pytrans pylex pydesugar pycps pymve pyll pycc

# Lexing
	
pylex: pylex.py
	echo '#! /bin/sh\npython3 pylex.py' > pylex; chmod a+x pylex;
	
lex: pylex
	./pylex
	
# Parsing
	
pyparse: derivative-parsers.rkt pyparse.rkt
	raco exe pyparse.rkt

parse: pyparse
	./pyparse
	
# Translating
	
pytrans: pytrans.rkt
	raco exe pytrans.rkt
	
trans: pytrans
	./pytrans
	
# Desugar
	
pydesugar: pydesugar.rkt
	raco exe pydesugar.rkt
	
desugar: pydesugar
	./pydesugar
	
# CPS conversion
	
pycps: pycps.rkt
	raco exe pycps.rkt
	
cps: pycps
	./pycps
	
# Mutable variable elimination
	
pymve: pymve.rkt
	raco exe pymve.rkt
	
mve: pymve
	./pymve
	
# Flat closure conversion
	
pycc: pycc.rkt
	raco exe pycc.rkt
	
cc: pycc
	./pycc
	
# Lambda lifting
	
pyll: pyll.rkt
	raco exe pyll.rkt
	
ll: pyll
	./pyll
		
# C code emission
pytoc: pytoc.rk
	raco exe pytoc.rkt
	
toc: pytoc
	./pytoc
	
# Tests
test: all
	for i in tests/input/*;\
		do echo "all conversions on $$i";\
		./pylex | ./pyparse | ./pytrans | ./pydesugar | ./pycps |\
			./pymve | ./pycc | ./pyll\
				< $$i > tests/ll_output/`basename $$i`.ll;\
	done;\
	$(DIFF) tests/ll_output tests/ll_given;

test/%: tests/input/%.txt all
	./pylex | ./pyparse | ./pytrans | ./pydesugar | ./pycps |\
		./pymve | ./pycc | ./pyll	\
			< $< > tests/ll_output/$(@F).txt.ll
	$(DIFF) tests/ll_output/$(@F).txt.ll tests/ll_given/$(@F).txt.ll
	
test-lex: pylex
	for i in tests/input/*.txt;\
	do echo "lexing $$i";\
		pylex < $$i > tests/lex_output/`basename $$i`.lex;\
	done;\
	$(DIFF) tests/lex_output tests/lex_given;

test-lex/%: tests/input/%.txt pylex
	./pylex < $< > tests/lex_output/$(@F).txt.lex
	$(DIFF) tests/lex_output/$(@F).txt.lex tests/lex_given/$(@F).txt.lex


test-parse: pyparse
	for i in tests/lex_given/*.txt.lex; \
	do echo "parsing $$i";\
		./pyparse < $$i > tests/parse_output/`basename $$i .lex`.ast;\
	done;\
	$(DIFF) tests/parse_output tests/parse_given;
			
test-parse/%: tests/lex_given/%.txt.lex pyparse
	./pyparse < $< > tests/parse_output/$(@F).txt.ast
	$(DIFF) tests/parse_output/$(@F).txt.ast tests/parse_given/$(@F).txt.ast
			
test-trans: pytrans
	for i in tests/parse_given/*.txt.ast; \
	do echo "translating $$i";\
		./pytrans < $$i\ > tests/transouput/`basename $$i .ast`.hir;\
	done;\
	$(DIFF) tests/trans_output tests/trans_given;
			
test-trans/%: tests/parse_given/%.txt.ast pytrans
	./pytrans < $< > tests/parse_output/$(@F).txt.hir
	$(DIFF) tests/trans_output/$(@F).txt.hir tests/trans_given/$(@F).txt.hir
			
			
test-desugar: pydesugar
	for i in tests/trans_given/*.txt.hir; \
	do echo "desugaring $$i";\
		./pydesugar < $$i\
			> tests/desugar_output/`basename $$i .hir`.lir;\
	done;\
	$(DIFF) tests/desugar_output tests/desugar_given;

test-desugar/%: tests/trans_given/%.txt.hir pydesugar
	./pydesugar < $< > tests/desugar_output/$(@F).txt.lir
	$(DIFF) tests/desugar_output/$(@F).txt.lir tests/desugar_given/$(@F).txt.lir
			
			
test-cps: pycps
	for i in tests/desugar_given/*.txt.lir;\
	do echo "cps converting $$i";\
		./pycps < $$i > tests/cps_output/`basename $$i .lir`.cps;\
	done;\
	$(DIFF) tests/cps_output tests/cps_given
			
test-cps/%: tests/desugar_given/%.txt.lir pycps
	./pycps < $< > tests/cps_output/$(@F).txt.cps
	$(DIFF) tests/cps_output/$(@F).txt.cps tests/cps_given/$(@F).txt.cps
	

test-mve: pymve
	for i in tests/cps_given/*.txt.cps;\
	do echo "mutable variable elimination for $$i";\
		./pymve < $$i > tests/mve_output/`basename $$i .cps`.mve;\
	done;\
	$(DIFF) tests/mve_output tests/mve_given
			
test-mve/%: tests/cps_given/%.txt.cps pymve
	./pymve < $< > tests/mve_output/$(@F).txt.mve
	$(DIFF) tests/mve_output/$(@F).txt.mve tests/mve_given/$(@F).txt.mve
	

test-cc: pycc
	for i in tests/mve_given/*.txt.mve;\
	do echo "flat closure converting $$i";\
		./pycc < $$i > tests/cc_output/`basename $$i .mve`.cc;\
	done;\
	$(DIFF) tests/cc_output tests/cc_given
			
test-cc/%: tests/mve_given/%.txt.mve pycc
	./pycc < $< > tests/cc_output/$(@F).txt.cc
	$(DIFF) tests/cc_output/$(@F).txt.cc tests/cc_given/$(@F).txt.cc
	
	
test-ll: pyll
	for i in tests/cc_given/*.txt.cc;\
	do echo "lambda lifting  $$i";\
		./pyll < $$i > tests/ll_output/`basename $$i .cc`.ll;\
	done;\
	$(DIFF) tests/ll_output tests/ll_given
			
test-ll/%: tests/cc_given/%.txt.cc pyll
	./pyll < $< > tests/ll_output/$(@F).txt.ll
	$(DIFF) tests/ll_output/$(@F).txt.ll tests/ll_given/$(@F).txt.ll


diff/%: tests/%_given tests/%_output
	$(DIFF) tests/$(@F)_output tests/$(@F)_given
	
# Test generation

# reference implementation recipe
ref-impl: matt/cheatlex.py matt/pyparse_rkt.zo matt/pytrans_rkt.zo matt/pydesugar_rkt.zo matt/pycps_rkt.zo matt/pymve_rkt.zo matt/pycc_rkt.zo matt/pyll_rkt.zo

test-rebuild: test-rebuild/lex test-rebuild/parse test-rebuild/trans test-rebuild/desugar test-rebuild/cps

test-rebuild/lex: tests/input tests/lex_given matt/cheatlex.py
	for i in tests/input/*.txt; \
	do echo "lexing $$i";\
		$(REF_LEX) < $$i > $$i.lex;\
	done;\
	mv tests/input/*.txt.lex tests/lex_given;

test-rebuild/parse: tests/lex_given tests/parse_given matt/pyparse_rkt.zo
	for i in tests/lex_given/*.lex; \
	do echo "parsing $$i";\
		$(REF_PARSE) < $$i > tests/parse_given/`basename $$i .lex`.ast;\
	done;
		
test-rebuild/trans: tests/parse_given tests/trans_given matt/pytrans_rkt.zo
	for i in tests/parse_given/*.ast; \
	do echo "translating $$i";\
		$(REF_TRANS) < $$i > tests/trans_given/`basename $$i .ast`.hir;\
	done;
	
test-rebuild/desugar: tests/trans_given tests/desugar_given matt/pydesugar_rkt.zo
	for i in tests/trans_given/*.hir; \
	do echo "desugaring $$i";\
		$(REF_DESUGAR) < $$i > tests/desugar_given/`basename $$i .hir`.lir;\
	done;

test-rebuild/cps: tests/desugar_given tests/cps_given matt/pycps_rkt.zo
	for i in tests/desugar_given/*.lir; \
	do echo "cps converting $$i";\
		$(REF_CPS) < $$i > tests/cps_given/`basename $$i .lir`.cps;\
	done;

test-rebuild/mve: tests/cps_given tests/mve_given matt/pymve_rkt.zo
	for i in tests/cps_given/*.cps; \
	do echo "mutable variable elimination for $$i";\
		$(REF_MVE) < $$i > tests/mve_given/`basename $$i .cps`.mve;\
	done;
	
test-rebuild/cc: tests/mve_given tests/cc_given matt/pycc_rkt.zo
	for i in tests/mve_given/*.mve; \
	do echo "flat closure converting $$i";\
		$(REF_CC) < $$i > tests/cc_given/`basename $$i .mve`.cc;\
	done;
	
test-rebuild/ll: tests/cc_given tests/ll_given matt/pyll_rkt.zo
	for i in tests/cc_given/*.cc; \
	do echo "lambda lifting $$i";\
		$(REF_LL) < $$i > tests/ll_given/`basename $$i .cc`.ll;\
	done;

test-create/%: tests/input/%.txt ref-impl
	$(REF_LEX) < tests/input/$(@F).txt\
		> tests/lex_given/$(@F).txt.lex
	$(REF_PARSE) < tests/lex_given/$(@F).txt.lex\
		> tests/parse_given/$(@F).txt.ast
	$(REF_TRANS) < tests/parse_given/$(@F).txt.ast\
		> tests/trans_given/$(@F).txt.hir
	$(REF_DESUGAR) < tests/trans_given/$(@F).txt.hir\
		> tests/desugar_given/$(@F).txt.lir
	$(REF_CPS) < tests/desugar_given/$(@F).txt.lir\
		> tests/cps_given/$(@F).txt.cps
	$(REF_MVE) < tests/cps_given/$(@F).txt.cps\
		> tests/mve_given/$(@F).txt.mve
	$(REF_CC) < tests/mve_given/$(@F).txt.mve\
		> tests/cc_given/$(@F).txt.cc
	$(REF_LL) < tests/cc_given/$(@F).txt.cc\
		> tests/ll_given/$(@F).txt.ll
	
# HIR, LIR and CPS execution
exec-hir: headers/hir_header.rkt
	@ cat headers/hir-header.rkt - | racket
	
exec-hir/%: tests/trans_given/%.txt.hir headers/hir_header.rkt
	cat headers/hir-header.rkt $< | racket 
	
exec-lir: headers/lir-header.rkt
	@ cat headers/lir-header.rkt - | racket
	
exec-lir/%: tests/desugar_output/%.txt.lir headers/lir-header.rkt
	cat headers/lir-header.rkt $< | racket
	
exec-cps: headers/cps-header.rkt
	@ cat headers/cps-header.rkt - | racket
	
exec-cps/%: tests/cps_output/%.txt.cps headers/cps-header.rkt
	cat headers/cps-header.rkt $< | racket 
	
# Submit file generation
submit: clean Makefile readme tests pylex.py ply derivative-parsers.rkt pyparse.rkt python-ast.grm.sx pytrans.rkt pydesugar.rkt pycps.rkt pymve.rkt pyll.rkt pycc.rkt pytoc.rkt pytoc-common.rkt
	mkdir $(SUBMIT_DIR)
	@ echo "copying files into temp dir"
	cp Makefile readme  $(SUBMIT_DIR)/
	cp -r tests $(SUBMIT_DIR)/
	cp pylex.py $(SUBMIT_DIR)/
	mkdir $(SUBMIT_DIR)/ply
	cp ply/*.py $(SUBMIT_DIR)/ply/
	cp derivative-parsers.rkt pyparse.rkt python-ast.grm.sx $(SUBMIT_DIR)/
	cp pytrans.rkt $(SUBMIT_DIR)/
	cp pydesugar.rkt $(SUBMIT_DIR)/
	cp pycps.rkt $(SUBMIT_DIR)/
	cp pymve.rkt pyll.rkt pycc.rkt pytoc.rkt pytoc-common.rkt $(SUBMIT_DIR)/
	@ echo "compressing files and remove temp dir"
	tar -czf compiler.tar.gz $(SUBMIT_DIR)/
	rm -r $(SUBMIT_DIR)
	
help:
	@ echo "\tAvailable options:\n\
	test - Run all test cases directly from Python source to cps\n\
	test/% - Translate tests/input/%.txt into cps\n\
	test-{lex, parse, trans, desugar, cps, mve, cc, ll} - Run all cases for given conversion\n\
	test-{lex, parse, trans, desugar, cps, mve, cc, ll}/% - Run specific case for conversion\n\
	diff/{lex, parse, trans, desugar, cps, mve, cc, ll} diff all tests for conversion\n\
	test-rebuild - rebuild all test cases from files in tests/input\n\
	test-rebuild/{lex, parse, trans, desugar, cps, mve, cc, ll}\
	 - rebuild for given conversion\n\
	test-create/% - create all tests for tests/input/%.txt\n\
	exec-{hir, lir, cps} - execute file on stdio\n\
	exec-{hir, lir, cps}/% - execute numbered test case\n\
	submit - create a compilers.tar.gz file for submission"
	
# Ashton Snelgrove
# u0662114
# snelgrov@eng.utah.edu

