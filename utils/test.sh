#! /bin/sh

# Ashton Snelgrove
# u0662114
# snelgrov@eng.utah.edu
#rm pytrans;
#echo testing case $1;
#make trans -s < tests/parsegiven/$1.txt.ast > tests/transoutput/$1.txt.hir &&
#meld tests/transoutput/$1.txt.hir tests/transgiven/$1.txt.hir

rm pydesugar;
make desugar -s < tests/transgiven/$1.txt.ast > tests/desugaroutput/$1.txt.lir &&
meld tests/desugaroutput/$1.txt.lir tests/desugargiven/$1.txt.lir
