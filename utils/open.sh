#! /bin/sh

# Ashton Snelgrove
# u0662114
# snelgrov@eng.utah.edu
gedit tests/input/$1.txt &
gedit tests/parsegiven/$1.txt.ast &
gedit tests/transgiven/$1.txt.hir &
