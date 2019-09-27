#!/bin/python3

# Phillip Mates
# Compilers Spring 2011
# Lexer Assignment

# Download test cases from TA

for i in range(1,101):
   print("wget \"http://www.cs.utah.edu/~yrbahn/test/" + str(i) + ".txt\"")
   # save *.res.txt to *.txt.res
   print("wget \"http://www.cs.utah.edu/~yrbahn/test/result/" + str(i) + ".res.txt\" -O \"" + str(i) + ".txt.res\"")
