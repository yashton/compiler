(ID "shape")
(PUNCT "=")
(ID "int")
(PUNCT "(")
(ID "input")
(PUNCT "(")
(ID "menu")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(KEYWORD while)
(ID "shape")
(PUNCT "!=")
(LIT 4)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "shape")
(PUNCT "==")
(LIT 1)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "length")
(PUNCT "=")
(ID "float")
(PUNCT "(")
(ID "input")
(PUNCT "(")
(LIT "Length: ")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "print")
(PUNCT "(")
(LIT "Area of square = ")
(PUNCT ",")
(ID "length")
(PUNCT "**")
(LIT 2)
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD elif)
(ID "shape")
(PUNCT "==")
(LIT 2)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "length")
(PUNCT "=")
(ID "float")
(PUNCT "(")
(ID "input")
(PUNCT "(")
(LIT "Length: ")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "width")
(PUNCT "=")
(ID "float")
(PUNCT "(")
(ID "input")
(PUNCT "(")
(LIT "Width: ")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(ID "print")
(PUNCT "(")
(LIT "Area of rectangle = ")
(PUNCT ",")
(ID "length")
(PUNCT "*")
(ID "width")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "print")
(PUNCT "(")
(LIT " Not a valid shape. try again")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(DEDENT)
(ENDMARKER)