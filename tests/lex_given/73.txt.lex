(LIT "Fibonacci iterator")
(NEWLINE)
(KEYWORD class)
(ID "Fib")
(PUNCT ":")
(NEWLINE)
(INDENT)
(LIT "iterator that yields numbers in the Fibonacci sequence")
(NEWLINE)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "max")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "max")
(PUNCT "=")
(ID "max")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "__iter__")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "self")
(PUNCT ".")
(ID "a")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(ID "self")
(PUNCT ".")
(ID "b")
(PUNCT "=")
(LIT 1)
(NEWLINE)
(KEYWORD return)
(ID "self")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "__next__")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "fib")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "a")
(NEWLINE)
(KEYWORD if)
(ID "fib")
(PUNCT ">")
(ID "self")
(PUNCT ".")
(ID "max")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD raise)
(ID "StopIteration")
(NEWLINE)
(DEDENT)
(ID "self")
(PUNCT ".")
(ID "a")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "b")
(PUNCT "=")
(ID "self")
(PUNCT ".")
(ID "b")
(PUNCT ",")
(ID "self")
(PUNCT ".")
(ID "a")
(PUNCT "+")
(ID "self")
(PUNCT ".")
(ID "b")
(NEWLINE)
(KEYWORD return)
(ID "fib")
(NEWLINE)
(DEDENT)
(DEDENT)
(ENDMARKER)