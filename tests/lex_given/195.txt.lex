(KEYWORD def)
(ID "partition")
(PUNCT "(")
(ID "l")
(PUNCT ",")
(ID "start")
(PUNCT ",")
(ID "end")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "pivot")
(PUNCT "=")
(ID "l")
(PUNCT "[")
(ID "end")
(PUNCT "]")
(NEWLINE)
(ID "bottom")
(PUNCT "=")
(ID "start")
(PUNCT "-")
(LIT 1)
(NEWLINE)
(ID "top")
(PUNCT "=")
(ID "end")
(NEWLINE)
(ID "done")
(PUNCT "=")
(KEYWORD False)
(NEWLINE)
(KEYWORD while)
(KEYWORD not)
(ID "done")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD while)
(KEYWORD not)
(ID "done")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "bottom")
(PUNCT "=")
(ID "bottom")
(PUNCT "+")
(LIT 1)
(NEWLINE)
(KEYWORD if)
(ID "bottom")
(PUNCT "==")
(ID "top")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "done")
(PUNCT "=")
(LIT 1)
(NEWLINE)
(KEYWORD break)
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "l")
(PUNCT "[")
(ID "bottom")
(PUNCT "]")
(PUNCT ">")
(ID "pivot")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "l")
(PUNCT "[")
(ID "top")
(PUNCT "]")
(PUNCT "=")
(ID "l")
(PUNCT "[")
(ID "bottom")
(PUNCT "]")
(NEWLINE)
(KEYWORD break)
(NEWLINE)
(DEDENT)
(DEDENT)
(KEYWORD while)
(KEYWORD not)
(ID "done")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "top")
(PUNCT "=")
(ID "top")
(PUNCT "-")
(LIT 1)
(NEWLINE)
(KEYWORD if)
(ID "top")
(PUNCT "==")
(ID "bottom")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "done")
(PUNCT "=")
(KEYWORD True)
(NEWLINE)
(KEYWORD break)
(NEWLINE)
(DEDENT)
(KEYWORD if)
(ID "l")
(PUNCT "[")
(ID "top")
(PUNCT "]")
(PUNCT "<")
(ID "pivot")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "l")
(PUNCT "[")
(ID "bottom")
(PUNCT "]")
(PUNCT "=")
(ID "l")
(PUNCT "[")
(ID "top")
(PUNCT "]")
(NEWLINE)
(KEYWORD break)
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(ID "l")
(PUNCT "[")
(ID "top")
(PUNCT "]")
(PUNCT "=")
(ID "pivot")
(NEWLINE)
(KEYWORD return)
(ID "top")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "quicksort")
(PUNCT "(")
(ID "l")
(PUNCT ",")
(ID "start")
(PUNCT ",")
(ID "end")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "start")
(PUNCT "<")
(ID "end")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "split")
(PUNCT "=")
(ID "partition")
(PUNCT "(")
(ID "l")
(PUNCT ",")
(ID "start")
(PUNCT ",")
(ID "end")
(PUNCT ")")
(NEWLINE)
(ID "quicksort")
(PUNCT "(")
(ID "l")
(PUNCT ",")
(ID "start")
(PUNCT ",")
(ID "split")
(PUNCT "-")
(LIT 1)
(PUNCT ")")
(NEWLINE)
(ID "quicksort")
(PUNCT "(")
(ID "l")
(PUNCT ",")
(ID "split")
(PUNCT "+")
(LIT 1)
(PUNCT ",")
(ID "end")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD else)
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(NEWLINE)
(DEDENT)
(DEDENT)
(ID "start")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(ID "end")
(PUNCT "=")
(LIT 8)
(NEWLINE)
(ID "li")
(PUNCT "=")
(PUNCT "[")
(LIT 4)
(PUNCT ",")
(LIT 3)
(PUNCT ",")
(LIT 2)
(PUNCT ",")
(LIT 10)
(PUNCT ",")
(LIT 1)
(PUNCT ",")
(LIT 9)
(PUNCT ",")
(LIT 7)
(PUNCT ",")
(LIT 2)
(PUNCT ",")
(LIT 11)
(PUNCT "]")
(NEWLINE)
(ID "quicksort")
(PUNCT "(")
(ID "li")
(PUNCT ",")
(ID "start")
(PUNCT ",")
(ID "end")
(PUNCT ")")
(NEWLINE)
(KEYWORD for)
(ID "i")
(KEYWORD in)
(ID "li")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "print")
(PUNCT "(")
(ID "i")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ENDMARKER)
