(KEYWORD import)
(ID "sys")
(PUNCT ",")
(ID "re")
(PUNCT ",")
(ID "glob")
(NEWLINE)
(KEYWORD for)
(ID "pat")
(KEYWORD in)
(ID "sys")
(PUNCT ".")
(ID "argv")
(PUNCT "[")
(LIT 1)
(PUNCT ":")
(PUNCT "]")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD for)
(ID "file")
(KEYWORD in)
(ID "glob")
(PUNCT ".")
(ID "glob")
(PUNCT "(")
(ID "pat")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "newfile")
(PUNCT "=")
(LIT 1)
(NEWLINE)
(KEYWORD for)
(ID "para")
(KEYWORD in)
(ID "open")
(PUNCT "(")
(ID "file")
(PUNCT ")")
(PUNCT ".")
(ID "read")
(PUNCT "(")
(PUNCT ")")
(PUNCT ".")
(ID "split")
(PUNCT "(")
(LIT "\n\n")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "dups")
(PUNCT "=")
(ID "re")
(PUNCT ".")
(ID "findall")
(PUNCT "(")
(LIT "(?m)(^.*(\\b\\w+\\b)\\s*\\b\\2\\b.*$)")
(PUNCT ",")
(ID "para")
(PUNCT ")")
(NEWLINE)
(KEYWORD if)
(ID "dups")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD if)
(ID "newfile")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "print")
(LIT "%s\n%s\n")
(PUNCT "%")
(PUNCT "(")
(LIT "-")
(PUNCT "*")
(LIT 70)
(PUNCT ",")
(ID "file")
(PUNCT ")")
(NEWLINE)
(ID "newfile")
(PUNCT "=")
(LIT 0)
(NEWLINE)
(DEDENT)
(KEYWORD for)
(ID "dup")
(KEYWORD in)
(ID "dups")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "print")
(LIT "[%s] -->")
(PUNCT "%")
(ID "dup")
(PUNCT "[")
(LIT 1)
(PUNCT "]")
(PUNCT ",")
(ID "dup")
(PUNCT "[")
(LIT 0)
(PUNCT "]")
(NEWLINE)
(DEDENT)
(DEDENT)
(DEDENT)
(DEDENT)
(DEDENT)
(ENDMARKER)
