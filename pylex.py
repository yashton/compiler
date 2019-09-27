#! /usr/bin/python3

# Ashton Snelgrove
# u0662114
# snelgrov@eng.utah.edu

import ply.lex as lex
from sys import stdin
import sys
import math

tokens = [
	'COMMENT',
	'KEYWORD',
	'PUNCT',
	'ID',
	'INDENT',
	'DEDENT',
	'NEWLINE',
	'LIT',
	'ERROR',
	'ENDMARKER'
	]
	
# States of the lexer. When in an exclusive state, only t_state_FOO rules will
# be matched. The default state is INITIAL. blankstart is used to catch blank
# first lines, and the lexer remains in INITIAL otherwise.	
states = [

	('linestart', 'exclusive'),
	('blankstart', 'exclusive'),
	('string', 'exclusive')
	]

# Comment matching. Returns no token.	
def t_COMMENT(t):
	r'\#.*'
	pass
		
# Keyword matching.
def t_KEYWORD(t):
	r'(False|None|True|and|as|assert|break|class|continue|def|del|elif|else|except|finally|for|from|global|if|import|in|is|lambda|nonlocal|not|or|pass|raise|return|try|while|with|yield)(?![_a-zA-Z])'
	return t
	
# Match any non-leading whitespace.		
def t_separators(t):
	r'[ ]+'
	pass

# Numeric literal handling. Numbers become LIT tokens, and the value becomes
# a Racket compatible literal value.
def t_IMAGINARY(t):
	r'([0-9]*\.[0-9]+|[0-9]+\.[0-9]*|[0-9]+|([0-9]+|[0-9]*\.[0-9]+|[0-9]+\.[0-9]*)[eE][-+]?[0-9]+)(j|J)'
	t.type = 'LIT'
	t.value = '+' + t.value[:-1] + 'i'
	return t

def t_EXPFLOAT(t):
	r'([0-9]+|[0-9]*\.[0-9]+|[0-9]+\.[0-9]*)[eE][-+]?[0-9]+'
	t.type = 'LIT'
	return t
	
def t_POINTFLOAT(t):
	r'[0-9]*\.[0-9]+|[0-9]+\.[0-9]*'
	t.type = 'LIT'
	return t

def t_HEXINTEGER(t):
	r'0[xX][0-9A-Fa-f]+'
	t.type = 'LIT'
	t.value = "#x" + t.value[2:]
	return t	

def t_OCTINTEGER(t):
	r'0[oO][0-7]+'
	t.type = 'LIT'
	t.value = "#o" + t.value[2:]
	return t
	
def t_BININTEGER(t):
	r'0[bB][01]+'
	t.type = 'LIT'
	t.value = "#b" + t.value[2:]
	return t

def t_DECINTEGER(t):
	r'[1-9][0-9]*|0+'
	t.type = 'LIT'
	return t

# Grouping punctuation matching rules.	
def t_GROUPINGSOPEN(t):
	r'\{|\[|\('
	t.value = '"' + t.value + '"'
	t.type = 'PUNCT'
	t.lexer.group += 1
	return t

def t_GROUPINGSCLOSE(t):
	r'\}|\]|\)'
	t.value = '"' + t.value + '"'
	t.type = 'PUNCT'
	t.lexer.group -= 1
	return t
	
# Other punctuation matching rules.

def t_DELIMITERSBIG(t):
	r'\+=|\-=|\*=|/=|//=|%=|&=|\|=|\^=|>>=|<<=|\*\*='
	t.value = '"' + t.value + '"'
	t.type = 'PUNCT'
	return t

def t_OPERATORS(t):
	r'\*\*|//|<<|>>|<=|>=|==|!='
	t.value = '"' + t.value + '"'
	t.type = 'PUNCT'
	return t

def t_DELIMITERSSMALL(t):
	r'\.|;|:|;|,|@|='
	t.value = '"' + t.value + '"'
	t.type = 'PUNCT'
	return t

def t_OPERATORSSMALL(t):
	r'<|>|\*|/|\+|\-|%|&|\||\^|~'
	t.value = '"' + t.value + '"'
	t.type = 'PUNCT'
	return t

# Explictly escaped newline matching
def t_ESCAPEDNEWLINE(t):
	r'\\\r?\n'
	t.lexer.lineno += 1
	pass	
		
# String matching. This rule does not handle escape characters
def t_STRINGOPEN(t):
	r"(?P<type>[brBR]?)(?P<openquote>'''|\"\"\"|'|\")"	
	t.lexer.stringStart = t.lexer.lexmatch.group('openquote')
	t.lexer.stringBody = ""
	t.lexer.stringType = t.lexer.lexmatch.group('type')
	t.lexer.begin('string')
	pass

def t_string_ESCAPEDSYMBOL(t):
	r'[\n\r]'
	if t.value == '\n':
		t.lexer.stringBody += "\\n"
	elif t.value == '\r':
		t.lexer.stringBody += "\\r"
	pass

def t_string_ESCAPEDESCAPE(t):
	r'\\\\'
	t.lexer.stringBody += '\\\\'
	pass

def t_string_IMPLICIT(t):
	r'\\\r?\n'
	pass
	
def t_string_ESCAPEDHEXCHAR(t):
	r'\\x(?P<ascii>[a-fA-F0-9]{2})'
	t.lexer.stringBody += chr(int(t.lexer.lexmatch.group('ascii'), 16))
	pass

def t_string_ESCAPEDOCTCHAR(t):
	r'\\(?P<ascii>[0-7]{3})'
	t.lexer.stringBody += chr(int(t.lexer.lexmatch.group('ascii'), 8))
	pass

def t_string_ESCAPEDQUOTE(t):
	r"\\[\"']"
	t.lexer.stringBody += t.value
	pass

def t_string_ESCAPEDOTHER(t):
	r'\\'
	t.lexer.stringBody += '\\'
	pass

# String internals.
def t_string_EXIT(t):
	r"('''|\"\"\"|'|\")"
	if t.lexer.stringStart == t.value:
		t.type = 'LIT'
		t.value = '"' + t.lexer.stringBody + '"'
		t.lexer.begin('INITIAL')
		return t
	else:
		if t.value == "'''":
			t.lexer.stringBody += "\\'\\'\\'"
		elif t.value == '"""':
			t.lexer.stringBody += '\\"\\"\\"'
		else:
			t.lexer.stringBody += '\\' + t.value 
		pass

def t_string_BODY(t):
	r"[^\\\"'\n\r]+"
	t.lexer.stringBody += t.value
	pass

# Identifier matching
def t_ID(t):
	r"[_A-Za-z][_A-Za-z0-9]*"
	t.value = '"' + t.value + '"'
	return t

#def t_implicit_newline(t):
#	r'\r?\n'
#	t.value = ""
# t.type = 'NEWLINE'
#	return t

# Standard newline matching	
def t_newline(t):
	r'\r?\n'
	# lineno tracks the current file line position
	t.lexer.lineno += 1
	t.type = 'NEWLINE'
	# lastpos is used to track the start of lines, as ^ and \A regex do not work.
	t.lexer.lastpos = t.lexer.lexpos
	if t.lexer.group <= 0:
		t.lexer.begin('linestart')
		return t

#t_ignore = ' \t'
		
# Error handling. Any non-matching character will emit an error and exit.
def t_error(t):
	t.value = 'Illegal character \'' + t.value + '\' on line ' +\
		str(t.lexer.lineno)
	t.lexer.skip(1)
	print('(ERROR "' + t.value + ')')
	sys.exit()
	
# these two states will only apply during the first line, which can either be
# blank or non-blank.def t_string_ESCAPEDSTR(t):
	r"\\[\"']"
	t.lexer.stringBody += t.value
	pass
def t_blankstart_linestart_BLANKLINE(t):
	r'[ ]*(\#.*)?\r?\n'
	t.lexer.lineno += 1
	pass

# The blankstart_other function is a catch-all, and places the lexer into the
# linestart state.		
def t_blankstart_other(t):
	r'(?=.)'
	t.lexer.begin('INITIAL')
		
def t_blankstart_error(t):
	t.lexer.begin('linestart')
	
# Indentation detection. The lexer will only be in the linestart state
# after a t_newline rule has been matched

def t_linestart_WHITESPACE(t):
	r'[ ]+'
	# Place the lexer back into normal operating state once handled
	t.lexer.begin('INITIAL')
#	if t.lexer.lexpos == (t.lexer.lastpos + len(t.value)):
		# lexpos is the position of the current match, lastpos that of the last
		# newline. The start of a line is indicated if the values are equal. 

	if len(t.lexer.indent) == 0:
		# no identation level
		t.lexer.indent.append(len(t.value))
		t.type = 'INDENT'
		return t
	elif t.lexer.indent[-1] == len(t.value):
		# no change in indentation level
		pass
	elif t.lexer.indent[-1] < len(t.value):
		# indentation level increase
		t.lexer.indent.append(len(t.value))
		t.type = 'INDENT'
		return t
	elif t.lexer.indent[-1] > len(t.value):
		# indentation level decrease
		t.lexer.indent.pop()
		t.type = 'DEDENT'
		t.value = 1
		return t
		
#	else:
#		pass
def t_linestart_OTHER(t):
	r'(?=.)'	
	t.lexer.begin('INITIAL')
	if t.lexer.indent[-1] > 0:
		t.type = 'DEDENT'
		t.value = len(t.lexer.indent) - 1
		t.lexer.indent = [0]
		return t
		
# The linestart_error function catches lines that begin with no whitespace, and 
# places the lexer into the default state.
def t_linestart_string_error(t):
	t.lexer.begin('INITIAL')
	print('(ERROR "' + t.value + ')')
	
	
# Create the lexer.
lexer = lex.lex()

# indent contains the indentation stack. Pushes a 0 onto the stack
lexer.indent = [0]
# create an integer depth counter for tracking grouping symbols to correctly
# create implicit line grouping
lexer.group = 0
# initialize the line beginning position tracking
lexer.lastpos = 0
# The lexer needs to start in the blankstart state, which will compensate for
# initially comments and blank lines. Only applies to the first line.
lexer.begin('blankstart')

# Create one large string from standard input. PLY requires a single string
# and cannot handle streamed input.
in_str = ""
for line in stdin:
	in_str += line

# Run the lexer on the input
lexer.input(in_str)

# Print out all the lexed tokens
for tok in lexer:
	if tok.type in ['NEWLINE', 'INDENT']:
		print("(" + tok.type + ")")	
	elif tok.type == 'DEDENT':
		print(("(DEDENT)\n"*tok.value)[:-1])
	else:
		print("(" + tok.type + " " + str(tok.value) + ")")
# pop any remaining indentation levels off of the indent stack
if len(lexer.indent) > 1:
	print(("(DEDENT)\n"*(len(lexer.indent)-1))[:-1])
# finally, print the EOF token.
print("(ENDMARKER)")

# Ashton Snelgrove
# u0662114
# snelgrov@eng.utah.edu


