#92
import ply.lex as lex
import ply.yacc as yacc
import sys

t_ignore = ' \t\v\r' # whitespace
#start = 'java'

tokens = (
	'ANDAND',
	'COMMA',
	'DIVIDE',
	'ELSE',
	'EQUAL',
	'EQUALEQUAL',
	'FUNCTION',
	'GE',
	'GT',
	'IDENTIFIER',
	'IF',
	'LBRACE',
	'LE',
	'LPAREN',
	'LT',
	'MINUS',
	'NOT',
	'NUMBER',
	'OROR',
	'PLUS',
	'RBRACE',
	'RETURN',
	'RPAREN',
	'SEMICOLON',
	'STRING',
	'TIMES',
	'BOOLVAL',
	'PLUSPLUS',
	'MINUSMINUS',
	'INT',
	'DOUBLE',
	'CHAR',
	'FLOAT',
	'BOOLEAN',
	'VOID',
	'NEW',
	'PUBLIC',
	'PRIVATE',
	'MOD', # %
	'DOT', # .
	'IMPORT',
	'STATIC',
	'FOR',
	'WHILE',
	'WSTRING',
	'SCHAR',
	'CLASS',
	'PRINTNEXTLN',
	'PRINT',
	'LSQB',
	'RSQB',
	'MAIN',
	'ARGS',
	'NOTEQUAL'
	)

precedence = (
		#
	('left', 'OROR'),
	('left', 'ANDAND'),
	('left', 'EQUALEQUAL'),
	('left', 'LT', 'LE', 'GT', 'GE'),
	('left', 'PLUS', 'MINUS'),
	('left', 'TIMES', 'DIVIDE'),
	('right', 'NOT'),
)

t_NOTEQUAL = r'!='
t_ANDAND = r'&&'
t_COMMA = r','
t_DIVIDE = r'/'
t_EQUAL = r'='
t_EQUALEQUAL = r'=='
#'FUNCTION', ########
t_GE = r'>='
t_GT = r'>'
t_IDENTIFIER = r'([_a-zA-Z])[_a-z0-9A-Z]*' 
t_LBRACE = r'\{'
t_LE = r'<='
t_LPAREN = r'\('
t_LT = r'<'
t_MINUS = r'-'
t_NOT = r'!'
t_OROR = r'\|\|'
t_PLUS = r'\+'
t_RBRACE = r'\}'
t_RPAREN = r'\)'
t_SEMICOLON = r';'
#colon - member initializer list
t_TIMES = r'\*'
t_PLUSPLUS = r'\+\+'
t_MINUSMINUS = r'--'
t_MOD = r'%' # %
t_DOT = r'\.'
t_LSQB = r'\['
t_RSQB = r'\]'

states = ( ( 'javaLinecomment', 'exclusive'),  ('javaBigcomment', 'exclusive'), )

def t_javaLinecomment(t):
	r'//'
	t.lexer.begin('javaLinecomment')

def t_javaLinecomment_end(t):
	r'\n'
	t.lexer.lineno +=  t.value.count("\n")
	t.lexer.begin('INITIAL')

def t_javaBigcomment(t):
	r'/\*'
	t.lexer.begin('javaBigcomment')

def t_javaBigcomment_end(t):
	r'\*/'
	t.lexer.lineno +=  t.value.count("\n")
	t.lexer.begin('INITIAL')

t_javaBigcomment_ignore = r'.'
t_javaLinecomment_ignore = r'.'


def t_javaBigcomment_error(t):
	t.lexer.skip(1)

def t_javaLinecomment_error(t):
	t.lexer.skip(1)

def t_newline(t):
	r'\n'
	t.lexer.lineno += 1

def t_error(t):
	print 'ERROR!!! ' + t.value[0]
	t.lexer.skip(1)

def t_WSTRING(t):
	r'"[^"]*"'
	t.value = t.value[1:-1]
	return t

def t_SCHAR(t):
	r"'[^']'"
	return t

def t_NUMBER(t):
	r'-?[0-9]+(\.[0-9]+)?'
	t.value = float(t.value)
	return t

def t_INT(t):
	r'int'
	return t

def t_ELSE(t):
	r'else' 
	return t

def t_DOUBLE(t): 
	r'double' 
	return t

def t_CHAR(t):
	r'char' 
	return t

#t_FLOAT ##problem with java compiler online

def t_BOOLEAN(t):
	r'boolean'
	return t

def t_VOID(t):
	r'void'
	return t

def t_NEW(t):
	r'new' 
	return t

def t_PUBLIC(t):
	r'public' 
	return t

def t_PRIVATE(t):
	r'private' 
	return t

def t_IMPORT(t):
	r'import'
	return t

def t_STATIC(t):
	r'static'
	return t

def t_FOR(t): 
	r'for' 
	return t

def t_WHILE(t):
	r'while' 
	return t

def t_STRING(t): 
	r'String' 
	return t

def t_BOOLVAL(t):
	r'true|false' 
	return t

def t_RETURN(t):
	r'return' 
	return t

def t_IF(t):
	r'if' 
	return t

def t_CLASS(t):
	r'class'
	return t

def t_PRINTNEXTLN(t):
	r'System.out.println'
	return t

def t_PRINT(t):
	r'System.out.print'
	return t
	
def t_MAIN(t):
	r'main'
	return t

def t_ARGS(t):
	r'args'
	return t

## PRINTING TOKEN LEFT
## Either make a whole printing token
## OR BREAK it

## GRAMMAR BEGINS
##############################################
# the parenthesis is linked to non terminals?
# while simple list concat is for terminals?


def p_start(p):
	'java : statement'
	p[0] = p[1]

def p_empty(p):
	'statement : '
	p[0] = []

def p_class_empty(p):
	'classBody : '
	p[0] = []
	
def p_arr_empty(p):
	'arrContent : '
	p[0] = []

# def p_arg_empty(p):
# 	'arguments : '
# 	p[0] = []

def p_statement_dec(p):
	'statement : decs statement'
	p[0] = [p[1]] + p[2]

def p_statement_class(p):
	'statement : classStmt'
	p[0] = [p[1]]

def p_class_body(p):
	'classBody : funcStmt classBody'
	## Initially I wrote p[0] = [p[1]] + p[2] but it showed another list within the tree. Hence changed it. 
	p[0] = [p[1]] + p[2]

# def p_print_statement(p):
# 	'statement : prtStmt'
# 	p[0] = [p[1]]

def p_print_statement(p):
	'statement : prtStmt statement'
	p[0] = [p[1]] + p[2]

# def p_loop_statment(p):
# 	'statement : loop'
# 	p[0] = [p[1]]

def p_loop_statment(p):
	'statement : loop statement'
	p[0] = [p[1]] + p[2]

# def p_if_statement(p):
# 	'statement : ifStat'
# 	p[0] = [p[1]]

def p_if_statement(p):
	'statement : ifStat statement'
	p[0] = [p[1]] + p[2]

def p_if_else_statment(p):
	'statement : ifelseStat statement'
	p[0] = [p[1]] + p[2]

def p_if_elseif_else_statement(p):
	'statement : ifBlock statement'
	p[0] = [p[1]] + p[2]

def p_elif_statement(p):
	'statement : elifBlock statement'
	p[0] = [p[1]] + p[2]

def p_else_statement(p):
	'statement : elseBlock statement'
	p[0] = [p[1]] + p[2]

def p_expression_arr_index(p):
	'arrExpression : IDENTIFIER LSQB expression RSQB'
	p[0] = (p[1],p[3])

def p_expression_iden(p):
	'expression : IDENTIFIER'
	p[0] = ('identifier_exp', p[1])

def p_expression_num(p):
	'expression : NUMBER'
	p[0] = ('num_exp', p[1])

def p_expression_string(p):
	'expression : WSTRING'
	p[0] = ('string_exp', p[1])

def p_expression_char(p):
	'expression : SCHAR'
	p[0] = ('char_exp', p[1])

def p_expression_bool(p):
	'expression : BOOLVAL'
	p[0] = ('bool_exp', p[1])

def p_argument_parsing(p):
	'arguments : arg'
	p[0] = [p[1]]

# def p_multiple_argument_parsing(p):
# 	'arguments : arg COMMA arguments'
# 	p[0] = [p[1]] + p[3]

def p_statement_array_dec(p):
	## Array declaration can also be done using statement_dec. No need of this function. Its excessive. Given the structure of this function and its comparison with statement_dec function we come to know that this func is excess
	'statement : arrdec statement'
	p[0] = [p[1]] + p[2]

def p_arr_content(p):
	'arrContent : element COMMA arrContent'
	p[0] = p[1] + p[3] 

def p_arr_content_last(p):
	'arrContent : element'
	p[0] = p[1]

def p_element(p):
	#if I remove the outer brackets for [p[1]] then the array is stored as tuple. In this case, each element is a list
	'element : expression'
	p[0] = [p[1]]

def p_if_body(p):
	'ifBody : statement'
	p[0] = p[1]

def p_for_loop_body1(p):
	'forLoopBody : statement'
	p[0] = p[1]

def p_for_loop_stmt1(p):
	'loop : FOR LPAREN var_types IDENTIFIER EQUAL expression SEMICOLON IDENTIFIER comp_op expression SEMICOLON expression RPAREN LBRACE forLoopBody RBRACE '
	p[0] = ("For Loop", p[3],p[4],p[6],p[8],p[9],p[10],p[12],("loop's body",p[15]))

def p_for_loop_stmt2(p):
	'loop : FOR LPAREN IDENTIFIER EQUAL expression SEMICOLON IDENTIFIER comp_op expression SEMICOLON expression RPAREN LBRACE forLoopBody RBRACE '
	p[0] = ("For Loop without init", p[3],p[5],p[7],p[8],p[9],p[11],("loop's body",p[14]))

def p_print_stmt(p):
	'prtStmt : PRINTNEXTLN LPAREN expression RPAREN SEMICOLON'
	p[0] = ("print statement", p[3])


def p_arg(p):
	'arg : var_types IDENTIFIER'
	p[0] = ("argument", p[1], p[2]) 


def p_arith_ops(p):
	"""var_types : INT
				| DOUBLE
				| STRING
				| CHAR
				| BOOLEAN"""
	p[0] = ("variable type",p[1])

def p_comparison_ops(p):
	"""comp_op : LE
			   | LT
			   | GE
			   | GT
			   | EQUALEQUAL
			   | NOTEQUAL"""

	p[0] = ("comparison operator", p[1])

def p_if_stat(p):
	'ifStat : IF LPAREN IDENTIFIER comp_op expression RPAREN LBRACE ifBody RBRACE'
	p[0] = ("if statement", p[3],p[4],p[5],("single if's body",p[8]))

def p_if_else_stat(p):
	'ifelseStat : IF LPAREN IDENTIFIER comp_op expression RPAREN LBRACE ifBody RBRACE ELSE LBRACE ifBody RBRACE'
	p[0] = ("if else statement",p[3],p[4],p[5],("if's body",p[8]), ("else's body", p[12]))

def p_if_block(p):
	'ifBlock : IF LPAREN IDENTIFIER comp_op expression RPAREN LBRACE ifBody RBRACE ELSE IF LPAREN IDENTIFIER comp_op expression RPAREN LBRACE ifBody RBRACE'
	p[0] = ("if elif statements", p[3], p[4], p[5],("if's block",p[8]),p[13],p[14],p[15],("first elif block",p[18]))

def p_elif_block(p):
	'elifBlock : ELSE IF LPAREN IDENTIFIER comp_op expression RPAREN LBRACE ifBody RBRACE'
	p[0] = ("if elif statements", p[4], p[5], p[6],("more elif's block",p[9]) )

def p_else_block(p):
	'elseBlock : ELSE LBRACE ifBody RBRACE'
	p[0] = ("elif's else statements", ("else's block",p[3]))

def p_expression_complex(p):
	"""expression : expression PLUS expression
			   | expression MINUS expression
			   | expression TIMES expression
			   | expression DIVIDE expression
			   | expression MOD expression"""
	p[0] = ('complex_exp', p[1],p[2],p[3])

def p_nested_exp(p):
	'expression : LPAREN expression RPAREN'
	p[0] = ('nested_exp',p[2])

def p_expression_special_ops(p): ##### THIS IS WRONG I THINK
	"""expression : IDENTIFIER PLUSPLUS
				  | IDENTIFIER MINUSMINUS"""
	p[0] = ('specialOps_exp', p[1],p[2])


def p_type_dec(p):
	"""decs : INT IDENTIFIER SEMICOLON 
			| CHAR IDENTIFIER SEMICOLON 
			| DOUBLE IDENTIFIER SEMICOLON 
		 	| STRING IDENTIFIER SEMICOLON 
		  	| BOOLEAN IDENTIFIER SEMICOLON"""

	if p[1] == 'int':
		p[0] = ("int declaration",p[1],p[2])
	elif p[1] == 'char':
		p[0] = ("char declaration",p[1],p[2])
	elif p[1] == 'double':
		p[0] = ("double declaration",p[1],p[2])
	elif p[1] == 'String':
		p[0] = ("String declaration",p[1],p[2])
	elif p[1] == 'boolean':
		p[0] = ("boolean declaration",p[1],p[2])


def p_type_dec_assignment(p):
	"""decs : INT IDENTIFIER EQUAL NUMBER SEMICOLON 
			| CHAR IDENTIFIER EQUAL SCHAR SEMICOLON
			| DOUBLE IDENTIFIER EQUAL NUMBER SEMICOLON
			| STRING IDENTIFIER EQUAL WSTRING SEMICOLON
		  	| BOOLEAN IDENTIFIER EQUAL BOOLVAL SEMICOLON"""

	if p[1] == 'int':
		p[0] = ("int declaration & assignment",p[1],p[2],p[3],p[4])
	elif p[1] == 'char':
		p[0] = ("char declaration & assignment",p[1],p[2],p[3],p[4])
	elif p[1] == 'double':
		p[0] = ("double declaration & assignment",p[1],p[2],p[3],p[4])
	elif p[1] == 'String':
		p[0] = ("String declaration & assignment",p[1],p[2],p[3],p[4])
	elif p[1] == 'boolean':
		p[0] = ("boolean declaration & assignment",p[1],p[2],p[3],p[4])

def p_var_assignment(p):
	'decs : IDENTIFIER EQUAL expression SEMICOLON'
	p[0] = ("variable assigned/accessed", p[1], p[2], p[3])

def p_var_assigned_arr_index_value(p):
	'decs : IDENTIFIER EQUAL arrdec SEMICOLON'
	p[0] = ("variable assigned arr index", p[1], p[2], p[3])

def p_var_increment(p):
	"""decs : IDENTIFIER PLUSPLUS SEMICOLON
			 | IDENTIFIER MINUSMINUS SEMICOLON"""
	p[0] = ("variable incre/decre", p[1], p[2])


def p_class_dec(p):
	## Restriction about the naming of class can be added => first character should be capital
	'classStmt : PUBLIC CLASS IDENTIFIER LBRACE classBody RBRACE'
	p[0] = ("main java class", p[1], p[2], p[3], p[5])


def p_main_func_dec(p):
	'funcStmt : PUBLIC STATIC VOID MAIN LPAREN STRING LSQB RSQB ARGS RPAREN LBRACE statement RBRACE'
	p[0] = ("main static func", p[4],p[12])


# def p_other_func_dec(p):
# 	'funcStmt : PUBLIC INT IDENTIFIER LPAREN RPAREN LBRACE statement RBRACE'
# 	p[0] = ("other func", p[2], p[3],p[7])

def p_other_func_dec(p):
	'funcStmt : PUBLIC INT IDENTIFIER LPAREN arguments RPAREN LBRACE statement RBRACE'
	p[0] = ("other func", p[2], p[3],p[5],p[8])



def p_arr_type_dec(p):
	"""arrdec : INT LSQB RSQB IDENTIFIER SEMICOLON
			| CHAR LSQB RSQB IDENTIFIER SEMICOLON
			| DOUBLE LSQB RSQB IDENTIFIER SEMICOLON
			| STRING LSQB RSQB IDENTIFIER SEMICOLON
			| BOOLEAN LSQB RSQB IDENTIFIER SEMICOLON"""

	if p[1] == 'int':
		p[0] = ("int array declaration",p[1],p[4])
	elif p[1] == 'char':
		p[0] = ("char array declaration",p[1],p[4])
	elif p[1] == 'double':
		p[0] = ("double array declaration",p[1],p[4])
	elif p[1] == 'String':
		p[0] = ("String array declaration",p[1],p[4])
	elif p[1] == 'boolean':
		p[0] = ("boolean array declaration",p[1],p[4])

def p_arr_size_type_dec(p):
	"""arrdec : INT LSQB RSQB IDENTIFIER EQUAL NEW INT LSQB NUMBER RSQB SEMICOLON
			| CHAR LSQB RSQB IDENTIFIER EQUAL NEW CHAR LSQB NUMBER RSQB SEMICOLON
			| DOUBLE LSQB RSQB IDENTIFIER EQUAL NEW DOUBLE LSQB NUMBER RSQB SEMICOLON
			| STRING LSQB RSQB IDENTIFIER EQUAL NEW STRING LSQB NUMBER RSQB SEMICOLON
			| BOOLEAN LSQB RSQB IDENTIFIER EQUAL NEW BOOLEAN LSQB NUMBER RSQB SEMICOLON"""

	if p[1] == 'int':
		p[0] = ("int array&size declaration",p[1],p[4],p[5],p[6],p[7],p[9])
	elif p[1] == 'char':
		p[0] = ("char array&size declaration",p[1],p[4],p[5],p[6],p[7],p[9])
	elif p[1] == 'double':
		p[0] = ("double array&size declaration",p[1],p[4],p[5],p[6],p[7],p[9])
	elif p[1] == 'String':
		p[0] = ("String array&size declaration",p[1],p[4],p[5],p[6],p[7],p[9])
	elif p[1] == 'boolean':
		p[0] = ("boolean array&size declaration",p[1],p[4],p[5],p[6],p[7],p[9])

def p_arr_full_dec(p):
	"""arrdec : INT LSQB RSQB IDENTIFIER EQUAL LBRACE arrContent RBRACE SEMICOLON
			  | DOUBLE LSQB RSQB IDENTIFIER EQUAL LBRACE arrContent RBRACE SEMICOLON
			  | CHAR LSQB RSQB IDENTIFIER EQUAL LBRACE arrContent RBRACE SEMICOLON
			  | STRING LSQB RSQB IDENTIFIER EQUAL LBRACE arrContent RBRACE SEMICOLON
			  | BOOLEAN LSQB RSQB IDENTIFIER EQUAL LBRACE arrContent RBRACE SEMICOLON
	"""
	p[0] = ("full array declaration", p[1],p[4],p[7])

def p_arr_index_access(p):
	'arrdec : arrExpression'
	p[0] = ("array indexed", p[1])

def p_arr_index_initialize(p):
	'arrdec : IDENTIFIER LSQB expression RSQB EQUAL expression SEMICOLON'
	p[0] = ("array index initialzed", p[1],p[3],p[6])

def p_error(p):
	print "SYNTAX ERROR : at line no. " + str(lexer.lineno)

##############################################
#interpreter

def eval_exp(tree_branch,environment):
	nodetype = tree_branch[0]

	if nodetype == "complex_exp":
		left_child = tree_branch[1]
		operator = tree_branch[2]
		right_child = tree_branch[3]
		left_val = eval_exp(left_child,environment)
		right_val = eval_exp(right_child,environment)
		if operator == "+":
			return left_val + right_val
		elif operator == "-":
			return left_val - right_val
		elif operator == "*":
			return left_val * right_val
		elif operator == "/":
			return left_val/right_val
		elif operator =="%":
			return left_val%right_val

	elif nodetype == "nested_exp":
		temp_exp = tree_branch[1]
		return eval_exp(temp_exp,environment)

	elif nodetype == "identifier_exp":
		v_name = tree_branch[1]
		v_value = env_lookup(v_name,environment)
		return v_value

	elif nodetype == "num_exp":
		return int(tree_branch[1])

	elif nodetype == "string_exp":
		return tree_branch[1]

	elif nodetype == "char_exp":
		return tree_branch[1]

	elif nodetype == "bool_exp":
		return tree_branch[1]



def eval_stmts(tree_branch,environment):
	stmt_type = tree_branch[0]

	if stmt_type == "int declaration":
		v_name = tree_branch[2]
		make_var(v_name,None,environment)

	elif stmt_type == "String declaration":
		v_name = tree_branch[2]
		make_var(v_name,None,environment)
	elif stmt_type == "double declaration":
		v_name = tree_branch[2]
		make_var(v_name,None,environment)
	elif stmt_type == "char declaration":
		v_name = tree_branch[2]
		make_var(v_name,None,environment)
	elif stmt_type == "boolean declaration":
		v_name = tree_branch[2]
		make_var(v_name,None,environment)
	elif stmt_type == "int declaration & assignment":
		v_name = tree_branch[2]
		v_value = tree_branch[4]
		#print type(v_value)
		if env_lookup(v_name, environment) == None:
			make_var(v_name,int(v_value),environment)
		#print environment

	elif stmt_type == "String declaration & assignment":
		v_name = tree_branch[2]
		v_value = tree_branch[4]
		#print type(v_value)
		if env_lookup(v_name, environment) == None:
			make_var(v_name,v_value,environment)
		#print environment

	elif stmt_type == "double declaration & assignment":
		v_name = tree_branch[2]
		v_value = tree_branch[4]
		#print type(v_value)
		if env_lookup(v_name, environment) == None:
			make_var(v_name,float(v_value),environment)
		#print environment
	elif stmt_type == "boolean declaration & assignment":
		v_name = tree_branch[2]
		v_value = tree_branch[4]
		#print type(v_value)
		if env_lookup(v_name, environment) == None:
			make_var(v_name,v_value,environment)
		#print environment
	elif stmt_type == "char declaration & assignment":
		v_name = tree_branch[2]
		v_value = tree_branch[4]
		#print type(v_value)
		if env_lookup(v_name, environment) == None:
			make_var(v_name,char(v_value),environment)
		#print environment

	elif stmt_type == "int array declaration":
		#arrdec : INT LSQB RSQB IDENTIFIER SEMICOLON
		v_name = tree_branch[2]	
		make_var(v_name,None,environment)	

	elif stmt_type == "char array declaration":
		v_name = tree_branch[2]	
		make_var(v_name,None,environment)	

	elif stmt_type == "double array declaration":
		v_name = tree_branch[2]	
		make_var(v_name,None,environment)	

	elif stmt_type == "String array declaration":
		v_name = tree_branch[2]	
		make_var(v_name,None,environment)	

	elif stmt_type == "boolean array declaration":
		v_name = tree_branch[2]	
		make_var(v_name,None,environment)	

	elif stmt_type == "int array&size declaration":
		#INT LSQB RSQB IDENTIFIER EQUAL NEW INT LSQB NUMBER RSQB SEMICOLON
		#("int array&size declaration",p[1],p[4],p[5],p[6],p[7],p[9])
		v_name = tree_branch[2]
		v_size = int(tree_branch[6])
		v_value = []
		for i in range(0,v_size):
			v_value.append(None)
		make_var(v_name,v_value,environment)
		
	elif stmt_type == "double array&size declaration":
		v_name = tree_branch[2]
		v_size = int(tree_branch[6])
		v_value = []
		for i in range(0,v_size):
			v_value.append(None)
		make_var(v_name,v_value,environment)

	elif stmt_type == "char array&size declaration":
		v_name = tree_branch[2]
		v_size = int(tree_branch[6])
		v_value = []
		for i in range(0,v_size):
			v_value.append(None)
		make_var(v_name,v_value,environment)

	elif stmt_type == "boolean array&size declaration":
		v_name = tree_branch[2]
		v_size = int(tree_branch[6])
		v_value = []
		for i in range(0,v_size):
			v_value.append(None)
		make_var(v_name,v_value,environment)

	elif stmt_type == "String array&size declaration":
		v_name = tree_branch[2]
		v_size = int(tree_branch[6])
		v_value = []
		for i in range(0,v_size):
			v_value.append(None)
		make_var(v_name,v_value,environment)

	elif stmt_type == "full array declaration":
		v_name = tree_branch[2]
		a_cont = tree_branch[3]
		a_size = len(a_cont)
		a_type = tree_branch[1]
		if a_size > 1:
			check = 0
			for e in range(0,a_size-1):
				if type(a_cont[e][1]) == type(a_cont[e+1][1]):
					check = check + 1
			check = check + 1
			if check == a_size:		
				if a_type == 'double':
					a_list = []
					for e in a_cont:
						a_list.append(float(e[1]))
					make_var(v_name,a_list,environment)
				elif a_type == 'int':
					a_list = []
					for e in a_cont:
						a_list.append(int(e[1]))
					make_var(v_name,a_list,environment)
					
				elif a_type == 'boolean':
					a_list = []
					for e in a_cont:
						a_list.append(e[1])
					make_var(v_name,a_list,environment)
				elif a_type == 'String':
					a_list = []
					for e in a_cont:
						a_list.append(str(e[1]))
					make_var(v_name,a_list,environment)
				elif a_type == 'char':
					pass	
		elif a_size == 1:
			if a_type == 'double':
				if type(a_cont[0][1]) == type(1.0):
					make_var(v_name,[float(a_cont[0][1])],environment)
			elif a_type == 'int':
				if type(a_cont[0][1]) == type(1.0):
					make_var(v_name,[int(a_cont[0][1])],environment)
			
			elif a_type == 'boolean':
				if type(a_cont[0][1]) == type(True):
					make_var(v_name,[int(a_cont[0][1])],environment)
			elif a_type == 'String':
				if type(a_cont[0][1]) == type("asad"):
					make_var(v_name,[int(a_cont[0][1])],environment)
			elif a_type == 'char':
					pass	
		
	elif stmt_type == "array indexed":
		index_exp = tree_branch[1]
		v_name = index_exp[0]
		num_index = eval_exp(index_exp[1],environment)
		this_array_size = len(env_lookup(v_name,environment))
		if num_index < this_array_size and num_index >= 0:
			result_exp = return_index(v_name,num_index,environment)
			#print result_exp
		else:
			print "Array index out of range. ABORT"
		

	elif stmt_type == "array index initialzed":
		v_name = tree_branch[1]
		num_index = eval_exp(tree_branch[2],environment)
		this_array_size = len(env_lookup(v_name,environment))
		if num_index < this_array_size and num_index >= 0:
			new_value = eval_exp(tree_branch[3],environment)
			set_index(v_name,num_index,new_value,environment)
		else:
			print "Initializing Array index out of range. ABORT"

	elif stmt_type == "variable assigned/accessed":
		v_name = tree_branch[1]
		#'decs : IDENTIFIER EQUAL expression SEMICOLON'
		#("variable assigned/accessed", p[1], p[2], p[3])

		# do error handling concercing types of variables
		v_exp = tree_branch[3]
		result_exp = eval_exp(v_exp,environment) 
		env_update(v_name,result_exp,environment)

	elif stmt_type == "variable incre/decre":
		#do the error handling just in case variable not declared
		v_name = tree_branch[1]
		v_value = env_lookup(v_name,environment)
		env_update(v_name,v_value+1,environment)

	elif stmt_type == "loop's body":
		loop_stmts = tree_branch[1]
		for l_sts in loop_stmts:
			eval_stmts(l_sts,environment)
		
		#print environment
		#deal statements within loop

	elif stmt_type == "For Loop":
		if tree_branch[1][1] == 'int':
			if tree_branch[2] == tree_branch[4]:
				indexing_op = tree_branch[2]
				indexing_op_initial_value = eval_exp(tree_branch[3],environment)
				indexing_op_final_value = eval_exp(tree_branch[6],environment)
				indexing_op_operation = tree_branch[7][2] ##++
				loops_body = tree_branch[8]
				make_var(indexing_op,indexing_op_initial_value,environment) ##initialize i
				if indexing_op_initial_value<indexing_op_final_value and tree_branch[5][1] == '<' and indexing_op_operation=='++':
					for i in range(indexing_op_initial_value,indexing_op_final_value):
						eval_stmts(tree_branch[8],environment)

						indexing_op_initial_value = indexing_op_initial_value + 1 #i++
						env_update(indexing_op,indexing_op_initial_value,environment) # updating i++

				elif tree_branch[5][1] == '>' and indexing_op_initial_value>indexing_op_final_value and indexing_op_operation=='--':
					for i in range(indexing_op_initial_value,indexing_op_final_value,-1):
						eval_stmts(tree_branch[8],environment)

						indexing_op_initial_value = indexing_op_initial_value - 1 #i--
						env_update(indexing_op,indexing_op_initial_value,environment) # updating i--

				elif tree_branch[5][1] == '>=' and indexing_op_initial_value>=indexing_op_final_value and indexing_op_operation=='--':
					for i in range(indexing_op_initial_value,indexing_op_final_value,-1):
						eval_stmts(tree_branch[8],environment)

						indexing_op_initial_value = indexing_op_initial_value - 1 #i--
						env_update(indexing_op,indexing_op_initial_value,environment) # updating i--

				elif tree_branch[5][1] == '<=' and indexing_op_initial_value<=indexing_op_final_value and indexing_op_operation=='++':
					for i in range(indexing_op_initial_value,indexing_op_final_value):
						eval_stmts(tree_branch[8],environment)

						indexing_op_initial_value = indexing_op_initial_value + 1 #i++
						env_update(indexing_op,indexing_op_initial_value,environment) # updating i++
								
			else:
				print "for loop not yet designed for two different ops"
		else:
			print "Use correct data type of variable"
	
	elif stmt_type == "For Loop without init":
		print "yet to implement. But it is very similar to initialization"

	elif stmt_type == "single if's body":
		if_body_stmts = tree_branch[1]
		for each_stmt in if_body_stmts:
			eval_stmts(each_stmt,environment)
		#print environment

	elif stmt_type == "if statement":
		v_value = env_lookup(tree_branch[1],environment)
		v_type = type(v_value)
		v_comp = eval_exp(tree_branch[3],environment)
		v_op = tree_branch[2][1]
		if v_type == type(1) and v_type==type(v_comp):
			if v_op=='<' and v_value<v_comp:
			 	eval_stmts(tree_branch[4],environment)
			 	#pass
			elif v_op =='>' and v_value>v_comp:
				eval_stmts(tree_branch[4],environment)

			elif v_op =='>=' and v_value>=v_comp:
				eval_stmts(tree_branch[4],environment)

			elif v_op =='<=' and v_value<=v_comp:
				eval_stmts(tree_branch[4],environment)
			
			elif v_op =='==' and v_value==v_comp:
				eval_stmts(tree_branch[4],environment)	
			elif v_op =='!=' and v_value!=v_comp:
				eval_stmts(tree_branch[4],environment)

		elif v_type == type(1.0) and v_type==type(v_comp):
			if v_op=='<' and v_value<v_comp:
			 	eval_stmts(tree_branch[4],environment)
			 	#pass
			elif v_op =='>' and v_value>v_comp:
				eval_stmts(tree_branch[4],environment)

			elif v_op =='>=' and v_value>=v_comp:
				eval_stmts(tree_branch[4],environment)

			elif v_op =='<=' and v_value<=v_comp:
				eval_stmts(tree_branch[4],environment)
			
			elif v_op =='==' and v_value==v_comp:
				eval_stmts(tree_branch[4],environment)	
			elif v_op =='!=' and v_value!=v_comp:
				eval_stmts(tree_branch[4],environment)
		
		elif v_type == type("abc") and v_type==type(v_comp):
			if v_op =='==' and v_value==v_comp:
				eval_stmts(tree_branch[4],environment)	
			elif v_op =='!=' and v_value!=v_comp:
				eval_stmts(tree_branch[4],environment)
			else:
				print "Error: string cant be greater, less, etc..."

		elif v_type == type(True) and v_type==type(v_comp):
			if v_op =='==' and v_value==v_comp:
				eval_stmts(tree_branch[4],environment)	
			elif v_op =='!=' and v_value!=v_comp:
				eval_stmts(tree_branch[4],environment)
			else:
				print "Error: boolean cant be greater, less, etc..."
		else:
			print "Type conflicts while doing comparison"
	
	elif stmt_type == "print statement":
		what_print = tree_branch[1]
		to_print = eval_exp(what_print,environment)
		print to_print

def set_index(vname,ind,value,env):
	if vname in env[1]:
		env[1][vname][ind] = value
	elif not env[0] == None:
		set_index(vname,ind,value,env[0])

def return_index(vname,ind,env):
	if vname in env[1]:
		return env[1][vname][ind]
	elif env[0] == None:
		return None
	else: 
		return return_index(vname,ind,env[0])

def make_var(vname, value, env):
	env[1][vname] = value

def env_update(vname,value,env):
	if vname in env[1]:
		env[1][vname] = value
	elif not env[0] == None:
		env_update(vname,value,env[0])

def env_lookup(vname,env):
	#(parent, dictionary)
	if vname in env[1]:
		return env[1][vname]
	elif env[0] == None:
		return None
	else:
		return env_lookup(vname,env[0])


##############################################
#check = "int d = 10; /*dawdpao\nwdj*/\n\n//asdkoaskdpaosk\nString d = \"654\" "
#check = "public class MyClass{}"

#check = "int x=5;\nx=5+(6*9);"
#check = "x = (5+(5*5))"; #THIS NESTING WORKS
#check = "public class Fib{public static void main(String [] args){}}"


# Command line code for test cases
test_arg = sys.argv[1]
file = open(test_arg,"r")

#file = open("test.txt","r")
check = file.read()

lexer = lex.lex()
#lexer.input(check)
parser = yacc.yacc()
result_tree = parser.parse(check,lexer=lexer)
print "\nTHIS IS THE PARSE TREE FROM java_test.txt:\n"
print result_tree
print "\n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n"

print "THE OUTPUT FROM java_test.txt WILL BE PRINTED HERE:\n"

memory = []
main_func_env = (None,{})

outer_class = result_tree[0]
num_outerClass_funcs = len(outer_class[4])

for i in range(0,num_outerClass_funcs):
	current_func = outer_class[4][i]
	if outer_class[4][i][0] == "main static func":
		num_stmts = len(current_func[2])
		main_func_stmts = current_func[2]
		for j in range(0,num_stmts):
			#eval_stmts(main_func_stmts[j],main_func_env)
			if main_func_stmts[j][0] == "For Loop":
				child_env = (main_func_env,{})
				eval_stmts(main_func_stmts[j],child_env)
			
			elif main_func_stmts[j][00] == "if statement":
				child_env = (main_func_env,{})
				eval_stmts(main_func_stmts[j],child_env)

			else:
				eval_stmts(main_func_stmts[j],main_func_env)


	elif outer_class[4][i][0] == "other func":
		#print current_func
		f_return_type = current_func[1]
		f_name = current_func[2]
		f_argument = current_func[3] # can be only 1
		
		other_func_stmts = current_func[4]
		other_func_env = (None,{})
		## add arguments to environment
		num_other_func_stmts = len(other_func_stmts)
		for j in range(0,num_other_func_stmts):
			eval_stmts(other_func_stmts,other_func_env)

print "\n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
print "\nCHECK THE SCOPE OF THE MAIN FUNCTION TO ENSURE CORRECTNESS\n\nMain Function Scope: => " + str(main_func_env)
#print program_env


#problems fixed related to loops, if statements, printing statement

#other functions are parsed but not yet interpreted
#print statement added and interpreted
#single if statment interpretation done
#correctly interprets arrays, indexes them, initializes each index and whole array also
#interprets declarations of variables, updates them, evaluates simple exp with parenthesis
#multiple args not working for function
#single arg other func parsed correctly
#introduced var types for loops only and defined for loop
#introduced comparison operator to reduce repition
#resolved multiple if-elifs-else and if-elifs are defined. Tree is fine
# if else block with all types of identifiers working fine => can be nested multiple times
#Single if statement defined => single ifs can be nested multiple times
#define p_arr_index_initialize to initialize an array index with a value or expression
#defined few funcs to get value of array_index : expression_arr_index, p_arr_index_access,p_var_assigned_arr_index_value => Only works for y=x[1]; But cannot individually use syntax like x[1];
#full array declaration done. IMP => #if in p_element I remove the outer brackets for [p[1]] then the array is stored as tuple. In this case, each element is a stored in a list	
#Statements can be defined within Main function now
#class with body done (class funcs introduced)| function without body (main func introduced along with other funcs)=> But multiple functions within a class come out to be in a list
# Array declaration can also be done using statement_dec func. No need of array_dec function. Its excessive. Given the structure of this function and its comparison with statement_dec function we come to know that this func is excess.
# THIS IS WRONG funcationality I think => def p_expression_special_ops(p): 
#Nesting done
#WORKING FINE => IMP => removed two upper portion functions "p_statement_dec_assignment" & "p_statement_var_access" because they seemed irrelevant. Therefore, written "decs" in "p_type_dec_assignment", "p_var_assignment" & "p_var_increment"
#added square brackets
#added array declaration func & array+size func (both in upper portion ("[p[1]]" wala portion) & lower protion | assumed we donot need square brackets while returning
#++ -- are implemented within expressions AND separately as well as part of variable assignment
#reassigning done. But int variable can later be reassigned some string, etc. GET THE IDEA because this happens with all other types as well
#all variable declarations & assignments done
