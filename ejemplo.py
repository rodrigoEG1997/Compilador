import ply.lex as lex
import ply.yacc as yacc
import sys

tokens = [
    'HOLA',
    'COMA',
    'QUE',
    'TAL',
]

t_HOLA = r'hola'
t_COMA = r','
t_QUE = r'que'
t_TAL = r'tal'
t_ignore = ' \t\r\n\f\v'

def t_error(t):
    print("Illegal character!")
    t.lexer.skip(1)


lexer = lex.lex()


def p_phrase(p):
    '''
    a : HOLA cont
      | HOLA
    '''
    print('✓✓✓ Valid phrase')

def p_cont(p):
    '''
    cont : COMA QUE TAL
    '''


def p_error(p):
    print('xxx Invalid phrase')


parser = yacc.yacc()





if (len(sys.argv) > 1):
    programName = sys.argv[1]
    programFile = open(programName, "r")
    # This is neccessary because the read method parses literal ends
    #  of lines as \\n instead of \n.
    program = programFile.read().replace('\\n', '\n')
    parser.parse(program)
    programFile.close()  
else:
    raise Exception('''
    No file name was provided.]
    Example: romp.py test.rmop
    ''')