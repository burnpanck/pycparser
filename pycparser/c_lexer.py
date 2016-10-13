# pycparser: c_lexer.py
#
# CLexer class: lexer for the C language
#
# Copyright (C) 2008-2011, Eli Bendersky
# License: BSD
#-----------------------------------------------------------------

import re
import sys

from .ply import lex
from .ply.lex import TOKEN


class CLexer(object):
    """ A lexer for the C language. After building it, set the
        input text with input(), and call token() to get new 
        tokens.
        
        The public attribute filename can be set to an initial
        filaneme, but the lexer will update it upon #line 
        directives.
    """
    def __init__(self, error_func, type_lookup_func, keep_comment=False):
        """ Create a new Lexer.
        
            error_func:
                An error function. Will be called with an error
                message, line and column as arguments, in case of 
                an error during lexing.
                
            type_lookup_func:
                A type lookup function. Given a string, it must
                return True IFF this string is a name of a type
                that was defined with a typedef earlier.

            keep_comment:
                When True,we gather all chunks of comments in
                commentDir. Key is the name of the file processed,
                value is the list of comments found in this file.
                Each list element is a 4-tuple:
                  [0]: token number of the lexer
                  [1]: 2-tuple (line, col) of first comment char
                  [2]: 2-tuple (line, col) of first non-comment char
                  [3]: comment string
        """
        self.error_func = error_func
        self.type_lookup_func = type_lookup_func

        if keep_comment:
            # For each filename we keep a list of comment tokens.
            # These lists are kept in the dictionary commentDir with
            # the filename as key. 
            # _filename is the current filename which content we are
            # currently parsing. 
            # tokenCount is a counter which is incremented for each
            # token parsed and it is also attached to each comment
            # block parsed. This might help to correlate AST nodes
            # with comment blocks, when AST nodes also include the
            # tokenCount value. But it might be sufficient to keep
            # the line/colno for this purpose, since this is easier
            # to use on files viewed in an editor.
            self.commentDir = {}
        else:
            self.commentDir= None

        self.tokenCount= 0
        self._filename = ''
        
        # Allow either "# line" or "# <num>" to support GCC's
        # cpp output
        #
        self.line_pattern = re.compile('([ \t]*line\W)|([ \t]*\d+)')

        self.pragma_pattern = re.compile('[ \t]*pragma\W')

        # There are further things coming through, which we ignore
        # for now. It can also be set to None.
        self.cpp_ignore = re.compile(r'[ \t]*(pragma|ident)[^\n]*\n')
        self.cpp_ignore = None

    @property
    def filename(self):
        return self._filename

    @filename.setter
    def filename(self, name):
        self._filename = name
        if self.commentDir == None:
            return
        cl = self.commentDir.get(name, None)
        if cl == None:
            self._commentList = self.commentDir[name]= []
        else:
            self._commentList = cl

    def build(self, **kwargs):
        """ Builds the lexer from the specification. Must be
            called after the lexer object is created. 
            
            This method exists separately, because the PLY
            manual warns against calling lex.lex inside
            __init__
        """
        self.lexer = lex.lex(object=self, **kwargs)

    def reset_lineno(self):
        """ Resets the internal line number counter of the lexer.
        """
        self.lexer.lineno = 1

    def reset_tokno(self):
        self.tokenCount= 0

    def input(self, text):
        self.lexer.input(text)
    
    def token(self):
        g = self.lexer.token()
        if g:
            tc= self.tokenCount
            self.tokenCount= tc+1
            
            # this was meant as a mechanism to correlate tokens to
            # chunks of C comment. But this proved not to be very
            # useful. I leave it here as a reminder to this idea.
            #                                           ElB, 2012-11-10.
            #setattr(g, 'tkno', tc)
            #g.value= (tc, g.value)
        return g

    def _find_col_pos(self, pos):
        """Column number of pos relative to previous '\n' character.

        When there is no previous '\n' we pretend to have a '\n' just
        before position 0.  Since column numbers start at 1, pos+1 is
        the result then.
        """
        nl= self.lexer.lexdata.rfind('\n', 0, pos)
        if nl < 0:
            return pos+1
        else:
            return pos - nl 

    def find_tok_column(self, token):
        """ Find the column of the token in its line.
        """
        return self._find_col_pos(token.lexpos)

    ######################--   PRIVATE   --######################
    
    ##
    ## Internal auxiliary methods
    ##
    def _error(self, msg, token):
        location = self._make_tok_location(token)
        self.error_func(msg, location[0], location[1])
        self.lexer.skip(1)
    
    def _make_tok_pyloc(self, token):
        "Return pythonic location of token."
        return (token.lineno-1, self.find_tok_column(token)-1)
    
    def _make_tok_location(self, token):
        return (token.lineno, self.find_tok_column(token))
    
    ##
    ## Reserved keywords
    ##
    keywords = (
        '_BOOL', '_COMPLEX', 'AUTO', 'BREAK', 'CASE', 'CHAR', 'CONST',
        'CONTINUE', 'DEFAULT', 'DO', 'DOUBLE', 'ELSE', 'ENUM', 'EXTERN',
        'FLOAT', 'FOR', 'GOTO', 'IF', 'INLINE', 'INT', 'LONG', 'REGISTER',
        'RESTRICT', 'RETURN', 'SHORT', 'SIGNED', 'SIZEOF', 'STATIC', 'STRUCT',
        'SWITCH', 'TYPEDEF', 'UNION', 'UNSIGNED', 'VOID',
        'VOLATILE', 'WHILE',
    )

    keyword_map = {}
    for keyword in keywords:
        if keyword == '_BOOL':
            keyword_map['_Bool'] = keyword
        elif keyword == '_COMPLEX':
            keyword_map['_Complex'] = keyword
        else:
            keyword_map[keyword.lower()] = keyword

    ##
    ## All the tokens recognized by the lexer
    ##
    tokens = keywords + (
        # Identifiers
        'ID', 
        
        # Type identifiers (identifiers previously defined as 
        # types with typedef)
        'TYPEID',
        
        # constants 
        'INT_CONST_DEC', 'INT_CONST_OCT', 'INT_CONST_HEX',
        'FLOAT_CONST', 'HEX_FLOAT_CONST',
        'CHAR_CONST',
        'WCHAR_CONST',
        
        # String literals
        'STRING_LITERAL',
        'WSTRING_LITERAL',

        # Operators 
        'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'MOD',
        'OR', 'AND', 'NOT', 'XOR', 'LSHIFT', 'RSHIFT',
        'LOR', 'LAND', 'LNOT',
        'LT', 'LE', 'GT', 'GE', 'EQ', 'NE',
        
        # Assignment
        'EQUALS', 'TIMESEQUAL', 'DIVEQUAL', 'MODEQUAL', 
        'PLUSEQUAL', 'MINUSEQUAL',
        'LSHIFTEQUAL','RSHIFTEQUAL', 'ANDEQUAL', 'XOREQUAL', 
        'OREQUAL',

        # Increment/decrement 
        'PLUSPLUS', 'MINUSMINUS',

        # Structure dereference (->)
        'ARROW',

        # Conditional operator (?)
        'CONDOP',
        
        # Delimeters 
        'LPAREN', 'RPAREN',         # ( )
        'LBRACKET', 'RBRACKET',     # [ ]
        'LBRACE', 'RBRACE',         # { } 
        'COMMA', 'PERIOD',          # . ,
        'SEMI', 'COLON',            # ; :

        # Ellipsis (...)
        'ELLIPSIS',
        
        # pre-processor 
        'PPHASH',      # '#'
    )

    ##
    ## Regexes for use in tokens
    ##
    ##

    # valid C identifiers (K&R2: A.2.3)
    identifier = r'[a-zA-Z_][0-9a-zA-Z_]*'

    hex_prefix = '0[xX]'
    hex_digits = '[0-9a-fA-F]+'

    # integer constants (K&R2: A.2.5.1)
    integer_suffix_opt = r'(u?ll|U?LL|([uU][lL])|([lL][uU])|[uU]|[lL])?'
    decimal_constant = '(0'+integer_suffix_opt+')|([1-9][0-9]*'+integer_suffix_opt+')'
    octal_constant = '0[0-7]*'+integer_suffix_opt
    hex_constant = hex_prefix+hex_digits+integer_suffix_opt
    
    bad_octal_constant = '0[0-7]*[89]'

    # character constants (K&R2: A.2.5.2)
    # Note: a-zA-Z and '.-~^_!=&;,' are allowed as escape chars to support #line
    # directives with Windows paths as filenames (..\..\dir\file)
    # For the same reason, decimal_escape allows all digit sequences. We want to
    # parse all correct code, even if it means to sometimes parse incorrect
    # code.
    #
    simple_escape = r"""([a-zA-Z._~!=&\^\-\\?'"])"""
    decimal_escape = r"""(\d+)"""
    hex_escape = r"""(x[0-9a-fA-F]+)"""
    bad_escape = r"""([\\][^a-zA-Z._~^!=&\^\-\\?'"x0-7])"""

    escape_sequence = r"""(\\("""+simple_escape+'|'+decimal_escape+'|'+hex_escape+'))'
    cconst_char = r"""([^'\\\n]|"""+escape_sequence+')'    
    char_const = "'"+cconst_char+"'"
    wchar_const = 'L'+char_const
    unmatched_quote = "('"+cconst_char+"*\\n)|('"+cconst_char+"*$)"
    bad_char_const = r"""('"""+cconst_char+"""[^'\n]+')|('')|('"""+bad_escape+r"""[^'\n]*')"""

    # string literals (K&R2: A.2.6)
    string_char = r"""([^"\\\n]|"""+escape_sequence+')'    
    string_literal = '"'+string_char+'*"'
    wstring_literal = 'L'+string_literal
    bad_string_literal = '"'+string_char+'*'+bad_escape+string_char+'*"'

    # floating constants (K&R2: A.2.5.3)
    exponent_part = r"""([eE][-+]?[0-9]+)"""
    fractional_constant = r"""([0-9]*\.[0-9]+)|([0-9]+\.)"""
    floating_constant = '(((('+fractional_constant+')'+exponent_part+'?)|([0-9]+'+exponent_part+'))[FfLl]?)'
    binary_exponent_part = r'''([pP][+-]?[0-9]+)'''
    hex_fractional_constant = '((('+hex_digits+r""")?\."""+hex_digits+')|('+hex_digits+r"""\.))"""
    hex_floating_constant = '('+hex_prefix+'('+hex_digits+'|'+hex_fractional_constant+')'+binary_exponent_part+'[FfLl]?)'

    ##
    ## Lexer states: used for preprocessor \n-terminated directives
    ##
    states = (
        # ppline: preprocessor line directives
        # 
        ('ppline', 'exclusive'),

        # pppragma: pragma
        #
        ('pppragma', 'exclusive'),
    )
    
    def t_PPHASH(self, t):
        r'[ \t]*\#'
        if self.line_pattern.match(t.lexer.lexdata, pos=t.lexer.lexpos):
            t.lexer.begin('ppline')
            self.pp_line = self.pp_filename = None
        elif self.pragma_pattern.match(t.lexer.lexdata, pos=t.lexer.lexpos):
            t.lexer.begin('pppragma')
        else:
            # Look for CPP patterns to ignore
            #~ print t.lexer.lexdata[t.lexer.lexpos:t.lexer.lexpos+90]
            if self.cpp_ignore != None:
                m = self.cpp_ignore.match(
                                t.lexer.lexdata, pos=t.lexer.lexpos)
                if m:
                    s, e = m.span()
                    t.lexer.lexpos += e - s
                    return

            lpos = t.lexer.lexpos
            print "Unknown CPP:", t.lexer.lexdata[lpos:lpos+20]
            t.type = 'PPHASH'
            return t
    
    ##
    ## Rules for the ppline state
    ##
    @TOKEN(string_literal)
    def t_ppline_FILENAME(self, t):
        if self.pp_line is None:
            self._error('filename before line number in #line', t)
        else:
            self.pp_filename = t.value.lstrip('"').rstrip('"')

    @TOKEN(decimal_constant)
    def t_ppline_LINE_NUMBER(self, t):
        if self.pp_line is None:
            self.pp_line = t.value
        else:
            # Ignore: GCC's cpp sometimes inserts a numeric flag
            # after the file name
            pass

    def t_ppline_NEWLINE(self, t):
        r'\n'
        
        if self.pp_line is None:
            self._error('line number missing in #line', t)
        else:
            self.lexer.lineno = int(self.pp_line)
            
            if self.pp_filename is not None:
                self.filename = self.pp_filename
                
        t.lexer.begin('INITIAL')

    def t_ppline_PPLINE(self, t):
        r'line'
        pass

    t_ppline_ignore = ' \t'

    def t_ppline_error(self, t):
        self._error('invalid #line directive', t)

    ##
    ## Rules for the pppragma state
    ##
    def t_pppragma_NEWLINE(self, t):
        r'\n'
        t.lexer.lineno += 1
        t.lexer.begin('INITIAL')

    def t_pppragma_PPPRAGMA(self, t):
        r'pragma'
        pass
        
    t_pppragma_ignore = ' \t<>.-{}();+-*/$%@&^~!?:,0123456789'

    @TOKEN(string_literal)
    def t_pppragma_STR(self, t): pass

    @TOKEN(identifier)
    def t_pppragma_ID(self, t): pass

    def t_pppragma_error(self, t):
        self._error('invalid #pragma directive', t)

    ##
    ## Start gathering comment tokens
    ##  Comments are gathered in a list of 4-tuples:
    ##    [0]: token-number
    ##    [1]: 2-tuple of lno,col describing the start of the comment
    ##    [2]: 2-tuple of lno,col describing the end of the comment
    ##    [3]: content of the comment
    ##  The col of the 'end of comment' is the index of the character
    ##  following the comment.
    ##  I.e. the lno,col tuples have pythonic meaning and the first
    ##  value is 0 not 1.
    ##
    def t_COMMENT(self, t):
        r"/[*/]"
        if self.commentDir == None:
            self.tokenCount+= 1
            return
        lno, col = self._make_tok_pyloc(t)
        lex = t.lexer
        #~ print "tlxp=%d, lxp=%d, lno=%d, col=%d" %(t.lexpos, lex.lexpos, lno, col)
        if t.value[-1] == '*':
            # Block comment
            ecPos = lex.lexdata.find('*/', lex.lexpos)
            echl = 2
        else:
            # One line comment 
            ecPos = lex.lexdata.find('\n', lex.lexpos)
            echl = 1

        if ecPos < 0:
            # Report error of unfinished comment
            lex.lexpos = len(lex.lexdata)
            ecPos = len(lexData)
        else:
            ecPos += echl
            lex.lexpos = ecPos

        cmData = lex.lexdata[t.lexpos:ecPos]
        nlc = cmData.count('\n')
        lex.lineno += nlc
        tc = self.tokenCount
        self._commentList.append((
            tc,
            (lno, col),
            (lno+nlc, self._find_col_pos(ecPos)-1),
            cmData,
        ))
        self.tokenCount = tc+1
        #~ print self._commentList[-1]

    ##
    ## Rules for the normal state
    ##
    t_ignore = ' \t'

    # Newlines
    def t_NEWLINE(self, t):
        r'\n+'
        t.lexer.lineno += t.value.count("\n")

    # Operators
    t_PLUS              = r'\+'
    t_MINUS             = r'-'
    t_TIMES             = r'\*'
    t_DIVIDE            = r'/'
    t_MOD               = r'%'
    t_OR                = r'\|'
    t_AND               = r'&'
    t_NOT               = r'~'
    t_XOR               = r'\^'
    t_LSHIFT            = r'<<'
    t_RSHIFT            = r'>>'
    t_LOR               = r'\|\|'
    t_LAND              = r'&&'
    t_LNOT              = r'!'
    t_LT                = r'<'
    t_GT                = r'>'
    t_LE                = r'<='
    t_GE                = r'>='
    t_EQ                = r'=='
    t_NE                = r'!='

    # Assignment operators
    t_EQUALS            = r'='
    t_TIMESEQUAL        = r'\*='
    t_DIVEQUAL          = r'/='
    t_MODEQUAL          = r'%='
    t_PLUSEQUAL         = r'\+='
    t_MINUSEQUAL        = r'-='
    t_LSHIFTEQUAL       = r'<<='
    t_RSHIFTEQUAL       = r'>>='
    t_ANDEQUAL          = r'&='
    t_OREQUAL           = r'\|='
    t_XOREQUAL          = r'\^='

    # Increment/decrement
    t_PLUSPLUS          = r'\+\+'
    t_MINUSMINUS        = r'--'

    # ->
    t_ARROW             = r'->'

    # ?
    t_CONDOP            = r'\?'

    # Delimeters
    t_LPAREN            = r'\('
    t_RPAREN            = r'\)'
    t_LBRACKET          = r'\['
    t_RBRACKET          = r'\]'
    t_LBRACE            = r'\{'
    t_RBRACE            = r'\}'
    t_COMMA             = r','
    t_PERIOD            = r'\.'
    t_SEMI              = r';'
    t_COLON             = r':'
    t_ELLIPSIS          = r'\.\.\.'

    t_STRING_LITERAL    = string_literal
    
    # The following floating and integer constants are defined as 
    # functions to impose a strict order (otherwise, decimal
    # is placed before the others because its regex is longer,
    # and this is bad)
    #
    @TOKEN(floating_constant)
    def t_FLOAT_CONST(self, t):
        return t

    @TOKEN(hex_floating_constant)
    def t_HEX_FLOAT_CONST(self, t):
        return t

    @TOKEN(hex_constant)
    def t_INT_CONST_HEX(self, t):
        return t

    @TOKEN(bad_octal_constant)
    def t_BAD_CONST_OCT(self, t):
        msg = "Invalid octal constant"
        self._error(msg, t)

    @TOKEN(octal_constant)
    def t_INT_CONST_OCT(self, t):
        return t

    @TOKEN(decimal_constant)
    def t_INT_CONST_DEC(self, t):
        return t

    # Must come before bad_char_const, to prevent it from 
    # catching valid char constants as invalid
    # 
    @TOKEN(char_const)
    def t_CHAR_CONST(self, t):
        return t
        
    @TOKEN(wchar_const)
    def t_WCHAR_CONST(self, t):
        return t
    
    @TOKEN(unmatched_quote)
    def t_UNMATCHED_QUOTE(self, t):
        msg = "Unmatched '"
        self._error(msg, t)

    @TOKEN(bad_char_const)
    def t_BAD_CHAR_CONST(self, t):
        msg = "Invalid char constant %s" % t.value
        self._error(msg, t)

    @TOKEN(wstring_literal)
    def t_WSTRING_LITERAL(self, t):
        return t
    
    # unmatched string literals are caught by the preprocessor
    
    @TOKEN(bad_string_literal)
    def t_BAD_STRING_LITERAL(self, t):
        msg = "String contains invalid escape code" 
        self._error(msg, t)

    @TOKEN(identifier)
    def t_ID(self, t):
        t.type = self.keyword_map.get(t.value, "ID")
        if t.type == 'ID' and self.type_lookup_func(t.value):
            t.type = "TYPEID"
        return t
    
    def t_error(self, t):
        msg = 'Illegal character %s' % repr(t.value[0])
        self._error(msg, t)


if __name__ == "__main__":
    filename = '../zp.c'
    text = open(filename).read()
    
    #~ text = '"'+r"""ka \p ka"""+'"'
    text = r"""
    546
        #line 66 "kwas\df.h" 
        id 4
        # 5 
        dsf
    """
    
    def errfoo(msg, a, b):
        sys.write(msg + "\n")
        sys.exit()
    
    def typelookup(namd):
        return False
    
    clex = CLexer(errfoo, typelookup)
    clex.build()
    clex.input(text)
    
    while 1:
        tok = clex.token()
        if not tok: break
            
        printme([tok.value, tok.type, tok.lineno, clex.filename, tok.lexpos])
