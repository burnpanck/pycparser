#-----------------------------------------------------------------
# plyparser.py
#
# PLYParser class and other utilites for simplifying programming
# parsers with PLY
#
# Copyright (C) 2008-2015, Eli Bendersky
# License: BSD
#-----------------------------------------------------------------


class Coord(object):
    """ Coordinates of a syntactic element. Consists of:
            - File name
            - Line number
            - (optional) column number, for the Lexer
            - (optional) pair of character indices encompassing all tokens of the node
    """
    __slots__ = ('file', 'line', 'column', 'span', '__weakref__')
    def __init__(self, file, line, column=None, span=None):
        self.file = file
        self.line = line
        self.column = column
        self.span = span

    def __str__(self):
        str = "%s:%s" % (self.file, self.line)
        if self.column: str += ":%s" % self.column
        if self.span: str += " [%d:%d)" % self.span
        return str


class ParseError(Exception): pass


class PLYParser(object):
    def _create_opt_rule(self, rulename):
        """ Given a rule name, creates an optional ply.yacc rule
            for it. The name of the optional rule is
            <rulename>_opt
        """
        optname = rulename + '_opt'

        def optrule(self, p):
            p[0] = p[1]

        optrule.__doc__ = '%s : empty\n| %s' % (optname, rulename)
        optrule.__name__ = 'p_%s' % optname
        setattr(self.__class__, optrule.__name__, optrule)

    def _coord(self, lineno, column=None):
        return Coord(
                file=self.clex.filename,
                line=lineno,
                column=column)

    def _yacccoord(self, yaccprod, tokenidx, spantokidx=None):
        """ Determine coordinates of a production token from within a yacc production rule. """
        return Coord(
            file=self.clex.filename,
            line=yaccprod.lineno(tokenidx),
            span=yaccprod.lexspan(tokenidx if spantokidx is None else spantokidx))

    def _combinecoords(self, coords, mainidx=0):
        main = coords[mainidx]
        spans = [c.span for c in coords if c.span is not None]
        return Coord(
            file=main.file,
            line=main.line,
            span=(min(s[0] for s in spans),max(s[1] for s in spans)) if spans else None)

    def _fullspan(self, coord, yaccprod):
        fullspan = Coord('',0,span=yaccprod.lexspan(0))
        return self._combinecoords([coord,fullspan])

    def _parse_error(self, msg, coord):
        raise ParseError("%s: %s" % (coord, msg))
