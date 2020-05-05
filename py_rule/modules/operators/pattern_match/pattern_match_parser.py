"""

A Transform operator for within a rule:

match $x            -> $y:
	value           -> value2
	$var            -> $var2
	@var.sub.query? -> subvar
	sumtype         -> ...
	$x(>2)?         -> ...
	_               -> ...
end


"""

import pyparsing as pp

HOTLOAD_VAR = pp.Forward()
HOTLOAD_SEN = pp.Forward()
HOTLOAD_QUERY = pp.Forward()

# Parser

# Actions


pattern_match_stmt = None

parse_point = None

def parseString(the_string):
    return parse_point.parseString(the_string)[:]
