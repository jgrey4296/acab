"""

A Transform operator for within a rule:

match $x            -> $y:
	value           -> value2
	$var            -> $var2
	@var.sub.query? -> subvar
	$x(::sumtype)   -> ...
	$x(>2)?         -> ...
	_               -> ...
end


"""

import pyparsing as pp
import py_rule.abstract.parsing.util as PU

HOTLOAD_VALBIND = pp.Forward()
HOTLOAD_VAR = pp.Forward()
HOTLOAD_SEN = pp.Forward()
HOTLOAD_QUERY = pp.Forward()

MATCH_KW = PU.s(pp.Keyword("match"))

# Parser
head = MATCH_KW + HOTLOAD_VAR + PU.ARROW + HOTLOAD_VAR + PU.COLON + PU.s(pp.lineEnd)

pattern = pp.empty()
match_line = pattern + PU.ARROW + HOTLOAD_VALBIND

pattern_match = head + pp.delimitedList(match_line, delim=pp.lineEnd) + PU.END

# Actions


parse_point = pattern_match

def parseString(the_string):
    return parse_point.parseString(the_string)[:]
