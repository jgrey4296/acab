"""
A Transform operator for within a rule:

match $x            -> $y:
	$x(== 20)?      -> value2
	$x(== $y)?      -> $var2
	@x.sub.query?   -> $subvar
	$x(::sumtype)?  -> ...
	_               -> ...
end


"""

import pyparsing as pp

from acab.abstract.parsing.consts import ARROW, END, COLON, s
import acab.abstract.parsing.parsers as PU


from acab.abstract.rule.production_abstractions import ProductionComponent

def build_transform_component(toks):
    component = ProductionComponent
    # TODO construct pattern match component
    # Get head, set rebind
    # get body -> [(pattern, $val/transform)]

    # TODO complain if there isn't a catchall at the end
    return component


HOTLOAD_VALBIND = pp.Forward()
HOTLOAD_VAR = pp.Forward()
HOTLOAD_SEN = pp.Forward()
HOTLOAD_QUERY = pp.Forward()

MATCH_KW = s(pp.Keyword("match"))

# Parser
# TODO: catch module alias (eg: λPM.match $x -> ...)?
head = MATCH_KW + HOTLOAD_VAR + ARROW + HOTLOAD_VAR + COLON + s(pp.lineEnd)

pattern = HOTLOAD_QUERY
# TODO: make result a sentence that will place a value in the head's var
match_line = pattern + ARROW + HOTLOAD_VALBIND

pattern_match = head + pp.delimitedList(match_line, delim=pp.lineEnd) + END

# Actions
pattern_match.setParseAction(build_transform_component)

parse_point = pattern_match

def parseString(the_string):
    return parse_point.parseString(the_string)[:]
