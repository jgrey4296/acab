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

from acab.core.parsing.consts import ARROW, END, COLON, s
import acab.core.parsing.parsers as PU


from acab.core.value.instruction import ProductionComponent

def build_transform_component(toks):
    component = ProductionComponent()
    # TODO construct pattern match component
    # Get head, set rebind
    # get body -> [(pattern, $val/transform)]

    # TODO complain if there isn't a catchall at the end
    return component


HOTLOAD_VALBIND = pp.Forward()
HOTLOAD_VAR     = pp.Forward()
HOTLOAD_SEN     = pp.Forward()
HOTLOAD_QUERY   = pp.Forward()

MATCH_KW = s(pp.Keyword("match"))

# TODO use indented block
# Parser
# TODO: catch module alias (eg: λPM.match $x -> ...)?
head = MATCH_KW + HOTLOAD_VAR + ARROW + HOTLOAD_VAR + COLON + s(pp.line_end)

pattern = HOTLOAD_QUERY
# TODO: make result a sentence that will place a value in the head's var
match_line = pattern + ARROW + HOTLOAD_VALBIND

pattern_match = head + pp.delimited_list(match_line, delim=pp.line_end) + END

# Actions
pattern_match.set_parse_action(build_transform_component)

parse_point = pattern_match

def parse_string(the_string):
    return parse_point.parse_string(the_string)[:]
