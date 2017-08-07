"""
	A Simple Parser. String -> Rule
"""
import logging as root_logger
import pyparsing as pp
logging = root_logger.getLogger(__name__)

from pyparsing import pyparsing_common as ppc
# Group, Suppress, ParseResults, Forward

# OnlyOnce, , FollowedBy, NotAny, OneOrMore, ZeroOrMore,
# Optional, SkipTo, Combine, Dict

# And, Each, MatchFirst, Or, CharsNotIn, Empty, Keyword,
# CaselessKeyword, Literal, CaselessLiteral,
# NoMatch, QuotedString, Regex, White, Word

#PARSER.setParseAction(lambda toks: toks))
#PARSER.setResultsName('')
#PARSER.parseString('')

s = pp.Suppress
op = pp.Optional
opLn = s(op(pp.LineEnd()))



#ops: ==, !=, <, >, >>=
operators = None

#Action operators: Assert, Retract, modify..
ActionOp = None

#values: string, num, variable
values = None

#VAR
variable = None

#WmeSpec = { x : y }
WmeSpec = None

# field op value ?
Clause = None
# ActionOp [value | wmeSpec]
Action = None


# if [Clause] -> [Actions]
RuleP = None


