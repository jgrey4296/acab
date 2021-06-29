"""
Agenda Parser

Should define an Agenda Instance with its parameters,
to then be used in a layer
-------------------
Number: (::σ)
DefaultAgenda: (::σ)
  selection.amount: $x(::Number)

primary.agenda: (::DefaultAgenda)
  | $proposals |

  # Query part:
  @proposals.data.
  some.amount!$x(::Number)?
  select($proposals, $x) -> $y

  return($y)


secondary.agenda(::RankingAgenda):
  | $proposals |

  # Use an already bound var from the rules
  dfs($proposals, $_(::Number)?) -> $x
  curve.sine($x) -> $z
  rank($proposals, $z) -> $q
  select($q, 5) -> $h

  return($h)
"""
import logging as root_logger
import pyparsing as pp
from acab.abstract.config.config import AcabConfig

from acab.abstract.parsing import parsers as PU
from acab.abstract.parsing.consts import N, NG, AGENDA_HEAD
from acab.abstract.parsing.funcs import make_agenda

logging = root_logger.getLogger(__name__)

config        = AcabConfig.Get()
QUERY_S     = config.prepare("Parse.Structure", "QUERY")()
TRANSFORM_S = config.prepare("Parse.Structure", "TRANSFORM")()
ACTION_S    = config.prepare("Parse.Structure", "ACTION")()


HOTLOAD_BASIC_SEN = pp.Forward()
HOTLOAD_QUERY = pp.Forward()
HOTLOAD_TRANSFORM = pp.Forward()
HOTLOAD_ACTION = pp.Forward()


# agenda should be a special case of rule
conditions  = N(QUERY_S , HOTLOAD_QUERY + PU.gap)
transforms  = N(TRANSFORM_S , HOTLOAD_TRANSFORM + PU.gap)
var_setting = NG(ACTION_S   , HOTLOAD_ACTION    + PU.component_gap)

agenda_body = PU.op(conditions) + PU.op(transforms) + PU.op(var_setting)

agenda_stmt = PU.STATEMENT_CONSTRUCTOR(AGENDA_HEAD,
                                       HOTLOAD_BASIC_SEN,
                                       agenda_body)

agenda_body.setParseAction(make_agenda)


parse_point = agenda_stmt

def parseString(s):
    return parse_point.parseString(s)
