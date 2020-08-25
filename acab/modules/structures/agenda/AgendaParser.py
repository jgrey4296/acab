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
from acab.config import AcabConfig

from acab.abstract.parsing import util as PU
from acab.abstract.production_operator import ProductionContainer
from acab.abstract.agenda import Agenda, make_agenda

logging = root_logger.getLogger(__name__)

util = AcabConfig.Get()
QUERY_S = util("Parsing.Structure", "QUERY_S")
TRANSFORM_S = util("Parsing.Structure", "TRANSFORM_S")
ACTION_S = util("Parsing.Structure", "ACTION_S")


HOTLOAD_BASIC_SEN = pp.Forward()
HOTLOAD_QUERY = pp.Forward()
HOTLOAD_TRANSFORM = pp.Forward()
HOTLOAD_ACTION = pp.Forward()


# agenda should be a special case of rule
conditions  = PU.N(QUERY_S , HOTLOAD_QUERY     + PU.gap)
transforms  = PU.N(TRANSFORM_S , HOTLOAD_TRANSFORM + PU.gap)
var_setting = PU.NG(ACTION_S   , HOTLOAD_ACTION    + PU.component_gap)

agenda_body = PU.op(conditions) + PU.op(transforms) + PU.op(var_setting)

agenda_stmt = PU.STATEMENT_CONSTRUCTOR(PU.AGENDA_HEAD,
                                       HOTLOAD_BASIC_SEN,
                                       agenda_body)

agenda_body.setParseAction(make_agenda)


parse_point = agenda_stmt

def parseString(s):
    return parse_point.parseString(s)
