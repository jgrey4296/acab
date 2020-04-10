"""
Agenda Parser

Should define an Agenda Instance with its parameters,
to then be used in a layer

σ::DefaultAgenda:
	select.$x(::Number)
end

σ::RankingAgenda:
	rank.by.$x
end

eg:
Σ::primary.agenda(::DefaultAgenda): select.20 end
Σ::secondary.agenda(::RankingAgenda): rank.by:$x? end

"""
import logging as root_logger
import pyparsing as pp
from py_rule.abstract.parsing import util as PU
from py_rule.abstract.production_operator import ProductionContainer
from py_rule.abstract.agenda import Agenda, make_agenda

logging = root_logger.getLogger(__name__)

def construct_agenda_setup_dict(toks):
    # construct initializer dictionary
    the_dict = {}
    return ("agenda_dict", the_dict)


HOTLOAD_BASIC_SEN = pp.Forward()

agenda_body = None

agenda_stmt = PU.STATEMENT_CONSTRUCTOR(PU.AGENDA_HEAD,
                                       HOTLOAD_BASIC_SEN,
                                       agenda_body,
                                       parse_fn=make_agenda)

parse_point = agenda_stmt

def parseString(s):
    return parse_point.parseString(s)
