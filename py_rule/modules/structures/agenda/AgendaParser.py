"""
Agenda Parser

Should define an Agenda Instance with its parameters,
to then be used in a layer

eg:
Σ::primary.agenda(::DefaultAgenda): select.20 end
Σ::secondary.agenda(::RankingAgenda): rank.by:$x? end

"""
import logging as root_logger
import pyparsing as pp
from py_rule.abstract.parsing import util as PU
from py_rule.abstract.production_operator import ProductionContainer
from py_rule.abstract.agenda import Agenda

logging = root_logger.getLogger(__name__)

def construct_agenda(toks):
    #Get the agenda constructor

    #construct

    #add the body

    return ("agenda", the_agenda)


HOTLOAD_BASIC_SEN = pp.Forward()

agenda_body = None

agenda_stmt = PU.STATEMENT_CONSTRUCTOR(PU.AGENDA_HEAD,
                                       HOTLOAD_BASIC_SEN,
                                       agenda_body)

parse_point = agenda_stmt

def parseString(s):
    return parse_point.parseString(s)
