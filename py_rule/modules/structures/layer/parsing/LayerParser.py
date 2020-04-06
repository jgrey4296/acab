"""
Layer Parser

Should define a layer as a combination of rule selectors
and a sequence of agendas

eg:
Γ::a.layer:
	a.rule.selector.$x?
	another.rule.selector.$y?

	$x -> primary.agenda
	$y -> secondary.agenda -> tertiary.agenda

end

"""
import logging as root_logger
import pyparsing as pp
from py_rule.abstract.parsing import util as PU
from py_rule.abstract.production_operator import ProductionContainer
from py_rule.abstract.layer import Layer

logging = root_logger.getLogger(__name__)

def construct_layer(toks):
    #Get the layer constructor

    #construct

    #add the body

    return ("layer", the_layer)


HOTLOAD_BASIC_SEN = pp.Forward()
HOTLOAD_QUERY_STMT = pp.Forward()

layer_body = None

layer_stmt = PU.STATEMENT_CONSTRUCTOR(PU.LAYER_HEAD,
                                      HOTLOAD_BASIC_SEN,
                                      layer_body)

layer_body.setParseAction(construct_layer)

parse_point = layer_stmt

def parseString(s):
    return parse_point.parseString(s)
