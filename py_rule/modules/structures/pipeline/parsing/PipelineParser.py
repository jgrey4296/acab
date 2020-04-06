"""
Pipeline Parser

Should define a pipline as a set of layers.
These will be verified to provide a flow from base inputs to outputs
by the rules they select

eg:
φ::a.pipeline.example:
	an.output.layer
	an.input.layer
	a.second.layer
	a.third.layer

end

Questions:
Where should modules be specified to be imported?
Where should actions be restricted?
Where should cleanup, and pipeline/layer state change be described?
Where to specify api connections?

"""
import logging as root_logger
import pyparsing as pp
from py_rule.abstract.parsing import util as PU
from py_rule.abstract.production_operator import ProductionContainer
from py_rule.abstract.pipeline import Pipeline

logging = root_logger.getLogger(__name__)

def construct_pipeline(toks):
    #Get the pipeline constructor

    #construct

    #add the body

    return ("pipeline", the_pipeline)


HOTLOAD_BASIC_SEN = pp.Forward()

pipeline_body = pp.delimitedList(HOTLOAD_BASIC_SEN, delim=PU.delim)

pipeline_stmt = PU.STATEMENT_CONSTRUCTOR(PU.PIPELINE_HEAD,
                                         HOTLOAD_BASIC_SEN,
                                         pipeline_body)

pipeline_body.setParseAction(construct_pipeline)

parse_point = pipeline_stmt

def parseString(s):
    return parse_point.parseString(s)
