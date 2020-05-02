"""
Pipeline Parser

Should define a pipeline as a set of layers.
These will be verified to provide a flow from base inputs to outputs
by the rules they select

eg:
φ::a.pipeline.example:
    init:
    typing.layer


    loop:
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
from py_rule.abstract.pipeline import Pipeline, make_pipeline
from py_rule.util import QUERY_S, TRANSFORM_S, ACTION_S

logging = root_logger.getLogger(__name__)

HOTLOAD_BASIC_SEN = pp.Forward()
HOTLOAD_QUERY = pp.Forward()
HOTLOAD_TRANSFORM = pp.Forward()
HOTLOAD_ACTION = pp.Forward()


# Pipelines should be a special case of rule
conditions  = PU.N(QUERY_S , HOTLOAD_QUERY     + PU.gap)
transforms  = PU.N(TRANSFORM_S , HOTLOAD_TRANSFORM + PU.gap)
var_setting = PU.NG(ACTION_S   , HOTLOAD_ACTION    + PU.component_gap)

pipeline_body = PU.op(conditions) + PU.op(transforms) + PU.op(var_setting)

pipeline_stmt = PU.STATEMENT_CONSTRUCTOR(PU.PIPE_HEAD,
                                         HOTLOAD_BASIC_SEN,
                                         pipeline_body)

pipeline_body.setParseAction(make_pipeline)

parse_point = pipeline_stmt

def parseString(s):
    return parse_point.parseString(s)
