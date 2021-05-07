"""
Pipeline Parser

Should define a pipeline as a set of layers.
These will be verified to provide a flow from base inputs to outputs
by the rules they select

eg:
a.pipeline.example: (::φ)
	// queries:

	// transforms:

	// actions:

    init:
		import acab.modules.operators
    	typing.layer

	input:
    loop:
		an.output.layer
		an.input.layer
		a.second.layer
		a.third.layer
	output:

	interface:

	exit:

end

Questions:
Where should modules be specified to be imported?
Where should actions be restricted?
Where should cleanup, and pipeline/layer state change be described?
Where to specify api connections?

"""
import logging as root_logger
import pyparsing as pp

from acab.abstract.config.config import AcabConfig
from acab.abstract.parsing import parsers as PU
from acab.abstract.parsing.consts import PIPE_HEAD
from acab.abstract.parsing.funcs import make_pipeline
from acab.abstract.core.production_abstractions import ProductionContainer

logging = root_logger.getLogger(__name__)

config        = AcabConfig.Get()
QUERY_S     = config.value("Parse.Structure", "QUERY")
TRANSFORM_S = config.value("Parse.Structure", "TRANSFORM")
ACTION_S    = config.value("Parse.Structure", "ACTION")

HOTLOAD_BASIC_SEN = pp.Forward()
HOTLOAD_QUERY = pp.Forward()
HOTLOAD_TRANSFORM = pp.Forward()
HOTLOAD_ACTION = pp.Forward()


# Pipelines should be a special case of rule
conditions  = PU.N(QUERY_S , HOTLOAD_QUERY     + PU.gap)
transforms  = PU.N(TRANSFORM_S , HOTLOAD_TRANSFORM + PU.gap)
var_setting = PU.NG(ACTION_S   , HOTLOAD_ACTION    + PU.component_gap)

pipeline_body = PU.op(conditions) + PU.op(transforms) + PU.op(var_setting)

pipeline_stmt = PU.STATEMENT_CONSTRUCTOR(PIPE_HEAD,
                                         HOTLOAD_BASIC_SEN,
                                         pipeline_body)

pipeline_body.setParseAction(make_pipeline)

parse_point = pipeline_stmt

def parseString(s):
    return parse_point.parseString(s)
