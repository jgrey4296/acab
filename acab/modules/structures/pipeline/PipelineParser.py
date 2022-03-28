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
import logging as logmod
import pyparsing as pp

from acab.core.config.config import AcabConfig
from acab.core.parsing import parsers as PU
from acab.core.parsing.consts import PIPE_HEAD
from acab.core.parsing.funcs import make_pipeline
from acab.core.data.instruction import ProductionContainer

logging = logmod.getLogger(__name__)

config        = AcabConfig()
QUERY_S     = config.prepare("Parse.Structure", "QUERY")()
TRANSFORM_S = config.prepare("Parse.Structure", "TRANSFORM")()
ACTION_S    = config.prepare("Parse.Structure", "ACTION")()

HOTLOAD_BASIC_SEN = pp.Forward()
HOTLOAD_QUERY     = pp.Forward()
HOTLOAD_TRANSFORM = pp.Forward()
HOTLOAD_ACTION    = pp.Forward()


# Pipelines should be a special case of rule
conditions  = PU.N(QUERY_S , HOTLOAD_QUERY     + PU.gap)
transforms  = PU.N(TRANSFORM_S , HOTLOAD_TRANSFORM + PU.gap)
var_setting = PU.NG(ACTION_S   , HOTLOAD_ACTION    + PU.component_gap)

pipeline_body = PU.op(conditions) + PU.op(transforms) + PU.op(var_setting)

pipeline_stmt = PU.STATEMENT_CONSTRUCTOR(HOTLOAD_BASIC_SEN,
                                         pipeline_body)

pipeline_body.set_parse_action(make_pipeline)

parse_point = pipeline_stmt

def parse_string(s):
    return parse_point.parse_string(s)
