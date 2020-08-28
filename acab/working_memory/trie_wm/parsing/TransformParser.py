""" Trie-based parser for the transform component of rules """
import logging as root_logger
import pyparsing as pp
from acab.abstract.sentence import Sentence
from acab.abstract.transform import TransformComponent
from acab.abstract.transform import Transform, TransformOp
from acab.abstract.parsing import util as PU
from acab.working_memory.trie_wm import util as WMU
from acab.working_memory.trie_wm.parsing.FactParser import VALBIND, BASIC_SEN, PARAM_SEN
from acab.config import AcabConfig

logging = root_logger.getLogger(__name__)

util = AcabConfig.Get()
LEFT_S = util("WorkingMemory.TrieWM", "LEFT_S")
RIGHT_S = util("WorkingMemory.TrieWM", "RIGHT_S")
OPERATOR_S = util("Parsing.Structure", "OPERATOR_S")
TARGET_S = util("WorkingMemory.TrieWM", "TARGET_S")


# Builders:
def build_transform_component(toks):
    params = []
    if LEFT_S in toks:
        params.append(toks[LEFT_S][0])
    params += toks[RIGHT_S][:]

    op = toks[OPERATOR_S][0]
    if isinstance(op, str):
        op = Sentence.build([op])

    rebind = toks[TARGET_S][0]
    filtered_params = [x[0] if len(x) == 1 else x for x in params]

    return TransformComponent(op, filtered_params, rebind=rebind, sugared=LEFT_S in toks)

def build_transform(toks):
    trans = Transform(toks[:])
    return (trans.type, trans)


# Hotloaded Transform Operators
HOTLOAD_TRANS_OP = pp.Forward()
HOTLOAD_TRANS_STATEMENTS = pp.Forward()
HOTLOAD_TRANS_OP.setName("Transform_Op")
HOTLOAD_TRANS_STATEMENTS.setName("Transform_Statement")

rebind = PU.ARROW + VALBIND
rebind.setName("Rebind")

# TODO: extend transform to take partial transforms?
# transform: ( bind op val|bind -> bind)
op_path  = PU.OP_PATH_C(BASIC_SEN)
op_path.setName("OperatorPath")

vals = PU.N(RIGHT_S, PU.zrm(PARAM_SEN))
vals.addCondition(lambda toks: all([isinstance(x, Sentence) for x in toks]))

transform_core = PU.N(OPERATOR_S, op_path) \
    + vals \
    + PU.N(TARGET_S, rebind)

transform_sugar = PU.NG(LEFT_S, PARAM_SEN) \
    + PU.N(OPERATOR_S, HOTLOAD_TRANS_OP) \
    + vals \
    + PU.N(TARGET_S, rebind)

transform_combined = pp.Or([transform_core, HOTLOAD_TRANS_STATEMENTS, transform_sugar])

transforms = pp.delimitedList(transform_combined, delim=PU.DELIM)

transform_statement = PU.STATEMENT_CONSTRUCTOR(PU.TRANSFORM_HEAD,
                                               BASIC_SEN,
                                               transforms + PU.component_gap)

# Actions
transform_core.setParseAction(build_transform_component)
transform_sugar.addParseAction(build_transform_component)
transforms.setParseAction(build_transform)

# NAMING
transform_core.setName("Transform_CORE")
transforms.setName("TransformPlural")
transform_statement.setName("TransformDefinition")

parse_point = transforms

# Main Parser:
def parseString(in_string):
    return parse_point.parseString(in_string)[0][1]
