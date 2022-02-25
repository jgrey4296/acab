"""
Pyparsing based parser to turn strings into [FactNode],
capable of parsing  multiple facts
"""
import logging as root_logger

import pyparsing as pp
from acab.core.parsing import funcs as Pfunc
from acab.core.parsing import parsers as PU
from acab.core.parsing.consts import (COMMA, DELIM, END, emptyLine, COLON,
                                      FACT_HEAD, NEGATION, NG, N, op, opLn, zrm, ln, FUNC_SYMBOL)
from acab.core.parsing.default_keys import OPERATOR, SEN, VALUE
from acab.core.data import default_structure as CDS
from acab.modules.parsing.exlo import constructors as PConst
from acab.core.parsing.indented_block import IndentedBlock
from acab.core.parsing.annotation import ValueRepeatAnnotation, ValueAnnotation

logging             = root_logger.getLogger(__name__)
# Hotload insertion points:
HOTLOAD_ANNOTATIONS   = pp.Forward()
HOTLOAD_BAD_HEADS     = pp.Forward()
HOTLOAD_SEN_ENDS      = pp.Forward()
HOTLOAD_SEN_HEADS     = pp.Forward()


BAD_HEADS           = ~(END | HOTLOAD_BAD_HEADS)("Bad Words")
BAD_HEADS.errmsg    = "Bad Head Word Found"

annotations = PU.DELIMIST(HOTLOAD_ANNOTATIONS, delim=COMMA)
annotations.setParseAction(PConst.build_constraint_list)
annotations.setName("Annotations")

sen_head_negation = NEGATION("SenNeg")
sen_head_negation.setParseAction(lambda x: ValueAnnotation(CDS.NEGATION, True))

# Core = a. | b! | $a. | $b!....
# Sentences are /SEN_WORD* (SEN_END | SEN_STATEMENT)/
# TODO explicit Semantic Hint operator?
SEN_MACRO             = pp.Forward()
SEN_HEAD              = BAD_HEADS + (PU.op(sen_head_negation) | HOTLOAD_SEN_HEADS)
SEN_WORD              = PU.PARAM_CORE(annotations)
SEN_NO_MODAL          = PU.PARAM_CORE(annotations, end=True) + ~COLON
SEN_END               = HOTLOAD_SEN_ENDS | SEN_NO_MODAL
SEN_WORD.setName("PBCore")
SEN_NO_MODAL.setName("PBEnd")

# The Prime Sentence definition:
SENTENCE = SEN_HEAD + NG(SEN, pp.ZeroOrMore(SEN_WORD) + SEN_END)
SENTENCE.setParseAction(Pfunc.construct_sentence)
SENTENCE.setName("Sentence")

SEN_PLURAL = PU.DELIMIST(SENTENCE, delim=DELIM)
SEN_PLURAL.setName("Sentence Plural")

# FIXME sentence plural macro
# SEN_MACRO_BODY     = IndentedBlock(SENTENCE)
# # Statement to specify multiple sub sentences
# SEN_MACRO        <<= PU.STATEMENT_CONSTRUCTOR(pp.Literal("ζ"),
#                                               SEN_MACRO_BODY,
#                                               parse_fn=Pfunc.construct_multi_sentences)

op_sentence = FUNC_SYMBOL + SENTENCE

# Naming
# BINDING_CORE.setName("BindCore")
# BINDING_END.setName("BindEnd")
# SEN_PLURAL.setName("SentencePlural")
# HOTLOAD_ANNOTATIONS.setName("Annotations")
# SEN_STATEMENT.setName("SentenceStatement")
# QUERY_OP_Internal.setName("Query_Statements")
# query_or_annotation.setName("QueryOrAnnotation")

parse_point = SEN_PLURAL

# MAIN PARSER:
def parseString(in_string):
    """ str -> [[Value]] """
    return parse_point.parseString(in_string)[:]
