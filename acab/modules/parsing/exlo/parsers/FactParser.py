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
from acab.core.parsing.default_keys import OPERATOR, SEN, VALUE, HEAD_ANNOTATION
from acab.core.parsing import default_keys as PDS
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
HOTLOAD_SEN_POSTS

# Controllable words that can't start a sentence
BAD_HEADS           = ~(END | HOTLOAD_BAD_HEADS)("Bad Words")
BAD_HEADS.errmsg    = "Bad Head Word Found"

# Annotations for sentence words, auto wrapped with parens
annotations = PU.DELIMIST(HOTLOAD_ANNOTATIONS, delim=COMMA)
annotations.set_parse_action(PConst.build_constraint_list)
annotations.set_name("Annotations")

# The one default head annotation: negation
sen_head_negation = NEGATION("SenNeg")
sen_head_negation.set_parse_action(lambda x: ValueAnnotation(CDS.NEGATION, True))

# Annotations for before and after a sentence
sen_head_annotations = sen_head_negation | HOTLOAD_SEN_HEADS
sen_post_annotations = HOTLOAD_SEN_POSTS

# Core = a. | b! | $a. | $b!....
# Sentences are /SEN_WORD* (SEN_END | SEN_STATEMENT)/
# TODO explicit Semantic Hint operator?
SEN_MACRO             = pp.Forward()
SEN_HEAD              = BAD_HEADS + NG(PDS.HEAD_ANNOTATION, zrm(sen_head_annotations))
SEN_WORD              = PU.PARAM_CORE(annotations)
SEN_NO_MODAL          = PU.PARAM_CORE(annotations, end=True) + ~COLON
SEN_END               = (HOTLOAD_SEN_ENDS | SEN_NO_MODAL) + NG(PDS.POST_ANNOTATION, zrm(sen_post_annotations))
SEN_WORD.set_name("PBCore")
SEN_NO_MODAL.set_name("PBEnd")

# The Prime Sentence definition:
SENTENCE = SEN_HEAD + NG(SEN, pp.ZeroOrMore(SEN_WORD) + SEN_END)
SENTENCE.set_parse_action(Pfunc.construct_sentence)
SENTENCE.set_name("Sentence")

SEN_PLURAL = PU.DELIMIST(SENTENCE, delim=DELIM)
SEN_PLURAL.set_name("Sentence Plural")

# FIXME sentence plural macro
# SEN_MACRO_BODY     = IndentedBlock(SENTENCE)
# # Statement to specify multiple sub sentences
# SEN_MACRO        <<= PU.STATEMENT_CONSTRUCTOR(pp.Literal("ζ"),
#                                               SEN_MACRO_BODY,
#                                               parse_fn=Pfunc.construct_multi_sentences)

# TODO: Make this a head annotation
op_sentence = FUNC_SYMBOL + SENTENCE

# Naming
# BINDING_CORE.set_name("BindCore")
# BINDING_END.set_name("BindEnd")
# SEN_PLURAL.set_name("SentencePlural")
# HOTLOAD_ANNOTATIONS.set_name("Annotations")
# SEN_STATEMENT.set_name("SentenceStatement")
# QUERY_OP_Internal.set_name("Query_Statements")
# query_or_annotation.set_name("QueryOrAnnotation")

parse_point = SEN_PLURAL

# MAIN PARSER:
def parse_string(in_string):
    """ str -> [[Value]] """
    return parse_point.parse_string(in_string)[:]
