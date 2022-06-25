"""
Pyparsing based parser to turn strings into [FactNode],
capable of parsing  multiple facts
"""
import logging as logmod

import acab.core.defaults.value_keys as CDS
import pyparsing as pp
from acab.core.defaults import parse_keys as PDS
from acab.core.defaults import parse_symbols as PDSym
from acab.core.defaults.parse_keys import HEAD_ANNOTATION, OPERATOR, SEN, VALUE
from acab.core.parsing import funcs as Pfunc
from acab.core.parsing import parsers as PU
from acab.core.parsing.annotation import ValueAnnotation, ValueRepeatAnnotation
from acab.core.parsing.consts import (COLON, COMMA, DELIM, END, FUNC_SYMBOL,
                                      NEGATION, NG, N, emptyLine, ln, op, opLn,
                                      zrm)
from acab.core.parsing.param_core import ParamCore
from acab.core.parsing.statement_core import StatementCore
from acab.modules.parsing.exlo import constructors as PConst
from acab.modules.parsing.exlo.parsers import util as EU

logging             = logmod.getLogger(__name__)
# Hotload insertion points:
HOTLOAD_ANNOTATIONS   = pp.Forward()
HOTLOAD_ANNOTATIONS.set_name('hl_word_annot')
HOTLOAD_BAD_HEADS     = pp.Forward()
HOTLOAD_BAD_HEADS.set_name('hl_bad_heads')
HOTLOAD_SEN_ENDS      = pp.Forward()
HOTLOAD_SEN_ENDS.set_name('hl_sen_ends')
HOTLOAD_SEN_HEADS     = pp.Forward()
HOTLOAD_SEN_HEADS.set_name('hl_sen_heads')
HOTLOAD_SEN_POSTS     = pp.Forward()
HOTLOAD_SEN_POSTS.set_name('hl_sen_posts')

# Controllable words that can't start a sentence
BAD_HEADS           = ~(END | HOTLOAD_BAD_HEADS)("Bad Words")
BAD_HEADS.errmsg    = "Bad Head Word Found"


# Annotations for sentence words, auto wrapped with parens
annotations = pp.delimited_list(HOTLOAD_ANNOTATIONS, delim=COMMA)
annotations.set_parse_action(PConst.build_constraint_list)
annotations.set_name("Annotations")

# The one default head annotation: negation
sen_head_negation = NEGATION("SenNeg")
sen_head_negation.set_name("SenNeg")
sen_head_negation.set_parse_action(lambda x: ValueAnnotation(CDS.NEGATION, True))

op_head_annotation = FUNC_SYMBOL
op_head_annotation.set_parse_action(lambda x: ValueAnnotation(CDS.TYPE_INSTANCE, CDS.OPERATOR))
op_head_annotation.set_name("SenLambda")

# Annotations for before and after a sentence
# TODO shift to accumulateforward
sen_head_annotations = sen_head_negation | op_head_annotation | HOTLOAD_SEN_HEADS
# TODO add optional { } wrapper?
sen_post_annotations = pp.Forward()
sen_post_annotations << HOTLOAD_SEN_POSTS

# Core = a. | b! | $a. | $b!....
# Sentences are /SEN_WORD* (SEN_END | SEN_STATEMENT)/
# TODO explicit Semantic Hint operator?
SEN_HEAD              = NG(HEAD_ANNOTATION, zrm(sen_head_annotations)) + BAD_HEADS
SEN_WORD              = ParamCore(annotations)
SEN_NO_MODAL          = ParamCore(annotations, end=True) + ~COLON
SEN_END               = (HOTLOAD_SEN_ENDS | SEN_NO_MODAL)
SEN_WORD.set_name("PBCore")
SEN_NO_MODAL.set_name("PBEnd")

# The Prime Sentence definition:
SENTENCE = (SEN_HEAD + pp.Group(pp.ZeroOrMore(SEN_WORD) + SEN_END)(SEN)
            + NG(PDS.POST_ANNOTATION, zrm(sen_post_annotations)))
SENTENCE.set_parse_action(Pfunc.construct_sentence)
SENTENCE.set_name("Sentence")

SEN_PLURAL = pp.delimited_list(SENTENCE, delim=DELIM)
SEN_PLURAL.set_whitespace_chars(" \t")
SEN_PLURAL.set_name("Sentence Plural")


SEN_MACRO             = pp.Forward()
# FIXME sentence plural macro
# SEN_MACRO_BODY     = pp.IndentedBlock(SENTENCE)
# # Statement to specify multiple sub sentences
# SEN_MACRO        <<= StatementCore(pp.Literal("ζ"),
#                                    SEN_MACRO_BODY,
#                                    parse_fn=Pfunc.construct_multi_sentences)

op_sentence = pp.Group(pp.FollowedBy(op_head_annotation) + SENTENCE)
op_sentence.add_condition(lambda s, l, t: t[0][0].type == CDS.OPERATOR)
op_sentence.set_parse_action(lambda s, l, t: t[0])
op_sentence.set_name("op_sentence")

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
