#!/usr/bin/env python
# pylint: disable=bad-whitespace
import logging as root_logger
import pyparsing as pp

from acab.config import AcabConfig

from acab.abstract.parsing import consts as PConst
from acab.abstract.parsing.consts import s, op, orm, zrm, NG, DBLCOLON, COLON
from acab.abstract.parsing.consts import OPAR, CPAR, STATEMENT_S, ARG_S
from acab.abstract.core.sentence import Sentence

logging = root_logger.getLogger(__name__)

Fwd_ArgList = pp.Forward()
Fwd_TagList = pp.Forward()

def construct_multi_sentences(toks):
    # TODO use sentence.build
    base_sen = toks[PConst.NAME_S][0]
    additional_sentences = toks[PConst.STATEMENT_S]

    new_sentences = []
    # combine
    for additional in additional_sentences:
        full_toks = base_sen.words[:] + additional.words[:]
        data = {}
        data.update(base_sen._data)
        data.update(additional._data)
        new_sen = Sentence(full_toks, data=data)
        new_sentences.append(new_sen)

    return new_sentences

def construct_sentence(toks):
    data = {PConst.NEGATION_S : False}
    if PConst.NEGATION_S in toks:
        data[PConst.NEGATION_S] = True
    return Sentence(toks[PConst.SEN_S][:], data=data)

def construct_statement(toks):
    # Take the statement, and add it to the location
    sen   = toks[PConst.NAME_S][0]
    targs = []
    tags  = []
    if PConst.ARG_S in toks:
        # BIND's ATOM returns a tuple of ('name', VARNAME)
        targs = [y for x,y in toks[PConst.ARG_S][:]]
    # Get Tags
    if PConst.TAG_S in toks:
        tags = [x[1] for x in toks[PConst.TAG_S]]

    obj_tuple  = toks[PConst.STATEMENT_S][0]
    obj_tuple[1].apply_params(targs).apply_tags(tags)

    new_sentence = sen.attach_statement(obj_tuple[1]).verify()

    return new_sentence

def STATEMENT_CONSTRUCTOR(head_p,
                          name_p,
                          body_p,
                          end=None,
                          args=True,
                          single_line=False,
                          parse_fn=None):
    """ Construct statements of the form:
    a.location: (::λ) |args| components end
    """
    line_p = PConst.emptyLine
    end_p  = PConst.END
    arg_p  = pp.empty

    if single_line:
        line_p = pp.empty
        end_p = pp.lineEnd
    elif end is not None:
        end_p = end

    if args:
        arg_p = op(NG(ARG_S, Fwd_ArgList + line_p))

    head_hint = OPAR + DBLCOLON + head_p + CPAR

    parser = NG(PConst.NAME_S, name_p) + PConst.COLON + s(head_hint) + op(pp.lineEnd) \
        + arg_p + Fwd_TagList + NG(STATEMENT_S, body_p) + end_p

    if parse_fn is not None:
        parser.addParseAction(parse_fn)
    else:
        parser.addParseAction(construct_statement)
    return parser


def OP_PATH_C(sen):
    op_path = PConst.FUNC_SYMBOL + sen
    op_path.setName("Operator_Path")
    return op_path
