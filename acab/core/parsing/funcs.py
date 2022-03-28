# pylint: disable=bad-whitespace
"""
Defines functions for parsers and parse -> data transform

"""
import logging as logmod

import pyparsing as pp
from acab.core.config.config import AcabConfig
from acab.core.data.instruction import (ProductionComponent,
                                       ProductionContainer,
                                       ProductionStructure)
from acab.interfaces import value as VI
from acab.core.data.value import AcabValue
from acab.core.parsing import consts as PConst
from acab.core.data.default_structure import TYPE_BOTTOM_NAME
from acab.core.parsing.annotation import ValueAnnotation
import acab.core.data.default_structure as DS
import acab.core.parsing.default_keys as PDS
from acab.core.data.factory import ValueFactory

logging = logmod.getLogger(__name__)

config = AcabConfig()
SEMANTIC_HINT    = DS.SEMANTIC_HINT

QUERY_SEM_HINT     = ValueFactory.value([config.prepare("SEMANTICS", "QUERY")()])
ACTION_SEM_HINT    = ValueFactory.value([config.prepare("SEMANTICS", "ACTION")()])
TRANSFORM_SEM_HINT = ValueFactory.value([config.prepare("SEMANTICS", "TRANSFORM")()])
RULE_SEM_HINT      = ValueFactory.value([config.prepare("SEMANTICS", "RULE")()])
AGENDA_SEM_HINT    = ValueFactory.value([config.prepare("SEMANTICS", "AGENDA")()])
LAYER_SEM_HINT     = ValueFactory.value([config.prepare("SEMANTICS", "LAYER")()])
PIPELINE_SEM_HINT  = ValueFactory.value([config.prepare("SEMANTICS", "PIPELINE")()])

ATOM = TYPE_BOTTOM_NAME

DEFAULT_TERM_DATA = {}
# TODO figure out a better way to do term defaults
# DEFAULT_TERM_DATA.update(config.defaults)

def make_value(s, loc, toks):
    """ Make a value coupled with additional data """
    value         = None
    annotations   = [x for x in toks[PDS.HEAD_ANNOTATION] if isinstance(x, ValueAnnotation)]
    annotations  += [x for x in toks[PDS.POST_ANNOTATION] if isinstance(x, ValueAnnotation)]
    _type         = ATOM
    data          = DEFAULT_TERM_DATA.copy()
    # TODO: link type primitives with type system

    if PDS.VALUE in toks:
        # is an actual value
        assert(isinstance(toks[PDS.VALUE][0], tuple))
        value = toks[PDS.VALUE][0][1]
        _type = toks[PDS.VALUE][0][0]
    else:
        raise SyntaxError("Unplanned parse type, expected a tuple of (type_str, value)", toks[PDS.VALUE])

    new_val = ValueFactory.value(value, data=data, _type=_type)
    for ann in annotations:
        ann(new_val)
    return [new_val]

def add_annotations(s, loc, toks):
    """ Add additional data to a node """
    word = toks[0][0]
    assert(isinstance(word, VI.Value_i))
    for anno in toks[0]:
        if not isinstance(anno, ValueAnnotation):
            continue

        anno(word)

    return [word]



def construct_multi_sentences(s, loc, toks):
    base_sen = toks[PDS.NAME][0]
    additional_sentences = toks[PDS.STATEMENT]

    new_sentences = []
    # combine
    for additional in additional_sentences:
        full_toks = base_sen.words[:] + additional.words[:]
        data = {}
        data.update(base_sen.data)
        data.update(additional.data)
        new_sen = ValueFactory.value(full_toks, data=data)
        new_sentences.append(new_sen)

    return new_sentences

def construct_sentence(s, loc, toks):
    assert(PDS.SEN in toks)
    sentence = ValueFactory.sen(toks[PDS.SEN][:])
    if PDS.HEAD_ANNOTATION in toks:
        for x in toks[PDS.HEAD_ANNOTATION]:
            x(sentence)
    if PDS.POST_ANNOTATION in toks:
        for x in toks[PDS.POST_ANNOTATION]:
            x(sentence)

    for x in toks:
        if not isinstance(x, ValueAnnotation):
            continue
        x(sentence)

    return sentence

def construct_statement(s, loc, toks):
    # Take the statement, and name it
    iden   = toks[PDS.NAME][0]
    targs = []
    tags  = []
    if PDS.ARG in toks:
        # PDS.BIND's ATOM returns a tuple of ('name', VARNAME)
        targs = [x[1] for x in toks[PDS.ARG]]
    # Get Tags
    if PDS.TAG in toks:
        tags = [y for x in toks[PDS.TAG] for y in x]

    obj = toks[PDS.STATEMENT][0]
    updated_obj = obj.apply_params(targs).apply_tags(tags)

    named_statement = iden.attach_statement(updated_obj)

    return named_statement

def build_assignment(s, loc, toks):
    # TODO refactor this into a valueannotation?
    return (toks[0][1], toks[1])


def strip_parse_type(s, loc, toks):
    """ Utility function to strip out parse data from return tuples,
    useful for:
    [("QUERY", actual_query)] -> [actual_query]

    NOTE: expects to be called from a group wrapping the actual parser
    """
    assert(all([isinstance(x, tuple) for x in toks[0]]))
    return [x[1] for x in toks[0]]



def deep_update_names(parser):
    logging.debug("Deep Updating Parser Names")
    queue = [parser]
    processed = set()

    while bool(queue):
        current = queue.pop(0)
        if current in processed:
            continue
        processed.add(current)

        if hasattr(current, "strRepr"):
            setattr(current, "strRepr", None)

        if hasattr(current, "expr"):
            queue.append(current.expr)
        elif hasattr(current, "exprs"):
            queue += current.exprs


def clear_parser_names(*parsers):
    logging.debug("Clearing Parser Names")
    for parser in parsers:
        if hasattr(parser, "name"):
            parser.set_name(None)

        if hasattr(parser, "strRepr") and parser.strRepr is not None:
            parser.strRepr = None
