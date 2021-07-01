# pylint: disable=bad-whitespace
"""
Defines functions for parsers and parse -> data transform

"""
import logging as root_logger

import pyparsing as pp
from acab.abstract.config.config import AcabConfig
from acab.abstract.core.production_abstractions import (ProductionComponent,
                                                        ProductionContainer,
                                                        ProductionStructure)
from acab.abstract.core.values import AcabValue, Sentence
from acab.abstract.parsing import consts as PConst
from acab.abstract.core.default_structure import TYPE_BOTTOM_NAME

import acab.abstract.core.default_structure as DS
import acab.abstract.parsing.default_structure as PDS

logging = root_logger.getLogger(__name__)

config = AcabConfig.Get()
SEMANTIC_HINT_V    = config.prepare("Value.Structure", "SEMANTIC_HINT")()

QUERY_SEM_HINT     = Sentence.build([config.prepare("SEMANTICS", "QUERY")()])
ACTION_SEM_HINT    = Sentence.build([config.prepare("SEMANTICS", "ACTION")()])
TRANSFORM_SEM_HINT = Sentence.build([config.prepare("SEMANTICS", "TRANSFORM")()])
RULE_SEM_HINT      = Sentence.build([config.prepare("SEMANTICS", "RULE")()])
AGENDA_SEM_HINT    = Sentence.build([config.prepare("SEMANTICS", "AGENDA")()])
LAYER_SEM_HINT     = Sentence.build([config.prepare("SEMANTICS", "LAYER")()])
PIPELINE_SEM_HINT  = Sentence.build([config.prepare("SEMANTICS", "PIPELINE")()])

ATOM = TYPE_BOTTOM_NAME

def make_value(s, loc, toks):
    """ Make a value coupled with additional data """
    value = None
    _type = ATOM
    data = PConst.DEFAULT_NODE_DATA.copy()
    # TODO: link type primitives with type system
    if PDS.BIND in toks:
        # is variable
        assert(isinstance(toks[PDS.BIND][0], tuple))
        value = toks[PDS.BIND][0][1]
        data[DS.BIND] = True
    elif PDS.AT_BIND in toks:
        # is a reference
        # (can only be at head of a sentence)
        assert(isinstance(toks[PDS.AT_BIND][0], tuple))
        value = toks[PDS.AT_BIND][0][1]
        data[DS.BIND] = DS.AT_BIND
    elif PDS.VALUE in toks:
        # is an actual value
        assert(isinstance(toks[PDS.VALUE], tuple))
        value = toks[PDS.VALUE][1]
        _type = toks[PDS.VALUE][0]
    else:
        raise SyntaxError("Unplanned parse type")

    new_val = AcabValue.safe_make(value, data=data, _type=_type)
    return new_val

def add_annotations(s, loc, toks):
    """ Add additional data to a node """
    update_data = {}
    if PDS.MODAL in toks:
        modal_tuple = toks[PDS.MODAL][0]
        update_data[modal_tuple[1][0]] = modal_tuple[1][1]
    if PDS.ANNOTATION in toks:
        update_data.update({x: y for x, y in toks[PDS.ANNOTATION]})
    toks[PDS.NODE].data.update(update_data)
    return toks[PDS.NODE]



def construct_multi_sentences(s, loc, toks):
    # TODO use sentence.build
    base_sen = toks[PDS.NAME][0]
    additional_sentences = toks[PDS.STATEMENT]

    new_sentences = []
    # combine
    for additional in additional_sentences:
        full_toks = base_sen.words[:] + additional.words[:]
        data = {}
        data.update(base_sen.data)
        data.update(additional.data)
        new_sen = Sentence.build(full_toks, data=data)
        new_sentences.append(new_sen)

    return new_sentences

def construct_sentence(s, loc, toks):
    assert(PDS.SEN in toks)
    data = {DS.NEGATION : False}
    if PDS.NEGATION in toks:
        data[DS.NEGATION] = True
    return Sentence.build(toks[PDS.SEN][:], data=data)

def construct_statement(s, loc, toks):
    # Take the statement, and add it to the location
    sen   = toks[PDS.NAME][0]
    targs = []
    tags  = []
    if PDS.ARG in toks:
        # PDS.BIND's ATOM returns a tuple of ('name', VARNAME)
        targs = [y for x,y in toks[PDS.ARG][:]]
    # Get Tags
    if PDS.TAG in toks:
        tags = [x[1] for x in toks[PDS.TAG]]

    type_name, obj = toks[PDS.STATEMENT][0]
    updated_obj = obj.apply_params(targs).apply_tags(tags)

    new_sentence = sen.attach_statement(updated_obj)

    return new_sentence

def build_clause(s, loc, toks):
    # detect negation and annotate the clause with it
    data = { DS.QUERY : True,
             DS.QUERY_FALLBACK : None }
    if PDS.QUERY_FALLBACK in toks:
        # TODO move this into verify
        # if PDS.NEGATION in toks:
        #     raise AcabParseException("Negated Fallback clauses don't make sense")
        data[DS.QUERY_FALLBACK] = toks[PDS.QUERY_FALLBACK][:]


    toks[0].data.update(data)
    return toks

def build_assignment(s, loc, toks):
    return (toks[0][1], toks[1])

