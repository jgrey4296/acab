#!/usr/bin/env python
# pylint: disable=bad-whitespace
"""
Defines functions for parsers and parse -> data transform

"""
import logging as root_logger
import pyparsing as pp

from acab.abstract.config.config import AcabConfig

from acab.abstract.parsing import consts as PConst
from acab.abstract.core.values import AcabValue
from acab.abstract.core.values import Sentence
from acab.abstract.rule.production_abstractions import ProductionContainer, ProductionComponent, ProductionStructure

logging = root_logger.getLogger(__name__)

config = AcabConfig.Get()


def make_value(toks):
    """ Make a value coupled with additional data """
    value = None
    _type = PConst.ATOM_V
    data = PConst.DEFAULT_NODE_DATA.copy()
    # TODO: link type primitives with type system
    if PConst.BIND_S in toks:
        # is variable
        assert(isinstance(toks[PConst.BIND_S][0], tuple))
        value = toks[PConst.BIND_S][0][1]
        data[PConst.BIND_S] = True
    elif PConst.AT_BIND_S in toks:
        # is a reference
        # (can only be at head of a sentence)
        assert(isinstance(toks[PConst.AT_BIND_S][0], tuple))
        value = toks[PConst.AT_BIND_S][0][1]
        data[PConst.BIND_S] = PConst.AT_BIND_S
    elif PConst.VALUE_S in toks:
        # is an actual value
        assert(isinstance(toks[PConst.VALUE_S], tuple))
        value = toks[PConst.VALUE_S][1]
        _type = toks[PConst.VALUE_S][0]
    else:
        raise SyntaxError("Unplanned parse type")

    new_val = AcabValue.safe_make(value, data=data, _type=_type)
    return new_val

def add_annotations(toks):
    """ Add additional data to a node """
    update_data = {}
    if PConst.MODAL_S in toks:
        modal_tuple = toks[PConst.MODAL_S][0]
        update_data[modal_tuple[1][0]] = modal_tuple[1][1]
    if PConst.ANNOTATION_S in toks:
        update_data.update({x: y for x, y in toks[PConst.ANNOTATION_S]})
    toks[PConst.NODE_S].data.update(update_data)
    return toks[PConst.NODE_S]



def construct_multi_sentences(toks):
    # TODO use sentence.build
    base_sen = toks[PConst.NAME_S][0]
    additional_sentences = toks[PConst.STATEMENT_S]

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

def construct_sentence(toks):
    assert(PConst.SEN_S in toks)
    data = {PConst.NEGATION_S : False}
    if PConst.NEGATION_S in toks:
        data[PConst.NEGATION_S] = True
    return Sentence.build(toks[PConst.SEN_S][:], data=data)

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

    type_name, obj = toks[PConst.STATEMENT_S][0]
    updated_obj = obj.apply_params(targs).apply_tags(tags)

    new_sentence = sen.attach_statement(updated_obj)

    return new_sentence

def build_constraint_list(toks):
    """ Build a constraint list """
    return (PConst.CONSTRAINT_S, [x[1] for x in toks[:]])

def build_query_component(toks):
    """ Build a comparison """
    op = toks[PConst.OPERATOR_S][0]
    return (PConst.CONSTRAINT_S, ProductionComponent(value=op, params=[toks[PConst.VALUE_S]]))

def build_clause(toks):
    # detect negation and annotate the clause with it
    data = { PConst.QUERY_S : True,
             PConst.QUERY_FALLBACK_S : None }
    if PConst.QUERY_FALLBACK_S in toks:
        # TODO move this into verify
        # if NEGATION_S in toks:
        #     raise AcabParseException("Negated Fallback clauses don't make sense")
        data[PConst.QUERY_FALLBACK_S] = toks[PConst.QUERY_FALLBACK_S][:]


    return toks[0].data.update(data)

def build_query(toks):
    query = ProductionContainer(value=toks[:])
    return (PConst.QUERY_S, query)

def build_assignment(toks):
    return (toks[0][1], toks[1])

def build_action_component(toks):
    params = []
    if PConst.LEFT_S in toks:
        params.append(toks[PConst.LEFT_S])
    if PConst.RIGHT_S in toks:
        params = toks[PConst.RIGHT_S][:]
    op = toks[PConst.OPERATOR_S][0]
    filtered_params = [x[0] if len(x) == 1 else x for x in params]
    return ProductionComponent(value=op, params=filtered_params, sugared=PConst.LEFT_S in toks)


def build_action(toks):
    # TODO: check this
    clauses = [x if isinstance(x, ProductionComponent)
               else ProductionComponent(value=Sentence.build([PConst.DEFAULT_ACTION_S]), params=[x]) for x in toks]
    act = ProductionContainer(value=clauses)

    return (act.type, act)

def build_transform_component(toks):
    params = []
    if PConst.LEFT_S in toks:
        params.append(toks[PConst.LEFT_S][0])
    params += toks[PConst.RIGHT_S][:]

    op = toks[PConst.OPERATOR_S][0]
    if isinstance(op, str):
        op = Sentence.build([op])

    rebind = toks[PConst.TARGET_S][0]
    filtered_params = [x[0] if len(x) == 1 else x for x in params]

    return ProductionComponent(value=op, params=filtered_params, rebind=rebind, sugared=PConst.LEFT_S in toks)

def build_transform(toks):
    trans = ProductionContainer(value=toks[:])
    return (PConst.TRANSFORM_S, trans)

def build_rule(toks):

    # Get Conditions
    if PConst.QUERY_S in toks:
        c = toks[PConst.QUERY_S][0][1]
        assert(isinstance(c, ProductionContainer))
    else:
        c = None

    # Get Transform
    if PConst.TRANSFORM_S in toks:
        t = toks[PConst.TRANSFORM_S][0][1]
        assert(isinstance(t, ProductionContainer))
    else:
        t = None

    # Get Action
    if PConst.ACTION_S in toks:
        a = toks[PConst.ACTION_S][0][1]
        assert(isinstance(a, ProductionContainer))
    else:
        a = None

    structure = {
        PConst.QUERY_S : c,
        PConst.TRANSFORM_S : t,
        PConst.ACTION_S : a
        }

    rule = ProductionStructure(structure=structure)
    return (rule.type, rule)

def make_agenda(toks):
    rule_type_str, as_rule = build_rule(toks)
    return  (PConst.AGENDA_S, as_rule)

def make_layer(toks):
    rule_type_str, as_rule = build_rule(toks)
    return  (PConst.LAYER_S, as_rule)


def make_pipeline(toks):
    rule_type_str, as_rule = build_rule(toks)
    return  (PConst.PIPELINE_S, as_rule)
