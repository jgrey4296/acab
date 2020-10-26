#!/usr/bin/env python
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from . import wrappers
from acab.config import AcabConfig

util = AcabConfig.Get()

ANON_VALUE_S    = util("Printing", "ANON_VALUE_S")
FUNC_S           = util("Parsing.Structure", "FUNC_S")
QUERY_S          = util("Parsing.Structure", "QUERY_S")
AT_BIND_S         = util("Parsing.Structure", "AT_BIND_S")
CONSTRAINT_S     = util("Parsing.Structure", "CONSTRAINT_S")
END_S            = util("Parsing.Structure", "END_S")
FALLBACK_MODAL_S = util("Printing", "FALLBACK_MODAL_S")
NEGATION_S       = util("Parsing.Structure", "NEGATION_S")
SEN_JOIN_S       = util("Printing", "SEN_JOIN_S")
CONTAINER_JOIN_S = util("Printing", "CONTAINER_JOIN_S")
PARAM_JOIN_S     = util("Printing", "PARAM_JOIN_S")
OBVIOUS_TYPES    = []
OPERATOR_S       = util("Parsing.Structure", "OPERATOR_S")
QUERY_S          = util("Parsing.Structure", "QUERY_S")
TAB_S            = util("Printing", "TAB_S", action=AcabConfig.actions_e.STRIPQUOTE)
TAG_S            = util("Parsing.Structure", "TAG_S")
VALUE_TYPE_S     = util("Parsing.Structure", "VALUE_TYPE_S")
BIND_S            = util("Parsing.Structure", "BIND_S")


# Handler Types: Simple, Record, Destruct, Sentinel, Override
def regroup_sentinel(PS, source, processed, acc, params):
    """
    A Generic Regroup Sentinel. Takes the context and
    puts it in the accumulation, using the params as the key
    """
    target_name = params
    if target_name in acc:
        processed = acc[target_name] + processed
    return (PS.accumulate, {target_name: processed}, None)


def list_to_inst_list(PS, source, the_list, acc, regroup_name) -> List[Tuple[Any, Any, Any, Any]]:
    assert(isinstance(the_list, List))
    inst_list = [(PS.e_print, x, None, None) for x in the_list]
    inst_list.append((PS.sentinel, source, regroup_sentinel, regroup_name))
    return inst_list

def value_sentinel(PS, source, processed, acc, params):
    # name, modal, constraints
    modal_data_field = PS.ask('MODAL_FIELD')

    name = acc['name']
    modal = ""
    if modal_data_field is not False:
        modal = acc[modal_data_field]
    if PS.ask("drop_end_op", for_uuid=acc['uuid']):
        modal = ""

    constraints = acc['constraints']
    joined_constraints = ""
    if bool(constraints):
        joined_constraints = wrappers._wrap_constraints(PS, joined_constraints, acc)

    # TODO add variable wrap / type wrap

    return (PS.simple, "{}{}{}".format(name, joined_constraints,modal), None)

def simple_value_sentinel(PS: 'AcabPrintSemantics', value: 'AcabValue', acc):
    """ Basic Value Handler, returns the name or a basic variable glyph """
    current_str = acc['name']
    if current_str is None and value.is_var:
        current_str = "$_"

    return (PS.simple, current_str, None)

def value_uuid_long_sentinel(PS, value, processed, acc):
    """ A Basic sentinel for tracking specific objects """
    uuid = accum['uuid']
    name = accum['name']

    return (PS.simple, "({} : {})".format(name, uuid), None)

def sentence_sentinel(PS, source, processed, acc):
    """ Combines its destruct'd words, with its parameters """
    # combine the words
    join_str = PS.ask("sentence_join_str")
    words = acc['words']
    combined = join_str.join(words)

    as_query = wrappers._maybe_wrap_question(PS, value, combined)
    as_negated = wrappers._maybe_wrap_negation(PS, value, combined)

    final = as_negated
    return (PS.simple, as_negated, None)

def operator_sentinel(PS, source, processed, acc):
    # Get func symbol
    # TODO have default, plus override from PS?
    param_join = PS.ask("params_join_str")
    func_symbol = FUNC_SYMBOL_S
    # TODO split unary and n-ary operators
    op_name = acc['op_name']
    # combine for final value
    params = param_join.join(acc['params'])

    # TODO if operator._sugared:

    total = "{}{} ({})".format(func_symbol, op_name, params)
    return (PS.simple, total, None)

def container_sentinel(PS, source, processed, acc):
    # TODO combine clauses as needed

    return None

def statement_sentinel(PS, source, processed, acc):
    # Sequence processed into final form
    # Get name
    # Get type
    # Get params
    # Get Tags
    # Get body

    return None


# Accumulators

def value_uuid_accumulator(PS, value, acc):
    return (PS.accumulate, {'uuid': str(value._uuid)}, None)

def value_name_accumulator(PS, value, acc):
    return (PS.accumulate, {'name': value.name}, None)


# Substructs

def value_params_substuct(PS, value, acc):
    # TODO specify a sentinel too
    params = [(PS.e_print, None, x) for x in value._params]
    return (PS.substruct, params, None)

def sentence_substruct(PS, value, acc):
    words = value.words
    # TODO: change this to an Override registration
    PS.set_for_uuid(words[-1]._uuid, "drop_end_op", True)

    words = [(PS.e_print, x, None) for x in value.words[:-1]]
    # words.append((PS.e_call, x, SPECIAL_FUNC))
    # Return an additional accumulator sentinel for the words
    return (PS.substruct, words, regrouper)

def operator_substruct(PS, value, acc):
    """ Op Fix is the count of vars to print before printing the op.
    eg: op_fix=0 : + 1 2 3 ...
        op_fix=2 : 1 2 + 3 ...
    """
    # Get op
    operator = value.op
    # Get Params
    the_params = value._params

    # TODO add custom accumulator sentinels
    components = [(PS.e_print, operator, None)]
    components += [(PS.e_print, x, None) for x in the_params]

    return (value, components, None)

# Simple Handlers

def container_handler(PS, container):
    the_clauses = [(PS.e_print, x, None) for x in container.clauses]
    return (container, the_clauses, None)

def statement_handler(PS, value, acc):
    # Get name
    # Get type
    # Get params
    # Get Tags
    # Get body

    # TODO Order contents with tuples
    content = []
    return (value, content, None)
