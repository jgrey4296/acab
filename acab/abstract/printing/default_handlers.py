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
        # TODO fix this
        joined_constraints = wrappers._wrap_constraints(PS, joined_constraints)

    # TODO add variable wrap / type wrap

    return (PS.simple, "{}{}{}".format(name, joined_constraints,modal))


def simple_value_sentinel(PS: 'AcabPrintSemantics', value: 'AcabValue', processed, acc, params: Any):
    """ Basic Value Handler, returns the name or a basic variable glyph """
    current_str = acc['name']
    if current_str is None and value.is_var:
        # TODO update to anon value
        current_str = PS.ask(ANON_VALUE_S)

    return (PS.simple, current_str, None)


def value_uuid_accumulator(PS, value, acc, params):
    return (PS.accumulate, {'uuid': str(value._uuid)}, None)

def value_name_accumulator(PS, value, acc, params):
    return (PS.accumulate, {'name': value.name}, None)

def value_uuid_long_sentinel(PS, value, processed, acc, params):
    """ A Basic sentinel for tracking specific objects """
    uuid = acc['uuid']
    name = acc['name']

    return (PS.simple, "({} : {})".format(name, uuid), None)




def type_instance_substruct(PS, value, acc, params):
    return None
def type_instance_sentinel(PS, value, processed, acc, params):
    return None
def sentence_substruct(PS, value, acc, params):
    words = value.words
    # TODO: change this to an Override registration
    PS.set_for_uuid(words[-1]._uuid, ["drop_end_op"])
    words = list_to_inst_list(PS, value, [x for x in value.words], acc, "words")
    return (PS.substruct, words, None)

def sentence_sentinel(PS, source, processed, acc, params):
    """ Combines its destruct'd words, with its parameters """
    # combine the words
    join_str = PS.ask(SEN_JOIN_S)
    words = acc['words']
    combined = join_str.join(words)

    as_query = wrappers._maybe_wrap_question(PS, source, combined)
    as_negated = wrappers._maybe_wrap_negation(PS, source, as_query)

    final = as_negated
    return (PS.simple, as_negated, None)


def operator_substruct(PS, value, acc, params):
    """ Op Fix is the count of vars to print before printing the op.
    eg: op_fix=0 : + 1 2 3 ...
        op_fix=2 : 1 2 + 3 ...
    """
    # Get op
    operator = value.op
    # Get Params
    the_params = list_to_inst_list(PS, value, [x for x in value._params], acc,"params")
    op_name = list_to_inst_list(PS, value, [operator], acc, "op_name")
    return (PS.substruct, the_params + op_name, operator_sentinel)

def operator_sentinel(PS, source, processed, acc, params):
    # Get func symbol
    # TODO have default, plus override from PS?
    param_join = PS.ask(PARAM_JOIN_S)
    func_symbol = PS.ask(FUNC_S)
    # TODO split unary and n-ary operators
    op_name = acc['op_name']
    # combine for final value
    params = param_join.join(acc['params'])

    # TODO if operator._sugared:

    total = "{}{} ({})".format(func_symbol, op_name, params)
    return (PS.simple, total, None)


def container_handler(PS, container, acc, params):
    the_clauses = list_to_inst_list(PS, container, [x for x in container.clauses], acc, "clauses")
    return (PS.SUBSTRUCT, the_clauses, None)

def container_sentinel(PS, source, processed, acc, params):
    join_str = PS.ask(CONTAINER_JOIN_S)
    clauses = acc['clauses']
    final = join_str.join(clauses)
    return (PS.simple, final, None)


def statement_handler(PS, value, acc, params):
    # Get name
    # Get type
    # Get params
    # Get Tags
    # Get body

    # TODO Order contents with tuples
    content = []
    return (PS.substruct, content, None)

def statement_sentinel(PS, source, processed, acc, params):
    # Sequence processed into final form
    # Get name
    name = acc['name']
    # Get type
    _type = acc['type']
    # Get params
    params = acc['params']
    # Get Tags
    tags = acc['tags']
    # Get body
    body = acc['body']

    # TODO
    final = ""
    return (None, final, None)



# Default Pairings:
DEF_VALUE_PAIR = ([value_name_accumulator], simple_value_sentinel)
DEF_UUID_PAIR = ([value_uuid_accumulator, value_name_accumulator], value_uuid_long_sentinel)
DEF_SEN_PAIR = ([sentence_substruct], sentence_sentinel)
DEF_TYPE_PAIR = ([type_instance_substruct], type_instance_sentinel)
DEF_OP_PAIR = ([operator_substruct], operator_sentinel)
DEF_CONTAINER_PAIR = ([container_handler], container_sentinel)
