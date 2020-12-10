#!/usr/bin/env python
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic
import logging as root_logger
logging = root_logger.getLogger(__name__)

from . import wrappers
from acab.abstract.config.config import AcabConfig

config = AcabConfig.Get()
# These don't vary at run time, so use .value
QUERY_V          = config.value("Value.Structure", "QUERY")
AT_BIND_V        = config.value("Value.Structure", "AT_BIND")
CONSTRAINT_V     = config.value("Value.Structure", "CONSTRAINT")
NEGATION_V       = config.value("Value.Structure", "NEGATION")
OPERATOR_V       = config.value("Value.Structure", "OPERATOR")
TYPE_INSTANCE_V  = config.value("Value.Structure", "TYPE_INSTANCE")
BIND_V           = config.value("Value.Structure", "BIND")

# These can vary at runtime, so prepare then use with print semantics:
OBVIOUS_TYPES    = config.prepare("Print.Data", "SUPPRESSION_TYPES", actions=[AcabConfig.actions_e.SPLIT])

ANON_VALUE_P     = config.prepare("Symbols", "ANON_VALUE")
FUNC_P           = config.prepare("Symbols", "FUNC")
END_P            = config.prepare("Symbols", "END")
FALLBACK_MODAL_P = config.prepare("Symbols", "FALLBACK_MODAL", actions=[AcabConfig.actions_e.STRIPQUOTE])
QUERY_SYMBOL_P   = config.prepare("Symbols", "QUERY")
TAG_P            = config.prepare("Symbols", "TAG")

SEN_JOIN_P       = config.prepare("Print.Patterns", "SEN_JOIN", actions=[AcabConfig.actions_e.STRIPQUOTE])
CONTAINER_JOIN_P = config.prepare("Print.Patterns", "CONTAINER_JOIN")
PARAM_JOIN_P     = config.prepare("Print.Patterns", "PARAM_JOIN")
TAB_P            = config.prepare("Print.Patterns", "TAB", actions=[AcabConfig.actions_e.STRIPQUOTE])



# Handler Types: Simple, Record, Destruct, Sentinel, Override
def regroup_sentinel(PS, source_val, processed, acc, params):
    """
    A Generic Regroup Sentinel. Takes the context and
    puts it in the accumulation, using the params as the key
    """
    target_name = params
    copied = processed[:]
    processed.clear()
    # NOTE: does not integrate already existing values in target_name
    return (PS.accumulate, {target_name: copied}, None, None)


def list_to_inst_list(PS, source_val, the_list, acc, regroup_name) -> List[Tuple[Any, Any, Any, Any]]:
    """ Convert a list into a list of instructions to handle,
    then group it
    """
    assert(isinstance(the_list, List))
    inst_list = [(PS.e_print, x, None, None) for x in the_list]
    if bool(inst_list):
        inst_list.append((PS.sentinel, source_val, regroup_sentinel, regroup_name))
    else:
        inst_list.append((PS.accumulate, {regroup_name: ''}, None, None))
    return inst_list

def value_sentinel(PS, source_val, processed, acc, params):
    # name, modal, constraints
    modal_data_field = PS.ask('MODAL_FIELD')

    name = acc['name']
    modal = ""
    if modal_data_field is not False:
        modal = acc[modal_data_field]
    # TODO this is relevant to statements as leaves/not
    if PS.ask("drop_modal", for_uuid=source_val.uuid):
        modal = ""

    joined_constraints = ""
    if 'constraints' in acc:
        constraints = acc['constraints']
        if bool(constraints):
            # TODO fix this
            joined_constraints = wrappers._wrap_constraints(PS, joined_constraints)

    return (PS.simple, "{}{}{}".format(name, joined_constraints,modal), None, None)


def simple_value_sentinel(PS: 'AcabPrintSemantics', value: 'AcabValue', processed, acc, params: Any):
    """ Basic Value Handler, returns the name or a basic variable glyph """
    current_str = acc['name']
    if current_str is None and value.is_var:
        # TODO update to anon value
        current_str = PS.use(ANON_VALUE_P)

    return (PS.simple, current_str, None, None)


def value_uuid_accumulator(PS, value, acc, params):
    return (PS.accumulate, {'uuid': str(value.uuid)}, None, None)

def value_name_accumulator(PS, value, acc, params):
    base = value.name
    reg_wrapped = wrappers._maybe_wrap_regex(PS, value, base)
    str_wrapped = wrappers._maybe_wrap_str(PS, value, reg_wrapped)
    with_var = wrappers._maybe_wrap_var(PS, value, str_wrapped)

    # TODO call type registered transforms
    return (PS.accumulate, {'name': with_var}, None, None)

def modality_accumulator(PS, value, acc, params):
    modal_field = PS.ask('MODAL_FIELD')
    modal_value = None
    if modal_field in value.data:
        modal_value = value.data[modal_field]

    modal_alias = PS.ask(modal_value)
    if bool(modal_alias):
        modal_value = modal_alias
    elif modal_value in config.modal_printing:
        modal_value = config.modal_printing[modal_value]
    else:
        # TODO define this in config
        modal_value = PS.use(FALLBACK_MODAL_P)

    return (PS.accumulate, {modal_field: modal_value}, None, None)

def value_uuid_long_sentinel(PS, value, processed, acc, params):
    """ A Basic sentinel for tracking specific objects """
    uuid = acc['uuid']
    name = acc['name']

    return (PS.simple, "({} : {})".format(name, uuid), None, None)




def type_instance_substruct(PS, value, acc, params):
    return None

def type_instance_sentinel(PS, value, processed, acc, params):
    return None

def sentence_substruct(PS, value, acc, params):
    logging.info("Sentence Substruct: {}".format(value))
    words = value.words
    # TODO: change this to an Override registration
    PS.set_for_uuid(words[-1].uuid, ["drop_modal"])
    words = list_to_inst_list(PS, value, [x for x in value.words], acc, "words")
    return (PS.substruct, words, None, None)

def sentence_sentinel(PS, source_val, processed, acc, params):
    """ Combines its destruct'd words, with its parameters """
    logging.info("Sentence Sentinel: {}".format(source_val))

    # combine the words
    join_str = PS.use(SEN_JOIN_P)
    words = acc['words']
    combined = join_str.join(words)

    as_query = wrappers._maybe_wrap_question(PS, source_val, combined)
    as_negated = wrappers._maybe_wrap_negation(PS, source_val, as_query)

    final = as_negated
    return (PS.simple, as_negated, None, None)


def operator_substruct(PS, value, acc, params):
    """ Op Fix is the count of vars to print before printing the op.
    eg: op_fix=0 : + 1 2 3 ...
        op_fix=2 : 1 2 + 3 ...
    """
    # Get op
    operator = value.op
    # Get Params
    the_params = list_to_inst_list(PS, value, [x for x in value.params], acc,"params")
    op_name = list_to_inst_list(PS, value, [operator], acc, "op_name")
    return (PS.substruct, the_params + op_name, operator_sentinel, None)

def operator_sentinel(PS, source_val, processed, acc, params):
    # Get func symbol
    # TODO have default, plus override from PS?
    param_join = PS.use(PARAM_JOIN_P)
    func_symbol = PS.use(FUNC_P)
    # TODO split unary and n-ary operators
    op_name = acc['op_name']
    # combine for final value
    params = param_join.join(acc['params'])

    # TODO if operator._sugared:

    total = "{}{} ({})".format(func_symbol, op_name, params)
    return (PS.simple, total, None, None)


def container_handler(PS, container, acc, params):
    the_clauses = list_to_inst_list(PS, container, [x for x in container.clauses], acc, "clauses")
    return (PS.SUBSTRUCT, the_clauses, None, None)

def container_sentinel(PS, source_val, processed, acc, params):
    join_str = PS.use(CONTAINER_JOIN_P)
    clauses = acc['clauses']
    final = join_str.join(clauses)
    return (PS.simple, final, None, None)


def statement_handler(PS, value, acc, params):
    # Get name
    # Get type
    # Get params
    # Get Tags
    # Get body

    # TODO Order contents with tuples
    content = []
    return (PS.substruct, content, None, None)

def statement_sentinel(PS, source_val, processed, acc, params):
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
    return (None, final, None, None)


def component_substruct(PS, source_val, acc, params):
    logging.info("Component Substruct: {}".format(source_val))
    components = []
    components.append((PS.e_print, source_val.op, None, None))
    components.append((PS.sentinel, source_val, regroup_sentinel, 'op'))

    if source_val.rebind is not None:
        components.append((PS.e_print, source_val.rebind, regroup_sentinel, 'rebind'))
    else:
        components.append((PS.accumulate, {'rebind' : None}, None, None))

    components += list_to_inst_list(PS, source_val, source_val.params, acc, 'params')
    return (PS.substruct, components, None, None)

def component_sentinel(PS, source_val, processed, acc, params):
    logging.info("Component Sentinel: {}".format(source_val))
    operator = acc['op']
    params = acc['params']
    rebind = acc['rebind']
    func_symbol = PS.use(FUNC_P)


    final_str = "{}{}".format(func_symbol, operator[0])
    # todo wrap list
    final_str += wrappers._maybe_wrap_list(PS, params)

    # todo wrap_rebind
    final_str += wrappers._maybe_wrap_rebind(PS, rebind)

    return (PS.simple, final_str, None, None)


def structure_substruct(PS, source_val, acc, params):
    # TODO: split out anything in the structure, plus name, etc
    # TODO: detect context: as leaf of sentence or not

    components = []
    return (PS.substruct, components, None, None)

def structure_sentinel(PS, source_val, processed, acc, params):
    # combine in a statement format

    return (PS.simple, "", None, None)


# Default Pairings:
DEF_VALUE_PAIR = ([value_name_accumulator], simple_value_sentinel)
DEF_UUID_PAIR = ([value_uuid_accumulator, value_name_accumulator], value_uuid_long_sentinel)
DEF_SEN_PAIR = ([sentence_substruct], sentence_sentinel)
DEF_TYPE_PAIR = ([type_instance_substruct], type_instance_sentinel)
DEF_OP_PAIR = ([operator_substruct], operator_sentinel)
DEF_CONTAINER_PAIR = ([container_handler], container_sentinel)
