#!/usr/bin/env python
from . import wrappers
from acab.config import AcabConfig

util = AcabConfig.Get()
FUNC_SYMBOL_S = util("Parsing", "FUNC_SYMBOL_S")
QUERY_SYMBOL_S = util("Parsing", "QUERY_SYMBOL_S")


def regrouper(PS, source, processed, acc):
    """
    Regroup some processed into a position in the accumulator
    """
    return (PS.accumulate, {'words': processed}, None)


# Handler Types: Simple, Record, Destruct, Sentinel, Override

# Sentinels


def value_sentinel(PS, source, processed, acc):
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
