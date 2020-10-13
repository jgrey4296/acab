#!/usr/bin/env python
from . import wrappers

def regrouper(PS, source, processed, acc):
    acc['words'] = processed
    return (PS.e_pass, None, None)


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

    return (PS.SIMPLE, "{}{}{}".format(name, joined_constraints,modal))

def simple_value_sentinel(PS: 'AcabPrintSemantics', value: 'AcabValue', acc):
    """ Basic Value Handler, returns the name """
    current_str = acc['name']
    return (PS.simple, current_str)

def value_uuid_long_sentinel(PS, value, processed, acc):
    uuid = accum['uuid']
    name = accum['name']

    return (PS.simple, "({} : {})".format(name, uuid))

def value_modal_sentinel(PS, value, processed, acc):
    modal_data_field = PS.ask('MODAL_FIELD')
    assert(modal_data_field is not False)
    name = acc['name']
    modal_str = acc[modal_data_field]

    drop_end = PS.ask("drop_end_op", for_uuid=value._uuid)
    if drop_end:
        return name
    else:
        return "{}{}".format(name, modal_str)

def sentence_sentinel(PS, source, processed, acc):
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

    # split processed


    # combine for final value
    # op_str = "{}{}".format(FUNC_SYMBOL_S, pprint(operator.op))
    # the_params = [x.pprint() for x in operator._params]

    return val

def container_sentinel(PS, source, processed, acc):
    # TODO combine clauses as needed

    return None

def statement_sentinel(PS, source, processed, acc):
    # Sequence processed into final form

    return None


# Accumulators

def value_uuid_accumulator(PS, value, acc):
    return (PS.accumulate, {'uuid': str(value._uuid)})

def value_name_accumulator(PS, value, acc):
    return (PS.accumulate, {'name': str(value.name)})


# Substructs

def value_params_substuct(PS, value, acc):
    # TODO specify a sentinel too
    return (PS.substruct, value._params)

def sentence_substruct(PS, value, acc):
    words = value.words
    PS.set_for_uuid(words[-1]._uuid, "drop_end_op", True)

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

    if operator._sugared:
        head = [x for x in the_params[:1]]
        tail = [x for x in the_params[1:]]

    # TODO add info tuples
    components = head + [operator] + tail


    return (value, components, operator_sentinel)

# Simple Handlers

def container_handler(PS, container): the_clauses = container.clauses return (container, the_clauses, container_sentinel)

def statement_handler(PS, value, acc):
    # Get name

    # Get type

    # Get params

    # Get Tags

    # Get body

    # TODO Order contents with tuples
    content = []
    return (value, content, statement_sentinel)
