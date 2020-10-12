#!/usr/bin/env python
from . import wrappers


def value_handler(semantics: 'AcabPrintSemantics', value: 'AcabValue') -> str:
    """ Basic Value Handler, returns the name """
    current_str = str(value.name)
    for func in [wrappers._maybe_wrap_str,
                 wrappers._maybe_wrap_regex,
                 wrappers._maybe_wrap_var]:
        current_str = func(semantics, value, current_str)

    # TODO use continuation to print type instance then combine
    # TODO add constraints to continuation
    # transform the value format if necessary
    # eg: 2.34 => 2d34
    # if hasattr(value, "type") and value.type in TYPE_WRAPS:
    #     val = TYPE_WRAPS[value.type](val)

    # # Wrap constraints
    # val = _wrap_constraints(val, value._data)

    return current_str


def value_uuid_long_handler(semantics, value) -> str:
    uuid = str(value._uuid)
    current_str = str(value.name)
    for func in [wrappers._maybe_wrap_str,
                 wrappers._maybe_wrap_regex,
                 wrappers._maybe_wrap_var]:
        current_str = func(semantics, value, current_str)

    return "({} : {})".format(current_str, uuid)

def value_modal_handler(semantics, value) -> str:
    modal_data_field = semantics.ask('MODAL_FIELD')
    assert(modal_data_field is not False)

    current_str = str(value.name)
    modal_str = wrappers._modal_operator(semantics, value, name)

    for func in [wrappers._maybe_wrap_str,
                 wrappers._maybe_wrap_regex,
                 wrappers._maybe_wrap_var]:
        current_str = func(semantics, value, current_str)

    # Return as tuple, to leave off modal-str if necessary
    # at end of sentence
    drop_end = semantics.ask("drop_end_op", for_uuid=value._uuid)
    if drop_end:
        return current_str
    else:
        return "{}{}".format(current_str, modal_str)


def sentence_finaliser(semantics, source, processed):
    # combine the words
    join_str = semantics.ask("sentence_join_str")
    remaining = ""
    combined = ""
    try:
        # TODO
        fallback_index = processed.index(("fallback_separator", None))
        combined = join_str.join(processed[:fallback_index])
        remaining = wrappers._wrap_fallback(semantics, processed[fallback_index+1:])
    except ValueError:
        combined = join_str.join(processed)

    as_query = wrappers._maybe_wrap_question(semantics, value, combined)
    as_negated = wrappers._maybe_wrap_negation(semantics, value, combined)

    final = as_negated + remaining

    return as_negated

def sentence_handler(semantics, value):
    words = value.words
    semantics.set_for_uuid(words[-1]._uuid, "drop_end_op", True)

    # TODO: fallback values
    FALLBACK_S = semantics.ask("FALLBACK_S")
    try:
        # TODO use an enum here
        fallback_words = [("fallback_separator", None)]
        fallback_words += value._data[FALLBACK_S]
        words += [y for x in fallback_words for y in x]
    except KeyError:
        pass

    return (value, words, sentence_finaliser)


def operator_finaliser(semantics, source, processed) -> str:
    # Get func symbol

    # split processed


    # combine for final value
    # op_str = "{}{}".format(FUNC_SYMBOL_S, pprint(operator.op))
    # the_params = [x.pprint() for x in operator._params]

    return val

def operator_handler(semantics, value) -> str:
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


    return (value, components, operator_finaliser)


def container_finaliser(semantics, source, processed) -> str:
    # TODO combine clauses as needed

    return None

def container_handler(semantics, container) -> str:
    the_clauses = container.clauses
    return (container, the_clauses, container_finaliser)


def statement_finaliser(semantics, source, processed) -> str:
    # Sequence processed into final form

    return None

def statement_handler(semantics, value) -> str:
    # Get name

    # Get type

    # Get params

    # Get Tags

    # Get body

    # TODO Order contents with tuples
    content = []
    return (value, content, statement_finaliser)
