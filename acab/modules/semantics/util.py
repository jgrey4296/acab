#!/usr/bin/env python3

from enum import Enum

from acab.abstract.config.config import AcabConfig
from acab.abstract.core.values import AcabValue

import logging as root_logger
logging = root_logger.getLogger(__name__)

config = AcabConfig.Get()

# TODO RDFSemantics, ReteSemantics
#
def SemanticSubCtxBuildDecorator(f):
    """ Used to easily wrap around rules, to provide
    an isolated context set for execution
    """
    def wrapped(self, *the_args, **the_kwargs):
        semSys = the_args[1]
        ctxs   = the_kwargs['ctxs']
        temp_container = semSys.build_ctxset(ops=ctxs._operators)
        temp_container.set_parent(ctxs)
        the_kwargs['ctxs'] = temp_container
        return f(self, *the_args, **the_kwargs)

    wrapped.__name__ = f.__name__
    return wrapped

def SemanticOperatorWrapDecorator(f):
    """ Use to simplify extracting raw values for use in operators,
    and wrapping the results into AcabValues """
    def wrapped(self, *the_args, **the_kwargs):
        unwrapped_args = [x.value for x in the_args]
        result = f(self, *unwrapped_args, **the_kwargs)
        return AcabValue.safe_make(result)

    wrapped.__name__ = f"SOWD({f})"
    return wrapped

def SemanticUnWrapData(f):
    """ Use to simplify extracting raw values for use in operators,
    and wrapping the results into AcabValues """
    def wrapped(self, *the_args, **the_kwargs):
        if 'data' in the_kwargs:
            unwrapped_data = {x: y.value for x,y in the_kwargs['data'].items()}
            the_kwargs['data'] = unwrapped_data
        return f(self, *the_args, **the_kwargs)

    wrapped.__name__ = f"SUWD({f})"
    return wrapped


def SemanticTestWrapDecorator(f):
    """ Use to simplify extracting raw values for use in operators,
    and wrapping the results into AcabValues """
    def wrapped(self, *the_args, **the_kwargs):
        unwrapped_args = [x.value for x in the_args]
        return f(self, *unwrapped_args, **the_kwargs)

    wrapped.__name__ = f"STWD({f})"
    return wrapped


def _get_params(self, params, bound_context):
    """ Retrieve a value's parameters from a context dict """
    assert(isinstance(bound_context, dict))
    output = []
    # TODO: enable currying?
    for x in params:
        if isinstance(x, Sentence):
            output.append(x.bind(bound_context))
        elif isinstance(x, list):
            output.append([y.bind(bound_context) for y in x])
        elif isinstance(x, AcabValue) and x.is_var:
            assert(x.value in bound_context)
            if x.is_at_var:
                output.append(bound_context[AT_BIND_S + x.value])
            elif isinstance(bound_context[x.value], list):
                # TODO does this need to unwrap all list values?
                output.append(bound_context[x.value])
            else:
                output.append(bound_context[x.value].value)
        else:
            output.append(x.value)
    return output

def _filter_candidates(self, target_pattern, candidates, match_func):
    """ Filter candidates using match_func to compare
    against this data_structure

    Where a Match = [(PatternNode, MatchNode)]
    Return [Match]

    match_func : Node -> [Node] -> [Node]
    """
    # TODO check this
    assert(isinstance(target_pattern, AcabStruct))

    if isinstance(candidates, AcabStruct):
        candidates = candidates.root

    if not isinstance(candidates, AcabNode):
        raise AcabBaseException()

    final_matches = []
    pattern_nodes = list(candidates.children.values())
    # (current pattern position, available choices, match state)
    queue = [(word, pattern_nodes, []) for word in target_pattern.root.children.values()]

    while bool(queue):
        current, available_nodes, match_state = queue.pop(0)

        matching_nodes = match_func(current, available_nodes)
        for node in matching_nodes:
            next_match_state = match_state + [(current, node)]

            if bool(current):
                next_available = list(node.children.values())
                next_patterns = list(current.children.values())
                queue += [
                    (word, next_available, next_match_state)
                    for word in next_patterns
                ]
            else:
                final_matches.append(next_match_state)

    return final_matches





# Stub decorator to override
SemanticBreakpointDecorator = lambda f: f

if "Module.Debug" in config:
    mod = config.prepare("Module.Debug", "IMPORT", actions=[config.actions_e.IMPORT])()
    decorator_name = config.prepare("Module.Debug", "BREAK_DECORATOR")()
    SemanticBreakpointDecorator = getattr(mod, decorator_name)
