#!/usr/bin/env python3

from enum import Enum

from acab.abstract.config.config import AcabConfig

import logging as root_logger
logging = root_logger.getLogger(__name__)

config = AcabConfig.Get()

# TODO RDFSemantics, ReteSemantics
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
