#!/usr/bin/env python3

import logging as root_logger
from enum import Enum

from acab.abstract.config.config import AcabConfig
from acab.abstract.core.values import AcabValue

logging = root_logger.getLogger(__name__)

config = AcabConfig.Get()

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
