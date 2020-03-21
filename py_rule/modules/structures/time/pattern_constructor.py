"""
Defines the PatternConstructor, used in parsing

PatternConstructor is a contextual class (__enter__ and __exit__)
that values are added to.

Upon exit, it calculates the appropriate time arcs for events

"""
from .arc import Arc
from .event import Event
from .pattern import Pattern, PatternChoice, PatternPar
from enum import Enum
from fractions import Fraction as t
import logging as root_logger
from py_rule.util import OPT_S
logging = root_logger.getLogger(__name__)

# PStart/End : SubPattern
# PDual : layering
# CStart/End : Choice
# OP : Optional
# SIL : Silence
CTOR_ACT = Enum("Actions for Pattern Constructor",
                "PSTART PEND PDUAL CSTART CEND OP SIL")

END_MATCHES = {
    CTOR_ACT.PSTART: CTOR_ACT.PEND,
    CTOR_ACT.CSTART: CTOR_ACT.CEND
    }


def construct_pattern_simple(orig_tokens):
    """ orig_tokens::[(value | pattern, data) | CTOR_ACT]
    create a new pattern, detecting as necessary parallels
    and choices
    """
    final_pattern_data = {OPT_S: False}
    patt_type = Pattern
    # TODO: detect different pattern types?
    tokens = orig_tokens[0][:]

    # Detect Initial Pattern Type
    start = tokens.pop(0)
    if start is CTOR_ACT.CSTART:
        patt_type = PatternChoice
    elif CTOR_ACT.PDUAL in tokens:
        patt_type = PatternPar

    # Detect Option Notation
    if OPT_S in orig_tokens[0]:
        final_pattern_data[OPT_S] = True
        tokens.pop()
    end = tokens.pop()
    assert(end == END_MATCHES[start])
    # split on commas
    pattern_phs = []
    while (CTOR_ACT.PDUAL in tokens):
        index = tokens.index(CTOR_ACT.PDUAL)
        pattern_phs.append(tokens[:index])
        tokens = tokens[index:]
        tokens.pop(0)

    # Append everything following the last comma
    if bool(tokens):
        pattern_phs.append(tokens[:])
    assert(bool(pattern_phs))
    # construct patterns
    patterns = []
    for ph in pattern_phs:
        ph_len = len(ph)
        new_pattern = Pattern(Arc(t(0, 1), t(1, 1)),
                              [Event(Arc(t(i, ph_len),
                                         t(i+1, ph_len)),
                                     v,
                                     isinstance(v, Pattern),
                                     d) for i, (v, d) in enumerate(ph)])
        patterns.append(new_pattern)

    # wrap in main pattern
    final_pattern = None
    if patt_type == Pattern:
        final_pattern = patterns[0]
    elif len(patterns) == 1:
        final_pattern = patt_type(patterns[0]._arc,
                                  patterns[0]._components)

    elif len(patterns) > 1:
        final_pattern = patt_type(Arc(t(0, 1), t(1, 1)), patterns)
    return (final_pattern, final_pattern_data)
