"""
Defines the PatternConstructor, used in parsing

PatternConstructor is a contextual class (__enter__ and __exit__)
that values are added to.

Upon exit, it calculates the appropriate time arcs for events

"""
from enum import Enum
from fractions import Fraction as t
import logging as logmod

from acab.modules.structures.time.util import OPT_S

from .time_core import TimeEvent, TimeContainer
from .pattern import PatternChoice, PatternPar

logging = logmod.getLogger(__name__)

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
    Stack based parser
    create a new pattern, detecting as necessary parallels
    and choices
    """
    final_pattern_data = {OPT_S: False}
    patt_type = TimeContainer
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
        new_pattern = TimeContainer(((t(0, 1), t(1, 1))),
                                    [TimeEvent((t(i, ph_len),
                                                t(i+1, ph_len)),
                                               v, d) for i, (v, d) in enumerate(ph)])
        patterns.append(new_pattern)

    # wrap in main pattern
    final_pattern = None
    if patt_type == TimeContainer:
        final_pattern = patterns[0]
    elif len(patterns) == 1:
        final_pattern = patt_type(patterns[0].arc,
                                  patterns[0].events)

    elif len(patterns) > 1:
        final_pattern = patt_type((t(0, 1), t(1, 1)), patterns)
    return (final_pattern, final_pattern_data)
