"""
Defines the PatternConstructor, used in parsing

PatternConstructor is a contextual class (__enter__ and __exit__)
that values are added to.

Upon exit, it calculates the appropriate time arcs for events

"""
import IPython
from .arc import Arc
from .event import Event
from .pattern import Pattern
from collections import namedtuple
from enum import Enum
from fractions import Fraction as t
import logging as root_logger
logging = root_logger.getLogger(__name__)

# PStart/End : SubPattern
# PDual : layering
# CStart/End : Choice
# OP : Optional
# SIL : Silence
CTOR_ACT = Enum("Actions for Pattern Constructor",
                "PSTART PEND PDUAL CSTART CEND OP SIL")

#Placeholders:
PatternPH = namedtuple("PatternPH", "values")
ChoicePH = namedtuple("ChoicePH", "values")
ParallelPH = namedtuple("ParallelPH", "values")

def construct_pattern(tokens):
    """   """
    #init stack with a top level pattern placeholder
    stack = []
    balance_count = 0
    while bool(tokens):
        logging.debug("Tokens: {}".format(tokens))
        head = tokens.pop(0)
        logging.debug("Head: {}".format(head))
        logging.debug("Stack: {}".format(stack))
        if isinstance(head, CTOR_ACT):
            if any(head is x for x in [CTOR_ACT.PSTART,
                                       CTOR_ACT.CSTART]):
                balance_count += 1
                logging.debug("Starting a Pattern or Choice")
                stack.append([])

            elif head is CTOR_ACT.PEND:
                logging.debug("Pattern end")
                balance_count -= 1
                p_vals = stack.pop()
                if bool(stack) and isinstance(stack[-1], ParallelPH):
                    logging.debug("Part of a Parallel Pattern")
                    parPH = stack.pop()
                    parPH.values.append(p_vals)
                    p_vals = parPH

                if isinstance(p_vals, ParallelPH):
                    #is parallel, do for each in parallel
                    sub_vals = [prepare_pvals(x) for x in p_vals.values]
                    processed = ParallelPH(sub_vals)
                else:
                    processed = prepare_pvals(p_vals)

                if bool(stack):
                    stack[-1].append(processed)
                else:
                    stack.append(processed)

            elif head is CTOR_ACT.PDUAL:
                logging.debug("Parallel")
                #run patterns in parallel
                #get the current pattern
                curr = stack.pop()
                if not bool(stack) or not isinstance(stack[-1], ParallelPH):
                    logging.debug("Creating new ParallelPH")
                    newPH = ParallelPH([curr])
                    stack.append(newPH)
                    stack.append([])
                else:
                    logging.debug("Using Existing ParallelPH")
                    stack[-1].values.append(curr)
                    stack.append([])

            elif head is CTOR_ACT.CEND:
                logging.debug("Choice End")
                balance_count -= 1
                c_vals = stack.pop()
                placeholder = ChoicePH(c_vals)
                stack[-1].append(placeholder)

            elif head is CTOR_ACT.OP:
                logging.warning("OP not implemented yet")

            elif head is CTOR_ACT.SIL:
                stack[-1].append(head)
            else:
                logging.warning("Other: {}".format(head))
        else:
            #push to list on the top of the stack
            stack[-1].append(head)
        logging.debug("Post Stack: {}".format(stack))
        logging.debug("------")

    if (balance_count != 0):
        raise Exception("Brackets in patterns need to be balanced")
    assert(len(stack) == 1)
    #create the final pattern
    farc= Arc(t(0,1),t(1,1))
    finalPH = stack.pop()
    if isinstance(finalPH, ParallelPH):
        finalEvents = [Event(farc, Pattern(farc, x.values), True) for x in finalPH.values]
    elif isinstance(finalPH, list):
        finalEvents = [Event(farc, Pattern(farc, x.values), True) for x in finalPH]
    else:
        finalEvents = finalPH.values

    result= Pattern(farc, finalEvents)
    return result

def prepare_pvals(p_vals):
    """ Prepare pattern values for assembling into a pattern """
    logging.debug("Preparing: {}".format(p_vals))
    p_len = len(p_vals)
    time_vals = []
    #calc times
    for i,x in enumerate(p_vals):
        arc = Arc(t(i,p_len), t(i+1, p_len))
        val = None
        if isinstance(x, PatternPH):
            val = [Event(arc, Pattern(arc, x.values), True)]
        elif isinstance(x, ParallelPH):
            pats = [Pattern(arc,y.values) for y in x.values]
            val = [Event(arc, y, True) for y in pats]
        elif isinstance(x, ChoicePH):
            #TODO: Make a Choice Event
            logging.warning("Choice events aren't implemented yet")
        else:
            val = [Event(arc, x)]
        time_vals += val
    #create pattern placeholder
    placeholder = PatternPH(time_vals)
    #add to new top of stack
    logging.debug("Resulting Placeholder: {}".format(placeholder))
    return placeholder
