"""
Utils related to rational time
"""
from enum import Enum
from fractions import Fraction, gcd
from functools import reduce
import logging as root_logger

logging = root_logger.getLogger(__name__)

PATTERN_T = Enum("Pattern Type", "DISCRETE ANALOG")
TIME_T = Enum("Time Type", "CLOCK EVENT SET SYMBOLIC")

def time_str(time):
    return "{}/{}".format(time.numerator,time.denominator)

def run_pattern(pat, num_cycles=3, action=None):
    #TODO
    return None

def print_run_pattern(pat, num_cycles=3):
    """ Given a pattern, output the Events Per Cycle """
    #Calculate the subdivision necessary
    base_count = reduce(gcd, pat.base(), 2).denominator
    logging.info("Base Count: {}".format(base_count))
    #Track the active events
    curr_events = set()
    #Run a number of cycles:
    for x in range(num_cycles):
        logging.info("Cycle: {}".format(x))
        #Run the Cycle:
        for y in range(base_count):
            pos = (x * base_count) + y
            #Query the Pattern:
            q = pat(Time(pos, base_count))

            #Calculate the Event entries and exits
            turn_on = [x for x in q if x not in curr_events]
            turn_off = [x for x in curr_events if x not in q]
            #Update the active events set
            curr_events = set(q)

            #Output:
            turn_on_s = ", ".join([w.print_flip(True) for w in turn_on])
            turn_off_s = ", ".join([w.print_flip(False) for w in turn_off])
            logging.info("A: {} R: {} Events: {} {}".format(pos,
                                                            y,
                                                            turn_on_s,
                                                            turn_off_s))
        logging.info("----")


class TimeVar:

    def __init__(self, name):
        self.name = name
