"""
Events express when a value holds in time
"""
from .arc import Arc
from fractions import Fraction
import logging as root_logger

logging = root_logger.getLogger(__name__)

class Event:
    """ A Value active during a timespan """

    def __init__(self, a, b, value_is_pattern=False,
                 params=None):
        assert(isinstance(a, Arc))
        self.arc = a.copy()
        self.values = b
        self.parameters = {}
        self.value_is_pattern = value_is_pattern

        if params is not None:
            self.parameters.update(params)


    def __call__(self, count, just_values=False, rand_s=None):
        """ Get a list of events given a time """
        if count in self.arc:
            if self.value_is_pattern:
                event_range = self.arc.size()
                offset_count = count - self.arc.start
                scaled_count = offset_count / event_range
                return self.values(scaled_count, just_values)
            else:
                return [self]
        return []

    def __contains__(self, other):
        return other in self.arc

    def __repr__(self):
        return "{} :: {}".format(str(self.values), str(self.arc))

    def __getitem__(self, val):
        """ event[x] """
        return self.parameters[val]

    def base(self):
        """ Get all fractions used in this event """
        time_list = self.arc.pair()
        size =  self.arc.size()
        if self.value_is_pattern:
            time_list += [(x * size) - self.arc.start  for x in self.values.base()]
        return set(time_list)

    def key(self):
        """ Get the start of the event, for sorting """
        return self.arc.start

    def print_flip(self, start=True):
        """ Get a string describing the event's entry/exit status """
        fmt_str ="⤒{} "
        if not start:
            fmt_str = "{}⤓"
        return fmt_str.format(str(self.values))

