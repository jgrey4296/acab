"""
A Pattern collects events together and cycles them
"""
from py_rule.data_structures.time.arc import Arc
from py_rule.data_structures.time.event import Event
from .pattern_iterator import PatternIterator
from .utils import TIME_T, f_gcd, Time
from fractions import Fraction
from functools import reduce
from math import floor
from random import Random
import IPython
import logging as root_logger

logging = root_logger.getLogger(__name__)

# TODO: binary tree / beachline for finding events?
class Pattern:
    """ A Collection of Events """

    def __init__(self, a, vals=None):
        if vals is None:
            vals = []
        self._arc = a.copy()
        # components :: [ Event || Pattern ]
        self._components = sorted(vals, key=lambda x: x.key())
        self._time_type = TIME_T.CLOCK
        self._data = {}
        self._wrap_template = "[{}]"
        self._join_template = " "

    def __str__(self):
        return "[{}]".format(self.pprint(True))

    def __repr__(self):
        return "Pattern({})".format(str(self))

    def __call__(self, count, just_values=False, rand_s=None):
        """ Query the Pattern for a given time """
        scaled_position = self.scale_time(count)
        results = []
        for x in self._components:
            results += x(scaled_position, False, rand_s)

        assert(all([isinstance(x, Event) for x in results]))
        if just_values:
            results = [x._value for x in results]

        return results

    def __contains__(self, other):
        """ Test whether a given object or time is within this patterns bounds """
        return other in self._arc

    def __add__(self, other):
        """ Concatenates two patterns together """
        l_comps = [self]
        r_comps = [other]
        if isinstance(self, PatternSeq):
            l_comps = self._components
        if isinstance(other, PatternSeq):
            r_comps = other._components

        # TODO: is this arc correct?
        return PatternSeq(self._arc,
                          l_comps + r_comps)

    def __sub__(self, other):
        """ Remove a pattern from another
        Equivalent to formatting the pattern with silence
        """
        return None

    def __mul__(self, other):
        """ Stacks two patterns together """
        l_comps = [self]
        r_comps = [other]
        if isinstance(self, PatternSeq):
            l_comps = self.components
        if isinstance(other, PatternSeq):
            r_comps = other.components

        return Pattern(self.arc,
                       l_comps + r_comps)


    def key(self):
        """ Key the Pattern by its start time, for sorting """
        return self.arc.start

    def base(self):
        """ Get all used fractions within this arc, scaled appropriately by offset
        and pattern size """
        counts = set()
        pattern_range = self.arc.size()
        for x in self.components:
            counts.update([a * pattern_range for a in x.base()])
        return counts

    def denominator(self):
        base_count = reduce(f_gcd, self.base(), 2).denominator
        return base_count

    def iter(self, just_values=True, rand_s=None):
        """ Treat the pattern as an iterator """
        return PatternIterator(self, just_values=just_values, rand_s=rand_s)

    def format(self, a_dict):
        """ Apply a substitution dictionary to variables in the pattern """
        return None

    def copy(self, deep=False):
        """ Copy the pattern for modification """
        return None

    def apply_to(self, other):
        """ Combine two patterns, using the structure of left one """
        return None


class PatternSeq(Pattern):
    def __call__(self, count, just_values=False, rand_s=None):
        """ Query the Pattern for a given time """
        f_count = floor(count)
        mod_f = f_count % len(self.components)
        return self.components[mod_f](count, just_values)


class PatternChoice(Pattern):
    def __call__(self, count, just_values=False, rnd_s=None):
        """ When called chooses from one of the options,
        using the random_state """
        return []


class PatternOptional(Pattern):
    def __call__(self, count, just_values=False, rnd_s=None):
        """ When called, either returns nothing, or
        calcs the components as normal """
        #if rand_s:
        # TODO: make call take a dict,
        # and return ([], {})?
        ## return []
        # else:
        # return super().__call__ ...
        return []

