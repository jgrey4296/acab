"""
A Pattern collects events together and cycles them
"""

from .pattern_iterator import PatternIterator
from .utils import TIME_T
from fractions import Fraction, gcd
from functools import reduce
from math import floor
import IPython
import logging as root_logger

logging = root_logger.getLogger(__name__)

class Pattern:
    """ A Collection of Events """

    @staticmethod
    def lcm(a,b):
        """ Get the lowest common multiple for two fractions """
        x = abs(a)
        gcd_result = gcd(a,b)
        x_prime = x / gcd_result
        y = abs(b)
        result = x_prime * y
        return result

    def __init__(self, a, vals=None):
        if vals is None:
            vals = []
        self.arc = a.copy()
        # components :: [ Event || Pattern ]
        self.components = sorted(vals, key=lambda x: x.key())
        self.time_type = TIME_T.CLOCK

    # TODO: add a state for random generator

    def __repr__(self):
        base_count = self.denominator()
        collection = []
        for y in range(base_count):
            q = self(Fraction(y, base_count), True)
            collection.append(q)

        most_simultaneous = max([len(x) for x in collection])
        rows = [[] for x in range(most_simultaneous)]
        output = "\n|" + ("-" * base_count) + "|\n"
        for x in collection:
            len_x = len(x)
            for j,r in enumerate(rows):
                if j < len_x:
                    r.append(x[j])
                else:
                    r.append('*')

        for row in rows:
            output += "|{}|\n".format("".join(row))

        return output

    def __str__(self):
        """ Print in the same format the parser reads """
        return repr(self)

    def __call__(self, count, just_values=False, rand_s=None):
        """ Query the Pattern for a given time """
        pattern_range = self.arc.size()
        f_count = floor(count)
        position = count - (f_count * (f_count >= pattern_range))
        scaled_position = position / pattern_range
        results = []
        # TODO: this could probably be better as a binary tree / beachline
        for x in self.components:
            results += x(scaled_position)

        if just_values:
            results = [x.values for x in results]

        return results

    def __contains__(self, other):
        """ Test whether a given object or time is within this patterns bounds """
        return other in self.arc

    def __add__(self, other):
        """ Concatenates two patterns together """
        l_comps = [self]
        r_comps = [other]
        if isinstance(self, PatternSeq):
            l_comps = self.components
        if isinstance(other, PatternSeq):
            r_comps = other.components

        # TODO: is this arc correct?
        return PatternSeq(self.arc,
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
        #TODO: use https://stackoverflow.com/questions/49981286/
        base_count = reduce(gcd, self.base(), 2).denominator
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

