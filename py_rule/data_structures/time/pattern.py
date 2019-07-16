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

    def __call__(self, count, just_values=False, rnd_s=None):
        """ Query the Pattern for a given time """
        scaled_position = self.scale_time(count)
        results = []
        for x in self._components:
            results += x(scaled_position, False, rnd_s)

        assert(all([isinstance(x, Event) for x in results]))
        if just_values:
            results = [x._value for x in results]

        return results

    def __contains__(self, other):
        """ Test whether a given object or time is within this patterns bounds """
        return other in self._arc

    def __add__(self, other):
        """ Concatenates two patterns together """
        return PatternSeq(self._arc,
                          [self, other])

    def __sub__(self, other):
        """ Remove a pattern from another
        Equivalent to formatting the pattern with silence
        """
        return None

    def __mul__(self, other):
        """ Stacks two patterns together """
        assert(isinstance(other, Pattern))
        return PatternPar(self._arc,
                          [self, other])

    def visualise(self, headless=False, base_count=None):
        if base_count is None:
            base_count = self.denominator()

        output = "\n|" + ("-" * base_count) + "|\n"
        if headless:
            output = ""

        collection = []
        for y in range(base_count):
            q = self(Fraction(y, base_count), True)
            collection.append(q)

        most_simultaneous = max([len(x) for x in collection])
        rows = [[] for x in range(most_simultaneous)]
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

    def pprint(self, wrap=False):
        needs_wrapping = not self.is_pure()
        comps = [x.pprint(needs_wrapping) for x in self._components]

        joined = self._join_template.join(comps)
        if wrap:
            return self._wrap_template.format(joined)
        else:
            return joined

    def is_pure(self):
        """ Where purity is defined as having components of the
        same purity """
        events = {x.is_pure() for x in self._components}
        return len(events) == 1


    def scale_time(self, count):
        pattern_range = self._arc.size()
        f_count = floor(count)
        position = count - (f_count * (f_count >= pattern_range))
        scaled_position = position / pattern_range
        return scaled_position

    def key(self):
        """ Key the Pattern by its start time, for sorting """
        return self._arc._start

    def base(self):
        """ Get all used fractions within this arc, scaled appropriately by offset
        and pattern size """
        counts = set()
        pattern_range = self._arc.size()
        for x in self._components:
            counts.update([a * pattern_range for a in x.base()])
        return counts

    def denominator(self):
        base_count = reduce(f_gcd, self.base(), 2).denominator
        return base_count

    def iter(self, just_values=True, rnd_s=None):
        """ Treat the pattern as an iterator """
        return PatternIterator(self,
                               just_values=just_values,
                               rnd_s=rnd_s)

    def format(self, a_dict):
        """ Apply a substitution dictionary to variables in the pattern """
        return None

    def copy(self, deep=False):
        """ Copy the pattern for modification """
        raise Exception("not implemented yet")

    def apply_to(self, other):
        """ Combine two patterns, using the structure of left one """
        raise Exception("Not implemented yet")


class PatternSeq(Pattern):

    def __init__(self, a, vals=None):
        assert(all([isinstance(x, Pattern) for x in vals]))
        flat_vals = []
        for x in vals:
            if isinstance(x, PatternSeq):
                flat_vals += x._components
            else:
                flat_vals.append(x)

        super().__init__(a, flat_vals)
        self._wrap_template = "[{}]"
        self._join_template = " -> "

    def __call__(self, count, just_values=False, rnd_s=None):
        """ Query the Pattern for a given time """
        scaled_position = self.scale_time(count)
        f_count = floor(count)
        mod_f = f_count % len(self._components)
        return self._components[mod_f](scaled_position, just_values, rnd_s)


class PatternPar(Pattern):
    def __init__(self, a, vals=None):
        assert(all([isinstance(x, Pattern) for x in vals]))
        super().__init__(a, vals)
        self._wrap_template = "[{}]"
        self._join_template = ", "

    def __call__(self, count, just_values=False, rnd_s=None):
        scaled_position = self.scale_time(count)
        results = []
        for x in self._components:
            results += x(scaled_position, False, rnd_s)

        assert(all([isinstance(x, Event) for x in results]))
        if just_values:
            results = [x._value for x in results]

        return results

    def visualise(self, headless=False, base_count=None):
        if base_count is None:
            base_count = self.denominator()

        div_line = "-" * base_count
        output = "\n|" + div_line + "|\n"
        if headless:
            output = ""

        collection = [x._value.visualise(True, base_count) for x in self._components]
        output += "/{}/\n".format(div_line).join(collection)
        return output


class PatternChoice(Pattern):
    def __init__(self, a, vals=None):
        super().__init__(a, vals)
        self._wrap_template = "<{}>"
        self._join_template = " "

        components = [x.copy() for x in self._components]
        self._components = [Event(Arc(Time(0, 1), Time(1, 1)), x, True)
                            if isinstance(x, Pattern) else x for x in components]


    def __call__(self, count, just_values=False, rnd_s=None):
        """ When called chooses from one of the options,
        using the random_state """
        scaled_position = self.scale_time(count)
        f_count = floor(count)
        rnd = Random(f_count)
        the_choice = rnd.choice(self._components)
        results = the_choice(scaled_position, False, rnd_s)

        assert(all([isinstance(x, Event) for x in results]))
        if just_values:
            results = [x._value for x in results]

        return results


    def pprint(self, wrap=False):
        needs_wrapping = not self.is_pure()
        comps = [x.pprint(needs_wrapping) for x in self._components]

        joined = self._join_template.join(comps)
        return self._wrap_template.format(joined)
