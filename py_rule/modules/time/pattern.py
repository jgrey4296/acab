"""
A Pattern collects events together and cycles them
"""
import logging as root_logger
from fractions import Fraction
from functools import reduce
from math import floor
from random import Random

from py_rule.abstract.value import PyRuleValue
from py_rule.modules.time.arc import Arc
from py_rule.modules.time.event import Event

from .pattern_iterator import PatternIterator
from .util import TIME_T, Time, f_gcd

logging = root_logger.getLogger(__name__)


# TODO: binary tree / beachline for finding events?
class Pattern(PyRuleValue):
    """ A Collection of Events """


    def __init__(self, a, vals=None, data=None, bindings=None):
        if vals is None:
            vals = []
        self._arc = a.copy()
        # components :: [ Event || Pattern ]
        self._components = sorted(vals, key=lambda x: x.key())
        self._time_type = TIME_T.CLOCK
        self._data = {}
        self._bindings = {}
        self._var_set = set()
        self._wrap_template = "[{}]"
        self._join_template = " "

        if data is not None:
            self._data.update(data)
        if bindings is not None:
            self._bindings.update(bindings)
        [self._var_set.update(x.var_set()) for x in self._components]

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
        return self.handle_call_results(results, just_values)

    def __contains__(self, other):
        """ Test whether a given object
        or time is within this patterns bounds
        """
        return other in self._arc

    def __add__(self, other):
        """ Concatenates two patterns together, without compression """
        return PatternSeq(self._arc,
                          [self, other])

    def __sub__(self, other):
        """
        Treat as a Parallel pattern where if an event occurs in both streams,
        it is silenced
        """
        return None

    def __mul__(self, other):
        """ Stacks two patterns together """
        assert(isinstance(other, Pattern))
        return PatternPar(self._arc,
                          [self, other])

    def __iter__(self):
        return self.iter()

    def handle_call_results(self, results, just_values=False):
        assert(all([isinstance(x, Event) for x in results]))
        if bool(self._bindings):
            results = [x.bind(self._bindings) for x in results]

        if just_values:
            results = [x._value for x in results]

        return results

    def copy(self, deep=False):
        """ Copy the pattern for modification """
        if deep:
            vals = [x.copy(True) for x in self._components]
        else:
            vals = self._components
        self_class = self.__class__
        copied = self_class(self._arc, vals, self._data, self._bindings)
        copied._time_type = self._time_type
        return copied

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
            for j, r in enumerate(rows):
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
        return self._arc.key()

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

    def iter(self, just_values=True, rnd_s=None, cycles=1, count=None):
        """ Treat the pattern as an iterator """
        return PatternIterator(self,
                               just_values=just_values,
                               rnd_s=rnd_s,
                               cycles=cycles,
                               count=count)

    def bind(self, a_dict):
        """ Apply a substitution dictionary to variables in the pattern """
        copied = self.copy()
        copied._bindings.update(a_dict)
        return copied

    def apply_to(self, other):
        """ Combine two patterns, using the structure of left one """
        raise NotImplementedError("Not implemented yet")

    def var_set(self):
        return self._var_set

    def compress(self, other):
        """ Compress two patterns to run in the same arc as self._arc """
        return Pattern(self._arc, [self, other])


class PatternSeq(Pattern):

    def __init__(self, a, vals=None, data=None, bindings=None):
        assert(all([isinstance(x, Pattern) for x in vals]))
        flat_vals = []
        for x in vals:
            if isinstance(x, PatternSeq):
                flat_vals += x._components
            else:
                flat_vals.append(x)

        super().__init__(a, flat_vals, data, bindings)
        self._wrap_template = "[{}]"
        self._join_template = " -> "

    def __call__(self, count, just_values=False, rnd_s=None):
        """ Query the Pattern for a given time """
        scaled_position = self.scale_time(count)
        f_count = floor(count)
        mod_f = f_count % len(self._components)
        vals = self._components[mod_f](scaled_position, False, rnd_s)
        return self.handle_call_results(vals, just_values)

    def is_pure(self):
        return True


class PatternPar(Pattern):
    def __init__(self, a, vals=None, data=None, bindings=None):
        assert(all([isinstance(x, Pattern) for x in vals]))
        super().__init__(a, vals, data, bindings)
        self._wrap_template = "[{}]"
        self._join_template = ", "

    def __call__(self, count, just_values=False, rnd_s=None):
        scaled_position = self.scale_time(count)
        results = []
        for x in self._components:
            results += x(scaled_position, False, rnd_s)

        return self.handle_call_results(results, just_values)

    def is_pure(self):
        return True

    def visualise(self, headless=False, base_count=None):
        if base_count is None:
            base_count = self.denominator()

        div_line = "-" * base_count
        output = "\n|" + div_line + "|\n"
        if headless:
            output = ""

        collection = [x.visualise(True, base_count) for x in self._components]
        output += "/{}/\n".format(div_line).join(collection)
        return output


class PatternChoice(Pattern):
    def __init__(self, a, vals=None, data=None, bindings=None):
        components = [x.copy() for x in vals]
        base_arc = Arc(Time(0, 1), Time(1, 1))
        vals = [Event(base_arc, x, True) if isinstance(x, Pattern)
                else x.set_arc(base_arc) for x in components]
        super().__init__(a, vals, data, bindings)
        self._wrap_template = "<{}>"
        self._join_template = " "

    def __call__(self, count, just_values=False, rnd_s=None):
        """ When called chooses from one of the options,
        using the random_state """
        scaled_position = self.scale_time(count)
        f_count = floor(count)
        rnd = Random(f_count)
        the_choice = rnd.choice(self._components)
        results = the_choice(scaled_position, False, rnd_s)

        return self.handle_call_results(results, just_values)

    def pprint(self, wrap=False):
        needs_wrapping = not self.is_pure()
        comps = [x.pprint(needs_wrapping) for x in self._components]

        joined = self._join_template.join(comps)
        return self._wrap_template.format(joined)
