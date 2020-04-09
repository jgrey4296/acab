"""
A Pattern collects events together and cycles them
"""
import logging as root_logger
from math import floor
from random import Random

from .util import Time
from py_rule.modules.structures.time.time_core import TimeContainer, TimeEvent

logging = root_logger.getLogger(__name__)


# TODO: binary tree / beachline for finding events?
class PatternSeq(TimeContainer):

    def __init__(self, a, vals=None, data=None, bindings=None):
        assert(all([isinstance(x, TimeContainer) for x in vals]))
        flat_vals = []
        for x in vals:
            if isinstance(x, PatternSeq):
                flat_vals += x.events
            else:
                flat_vals.append(x)

        super().__init__(a, flat_vals, data, bindings)
        self._wrap_template = "[{}]"
        self._join_template = " -> "

    def __call__(self, count, just_values=False, rnd_s=None):
        """ Query the Pattern for a given time """
        scaled_position = self.scale_time(count)
        f_count = floor(count)
        mod_f = f_count % len(self.events)
        vals = self.events[mod_f](scaled_position, False, rnd_s)
        return self.handle_call_results(vals, just_values)

    def is_pure(self):
        return True


class PatternPar(TimeContainer):
    def __init__(self, a, vals=None, data=None, bindings=None):
        assert(all([isinstance(x, TimeContainer) for x in vals]))
        super().__init__(a, vals, data, bindings)
        self._wrap_template = "[{}]"
        self._join_template = ", "

    def __call__(self, count, just_values=False, rnd_s=None):
        scaled_position = self.scale_time(count)
        results = []
        for x in self.events:
            results += x(scaled_position, False, rnd_s)

        return self.handle_call_results(results, just_values)

    def is_pure(self):
        return True

    def visualise(self, headless=False, base_count=None):
        if base_count is None:
            base_count = self.denominator

        div_line = "-" * base_count
        output = "\n|" + div_line + "|\n"
        if headless:
            output = ""

        collection = [x.visualise(True, base_count) for x in self.events]
        output += "/{}/\n".format(div_line).join(collection)
        return output


class PatternChoice(TimeContainer):
    def __init__(self, a, vals=None, data=None, bindings=None):
        components = [x.copy() for x in vals]
        base_arc = (Time(0, 1), Time(1, 1))
        vals = [TimeEvent(base_arc, x) if isinstance(x, TimeContainer)
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
        the_choice = rnd.choice(self.events)
        results = the_choice(scaled_position, False, rnd_s)

        return self.handle_call_results(results, just_values)

    def pprint(self, wrap=False):
        needs_wrapping = not self.is_pure()
        comps = [x.pprint(needs_wrapping) for x in self.events]

        joined = self._join_template.join(comps)
        return self._wrap_template.format(joined)
