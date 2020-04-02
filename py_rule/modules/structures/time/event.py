"""
Events express when a value holds in time
"""
from fractions import Fraction
import logging as root_logger

from py_rule.abstract.value import PyRuleValue

from .arc import Arc
from . import util

logging = root_logger.getLogger(__name__)


class Event(PyRuleValue):
    """ A Value active during a timespan """

    def __init__(self, a, b, value_is_pattern=False, params=None):
        assert(isinstance(a, Arc))
        super(Event, self).__init__()
        self._arc = a.copy()
        self._value = b
        self._params = {util.BIND_S: False,
                        util.OPT_S: False}
        self._value_is_pattern = value_is_pattern
        if params is not None:
            self._params.update(params)
        assert(not (value_is_pattern and self._params[util.BIND_S]))

    def __call__(self, count, just_values=False, rnd_s=None):
        """ Get a list of events given a time """
        if count in self._arc:
            if self._value_is_pattern:
                event_range = self._arc.size()
                offset_count = count - self._arc._start
                scaled_count = offset_count / event_range
                return self._value(scaled_count, just_values, rnd_s)
            else:
                return [self]
        return []

    def __contains__(self, other):
        return other in self._arc

    def __str__(self):
        return self.pprint(True)

    def __repr__(self):
        return "{} :: {}".format(str(self._value), str(self._arc))

    def __getitem__(self, val):
        """ event[x] """
        return self._params[val]

    def copy(self, deep=False):
        if deep:
            val = self._value.copy(True)
        else:
            val = self._value

        return Event(self._arc, val, self._value_is_pattern, self._params)

    def set_arc(self, arc):
        assert(isinstance(arc, Arc))
        self._arc = arc.copy()
        return self

    def base(self):
        """ Get all fractions used in this event """
        time_list = self._arc.pair()
        size = self._arc.size()
        if self._value_is_pattern:
            time_list += [(x * size) - self._arc._start
                          for x in self._value.base()]
        return set(time_list)

    def key(self):
        """ Get the start of the event, for sorting """
        return self._arc.key()

    def print_flip(self, start=True):
        """ Get a string describing the event's entry/exit status """
        fmt_str = "⤒{} "
        if not start:
            fmt_str = "{}⤓"
        return fmt_str.format(str(self._value))

    def pprint(self, wrap=False):
        head = ""
        tail = ""
        if self._params[util.BIND_S]:
            head = "$"
        if self._params[util.OPT_S]:
            tail = "?"
        if self._value_is_pattern:
            return "{}{}".format(self._value.pprint(wrap), tail)
        else:
            return "{}{}{}".format(head, str(self._value), tail)

    def is_pure(self):
        """ Where purity is defined as being a simple
        value, not a Pattern
        Currently this is equivalent to being a string
        """
        return isinstance(self._value, str)

    def bind(self, bindings):
        assert(self.is_pure())
        copied = self.copy()
        if self._params[util.BIND_S] and self._value in bindings:
            copied._value = bindings[self._value]
        return copied

    def var_set(self):
        if self._value_is_pattern:
            return self._value.var_set()
        elif self._params[util.BIND_S]:
            return set(self._value)
        else:
            return []
