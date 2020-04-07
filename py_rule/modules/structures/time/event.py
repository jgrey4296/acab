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
        data = {util.BIND_S: False,
                util.OPT_S: False}
        if params is not None:
            data.update(params)

        assert(isinstance(a, Arc))
        assert(not (value_is_pattern and data[util.BIND_S]))
        super(Event, self).__init__((a.copy(), b), data=data)

        self._value_is_pattern = value_is_pattern

    @property
    def _arc(self):
        return self._value[0]

    @property
    def _event(self):
        return self._value[1]

    def __call__(self, count, just_values=False, rnd_s=None):
        """ Get a list of events given a time """
        if count in self._arc:
            if self._value_is_pattern:
                event_range = self._arc.size()
                offset_count = count - self._arc._start
                scaled_count = offset_count / event_range
                return self._event(scaled_count, just_values, rnd_s)
            else:
                return [self]
        return []

    def __contains__(self, other):
        return other in self._arc

    # def __str__(self):
    #     return self.pprint(True)

    # def __repr__(self):
    #     return "{} :: {}".format(str(self._event), str(self._arc))

    def __getitem__(self, val):
        """ event[x] """
        return self._data[val]

    def copy(self, deep=False):
        if deep:
            val = self._event.copy(True)
        else:
            val = self._event

        return Event(self._arc, val, self._value_is_pattern, self._data)

    def set_arc(self, arc):
        assert(isinstance(arc, Arc))
        new_value = (arc.copy(), self._event)
        self._value = new_value
        return self

    def base(self):
        """ Get all fractions used in this event """
        time_list = self._arc.pair()
        size = self._arc.size()
        if self._value_is_pattern:
            time_list += [(x * size) - self._arc._start
                          for x in self._event.base()]
        return set(time_list)

    def key(self):
        """ Get the start of the event, for sorting """
        return self._arc.key()

    def print_flip(self, start=True):
        """ Get a string describing the event's entry/exit status """
        fmt_str = "⤒{} "
        if not start:
            fmt_str = "{}⤓"
        return fmt_str.format(str(self._event))

    def pprint(self, wrap=False):
        head = ""
        tail = ""
        if self._params[util.BIND_S]:
            head = "$"
        if self._params[util.OPT_S]:
            tail = "?"
        if self._value_is_pattern:
            return "{}{}".format(self._event.pprint(wrap), tail)
        else:
            return "{}{}{}".format(head, str(self._event), tail)

    def is_pure(self):
        """ Where purity is defined as being a simple
        value, not a Pattern
        Currently this is equivalent to being a string
        """
        return isinstance(self._event, str)

    def bind(self, bindings):
        assert(self.is_pure())
        event = self._event
        if self._data[util.BIND_S] and self._event in bindings:
            event = bindings[self._event]

        copied = Event(self._arc,
                       event,
                       value_is_pattern=self._value_is_pattern,
                       params=self._data)
        return copied

    def var_set(self):
        if self._value_is_pattern:
            return self._event.var_set()
        elif self._data[util.BIND_S]:
            return set(self._event)
        else:
            return []
