""" A Base class for time structures """
import logging as root_logger
from fractions import Fraction
from functools import reduce
from math import floor
from random import Random

from py_rule.abstract.value import PyRuleValue
from py_rule.abstract.printing import util as PrU
from .pattern_iterator import PatternIterator

from . import util
from .util import TIME_T, Time, f_gcd, PATTERN_S

logging = root_logger.getLogger(__name__)


class BaseTime(PyRuleValue):

    def __init__(self, arcTuple, data=None, type_str=None):
        assert(isinstance(arcTuple, (list, tuple)))
        assert(all([isinstance(x, Fraction) for x in arcTuple]))
        assert(arcTuple[0] < arcTuple[1])
        super(BaseTime, self).__init__(arcTuple, data=data, type_str=type_str)

    def __contains__(self, other):
        """ Test whether the given time is within bounds """
        test = other
        if not isinstance(other, Fraction):
            assert(isinstance(other, BaseTime))
            test = other.start
        return self.start <= test < self.end

    def __eq__(self, other):
        return self.start == other.start and self.end == other.end

    @property
    def arc(self):
        return (self.start, self.end)

    @property
    def start(self):
        return self._value[0]

    @property
    def end(self):
        return self._value[1]

    @property
    def key(self):
        """ Get the start of the event, for sorting """
        return self.start

    @property
    def base(self):
        the_set = set(self.arc)
        event_size = self.size()
        if isinstance(self.event, TimeContainer):
            the_set.update([(event_size * x) - self.start for x in self.event.base])
        return the_set


    def set_arc(self, value):
        arc_tuple = self.arc
        if isinstance(value, BaseTime):
            arc_tuple = value.arc
        elif isinstance(value, (tuple, list)):
            arc_tuple = value

        assert(arc_tuple[0] < arc_tuple[1])
        self._value = arc_tuple
        return self

    def size(self):
        """ Get the length of time the arc describes """
        return self.end - self.start

    def bound(self, other):
        assert(isinstance(other, BaseTime))
        start = min(self.start, other.start)
        end = max(self.end, other.end)
        return BaseTime((start, end))

    def pprint(self):
        return "BaseTime"


class TimeEvent(BaseTime):

    def __init__(self, arcTuple, event, data=None, type_str=util.TIME_EVENT_S):
        super(TimeEvent, self).__init__(arcTuple, data=data, type_str=type_str)
        self._event = event
        assert(not (isinstance(event, TimeContainer) and self.is_var))

    def __call__(self, count, just_values=False, rnd_s=None):
        """ Get a list of events given a time """
        if not count in self:
            return []

        if not isinstance(self.event, TimeContainer):
            return [self]

        event_range = self.size()
        offset_count = count - self.start
        scaled_count = offset_count / event_range
        return self._event(scaled_count, just_values, rnd_s)


    @property
    def event(self):
        return self._event

    @property
    def var_set(self):
        var_set = {'in' : set(), 'out': set()}
        if self.is_var:
            var_set['in'].add(self.event)
        elif isinstance(self.event, PyRuleValue):
            update_set = self.event.var_set
            var_set['in'].update(update_set['in'])
            var_set['out'].update(update_set['out'])

        return var_set


    def pprint(self, wrap=False):
        if isinstance(self._event, PyRuleValue):
            value = self._event.pprint()
        else:
            value = str(self._event)

        if self.is_var:
            value = PrU._wrap_var(value)
        if util.OPT_S in self._data and self._data[util.OPT_S]:
            value = PrU._wrap_question(value)

        return value

    def bind(self, bindings):
        assert(self.is_pure())
        copied = self.copy()
        if self.is_var and self.event in bindings:
            copied._event = bindings[self.event]
            copied._data[util.BIND_S] = False

        return copied

    def is_pure(self):
        return not isinstance(self.event, TimeContainer)


class TimeContainer(BaseTime):
    """ The Core Time Structure, describing an rational arc of time """

    def __init__(self, arcTuple, events=None, data=None, type_str=util.TIME_PATTERN_S):
        if events is None:
            events = []
        assert(all([isinstance(x, BaseTime) for x in events]))
        sorted_events= sorted(events, key=lambda x: x.key)
        super(BaseTime, self).__init__(arcTuple,
                                       data=data,
                                       type_str=type_str)
        self._events = sorted_events
        self._wrap_template = "[{}]"
        self._join_template = " "
        self._time_type = TIME_T.CLOCK

    def __eq__(self, other):
        # TODO: check arc as well?
        assert(isinstance(other, TimeContainer))
        return all([x == y for x, y in zip(self.events, other.events)])

    def __getitem__(self, val):
        """ event[x] """
        return self._data[val]

    def __add__(self, other):
        """ Concatenates two patterns together, without compression """
        raise NotImplementedError()

    def __sub__(self, other):
        """
        Treat as a Parallel pattern where if an event occurs in both streams,
        it is silenced
        """
        raise NotImplementedError()

    def __mul__(self, other):
        """ Stacks two patterns together """
        raise NotImplementedError()

    def __iter__(self):
        return self.iter()

    def __call__(self, count, just_values=False, rnd_s=None, bindings=None):
        """ Query the Pattern for a given time """
        scaled_position = self.scale_time(count)
        results = []
        for x in self.events:
            results += x(scaled_position, False, rnd_s)
        return self.handle_call_results(results, just_values, bindings=bindings)


    @property
    def events(self):
        return self._events

    @property
    def denominator(self):
        base_count = reduce(f_gcd, self.base, 2).denominator
        return base_count

    @property
    def base(self):
        """ Get all fractions used in this pattern """
        time_list = set(self.arc)
        size = self.size()

        for x in self.events:
            x_base = x.base
            time_list.update([(y * size) - self.start for y in x_base])

        return set(time_list)

    @property
    def var_set(self):
        var_set = {'in': set(), 'out' : set()}
        for x in self.events:
            temp_set = x.var_set
            var_set['in'].update(temp_set['in'])
            var_set['out'].update(temp_set['out'])

        return var_set


    def is_pure(self):
        """ Where purity is defined as being a simple
        value, not a Pattern
        Currently this is equivalent to being a string
        """
        return all([isinstance(x, TimeEvent) for x in self.events])

    def bind(self, bindings):
        rebinds = [x.bind(bindings) for x in self.events]
        copied = self.__class__(self.arc, rebinds, self._data)
        return copied

    def scale_time(self, count):
        pattern_range = self.size()
        f_count = floor(count)
        position = count - (f_count * (f_count >= pattern_range))
        scaled_position = position / pattern_range
        return scaled_position

    def iter(self, just_values=True, rnd_s=None, cycles=1, count=None):
        """ Treat the pattern as an iterator """
        return PatternIterator(self,
                               just_values=just_values,
                               rnd_s=rnd_s,
                               cycles=cycles,
                               count=count)

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

        most_simultaneous = max([x.size() for x in collection])
        rows = [[] for x in range(most_simultaneous)]
        for x in collection:
            len_x = x.size()
            for j, r in enumerate(rows):
                if j < len_x:
                    r.append(x[j])
                else:
                    r.append('*')

        for row in rows:
            output += "|{}|\n".format("".join(row))

        return output

    def compress(self, other):
        """ Compress two patterns to run in the same arc as self._arc """
        return Pattern(self._arc, [self, other])

    def handle_call_results(self, results, just_values=False, bindings=None):
        assert(all([isinstance(x, TimeEvent) for x in results]))
        if bool(bindings):
            results = [x.bind(bindings) for x in results]

        if just_values:
            results = [x._event for x in results]

        return results

    def pprint(self, wrap=False):
        needs_wrapping = not self.is_pure()
        comps = [x.pprint(needs_wrapping) for x in self.events]

        joined = self._join_template.join(comps)
        if wrap:
            return self._wrap_template.format(joined)
        else:
            return joined

