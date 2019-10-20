"""
Arcs express segments of rational time
"""

from fractions import Fraction
from .utils import time_str
import logging as root_logger
logging = root_logger.getLogger(__name__)


class Arc:
    """ A segment of rational time """

    def __init__(self, a, b):
        assert(isinstance(a, Fraction))
        assert(isinstance(b, Fraction))
        assert(a < b)
        self._start = a
        self._end = b

    def __contains__(self, other):
        """ Test whether the given time is within bounds """
        test = other
        if not isinstance(other, Fraction):
            assert(hasattr(other, '_arc'))
            test = other._arc._start
        assert(isinstance(test, Fraction))
        return self._start <= test and test < self._end

    def __repr__(self):
        """ A Readable format of an arc """
        return "({}..{})".format(time_str(self._start),
                                 time_str(self._end))

    def __eq__(self, other):
        assert(isinstance(other, Arc))
        return all([x == y for x,y in zip(self.pair(), other.pair())])


    def key(self):
        return self._start
    def copy(self):
        return Arc(self._start, self._end)

    def pair(self):
        """ Treat the arc as a list """
        return [self._start, self._end]

    def size(self):
        """ Get the length of time the arc describes """
        return self._end - self._start

    def bound(self, other):
        assert(isinstance(other, Arc))
        start = min(self._start, other._start)
        end = max(self._end, other._end)
        return Arc(start, end)
