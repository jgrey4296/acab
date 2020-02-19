"""
PatternIterator automates pattern output
"""
from fractions import Fraction as f
from .util import time_str
import logging as root_logger
logging = root_logger.getLogger(__name__)

class PatternIterator:
    """ Automates retrieval of pattern values  """

    def __init__(self, pattern, just_values=True, rnd_s=None, cycles=1, count=None):
        self._pattern = pattern
        self._denominator = self._pattern.denominator()
        self._position = 0
        self._just_values = just_values
        self._rnd_state = rnd_s
        self._target = count
        if self._target is None and cycles is not None:
            self._target = cycles * self._denominator

    def __iter__(self):
        return self

    def __next__(self):
        time = f(self._position, self._denominator)
        if self._target is not None and self._position >= self._target:
            raise StopIteration()
        #if at a new cycle, tick the random generator
        result = self._pattern(time, just_values=self._just_values, rnd_s=self._rnd_state)
        self._position += 1
        return result
