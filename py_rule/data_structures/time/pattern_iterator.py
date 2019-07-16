"""
PatternIterator automates pattern output
"""
from fractions import Fraction as f
from .utils import time_str
import logging as root_logger
logging = root_logger.getLogger(__name__)

class PatternIterator:
    """ Automates retrieval of pattern values  """

    def __init__(self, pattern, just_values=True, rnd_s=None):
        self._pattern = pattern
        self._denominator = self._pattern.denominator()
        self._position = 0
        self._just_values = just_values
        self._rnd_state = rnd_s

    def __iter__(self):
        return self

    def __next__(self):
        time = f(self._position, self._denominator)
        self._position += 1
        #if at a new cycle, tick the random generator
        return self._pattern(time, just_values=self._just_values, rnd_s=self._rnd_state)

    #raise StopIteration
