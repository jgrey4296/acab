"""
PatternIterator automates pattern output
"""
from fractions import Fraction as f
from .utils import time_str
import logging as root_logger
logging = root_logger.getLogger(__name__)

class PatternIterator:
    """ Automates retrieval of pattern values  """

    def __init__(self, pattern, just_values=True, rand_s=None):
        self.pattern = pattern
        self.denominator = self.pattern.denominator()
        self.position = 0
        self.just_values = just_values
        self.rand_state = rand_s

    def __iter__(self):
        return self

    def __next__(self):
        time = f(self.position, self.denominator)
        self.position += 1
        #if at a new cycle, tick the random generator
        return self.pattern(time, just_values=self.just_values, rand_s=self.rand_state)

    #raise StopIteration
