""" The WME-based implementation of a rule engine """
import logging as root_logger
from pyRule.EngineBase import EngineBase
from pyRule import utils as util
from .FactBase import FactBase
logging = root_logger.getLogger(__name__)


class WMEEngine(EngineBase):
    """ The base class that wme and trie versions implement the interface of """

    def __init__(self, path=None, init=None):
        super().__init__(FactBase, path=path, init=init)


    def load_file(self, filename):
        """ Load a file spec for the facts / rules for this engine """
        raise Exception("Base Engine Stub")

    def registerRules(self, s):
        """ Register passed in rule specifications """
        raise Exception("Base Engine Stub")

    def add(self, s):
        """ Assert a new fact into the engine """
        raise Exception("Base Engine Stub")

    def retract(self, s):
        """ Remove a fact from the engine """
        raise Exception("Base Engine Stub")
