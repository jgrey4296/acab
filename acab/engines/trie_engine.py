"""
The combined engine of underlying trie based working memory,
with support for transforms and actions
"""
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

import logging as root_logger
from types import ModuleType
from os.path import exists, expanduser
from os.path import abspath
import pyparsing as pp
import re

from acab.abstract.config.config import AcabConfig
from acab.error.acab_parse_exception import AcabParseException

from acab.abstract.core.values import Sentence

from acab.abstract.containers.production_abstractions import ProductionOperator
from acab.abstract.interfaces.dsl_interface import DSL_Interface
from acab.abstract.engine.engine import Engine

from acab.working_memory.trie_wm.trie_working_memory import TrieWM
from acab.working_memory.trie_wm.parsing import TotalParser as TotalP

logging = root_logger.getLogger(__name__)

config = AcabConfig.Get()

MODULE_SPLIT_REG = re.compile(config.value("Parse.Patterns", "MODULE_SPLIT_REG"))

# TODO Deprecate this, move code into abstract.engine, or a working memory instance
class TrieEngine(Engine):
    """ The Engine for an Agent.
    Holds a working memory, with rules, keeps track of proposed actions
    and the history of the agent. Performs actions that are registered
    """

    def __init__(self, modules=None, init=None):
        super().__init__(TrieWM, modules=modules, init_strs=init)

    def _load_file(self, filename):
        """ Given a filename, read it, and interpret it as an EL DSL string """
        assert(isinstance(filename, str))
        filename = abspath(expanduser(filename))
        logging.info("Loading: {}".format(filename))
        assert exists(filename), filename
        with open(filename) as f:
            # everything should be an assertion
            try:
                # TODO switch this to a working memory call
                assertions = TotalP.parseFile(f)
            except pp.ParseException as exp:
                print("-----")
                print(str(exp))
                print(exp.markInputline())
                print("File Not Asserted into WM")
                return False

            # Assert facts:
            for x in assertions:
                logging.info("File load assertions: {}".format(x))
                self.add(x)

        return True


    def register_ops(self, sentences: List[Sentence]):
        """
        Take a list of tuples: [(Sentence, Operator)], and add
        the operators as the leaves of the sentence
        """
        assert(isinstance(sentences, list))
        # TODO convert strings
        assert(all([isinstance(x, Sentence) for x in sentences])), breakpoint()

        # TODO: error on duplication
        for x in sentences:
            self._working_memory.add(x)


    def get_operator(self, op_sen):
        """
        Query for an operator, using the sentence.
        If the sentence triggers an alias,
        The alias path is replaced by the full path,
        and retrieval starts anew
        """
        if isinstance(op_sen, ProductionOperator):
            return op_sen

        assert(isinstance(op_sen, Sentence))

        # Attempt alias expansion first
        head = op_sen[0:1]
        alias_result = self._working_memory.query(head)
        if bool(alias_result) and isinstance(alias_result.nodes[0].value, Sentence):
            alias_words = alias_result.nodes[0].value.words
            op_sen = Sentence.build(alias_words + op_sen[1:].words)

        # extract operator from return context
        op_result = self._working_memory.query(op_sen)
        if bool(op_result):
            op = op_result.nodes[0].value
            assert(isinstance(op, ProductionOperator))
            return op

        raise Exception("Operator Not Found: {}".format(str(op_sen)))

    def alias_module(self, sentence, alias_sentence):
        """
        Assert the sentence at the alias
        """
        assert(isinstance(sentence, Sentence))
        assert(isinstance(alias_sentence, Sentence))
        assert(len(alias_sentence) == 1)
        self._working_memory.add(alias_sentence, leaf=sentence)

    def extract_from_module(self, module: ModuleType) -> Tuple[List[Sentence], List['DSL_Fragment']]:
        """
        DFS on a module to retrieve dsl fragments and operators
        Only searches descendents of the original module,
        and only those descendents' __init__ files.
        """
        base_path = module.__package__
        reference_path = MODULE_SPLIT_REG.split(module.__name__)
        queue = [(base_path, module)]
        components = []
        dsl_fragments = []
        while bool(queue):
            curr_path, curr_mod = queue.pop(0)

            # Ignore dunders
            mod_contents = [(x, getattr(curr_mod, x)) for x in dir(curr_mod) if "__" not in x]

            # queue submodules
            sub_modules = [(y.__package__, y) for x,y in mod_contents if isinstance(y, ModuleType)]
            queue += [(x,y) for x,y in sub_modules if base_path in y.__package__ and "__init__" in y.__file__]

            # Get module dsl_fragments
            dsl_fragments += [y for x,y in mod_contents if isinstance(y, DSL_Interface)]
            dsl_fragments += [y() for x,y in mod_contents if (not y == DSL_Interface) and isinstance(y, type) and issubclass(y, DSL_Interface)]

            # Get operators:
            applicable_operator = lambda y: isinstance(y, ProductionOperator) or isinstance(y, type) and issubclass(y, ProductionOperator)
            op_needs_instantiation = lambda y: (not y == ProductionOperator) and isinstance(y, type) and issubclass(y, ProductionOperator)

            loc_op_pairs = [(reference_path + MODULE_SPLIT_REG.split(x), y) for x,y in mod_contents if applicable_operator(y)]
            instanced_operators = [(xs, y() if op_needs_instantiation else y) for xs, y in loc_op_pairs]

            sentences = [Sentence.build(xs).attach_statement(y) for xs, y in instanced_operators]
            components += sentences

        # TODO: load any values needed for the operators?

        return components, dsl_fragments
