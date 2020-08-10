"""
The combined engine of underlying trie based working memory,
with support for transforms and actions
"""
import logging as root_logger
from types import ModuleType
from os.path import exists, expanduser
from os.path import abspath
import pyparsing as pp

from acab.abstract.dsl_fragment import DSL_Fragment
from acab.error.acab_parse_exception import AcabParseException
from acab.abstract.sentence import Sentence
from acab.abstract.production_operator import ProductionOperator
from acab.abstract.engine import Engine
from acab.working_memory.trie_wm.trie_working_memory import TrieWM
from acab.working_memory.trie_wm.parsing import TotalParser as TotalP

logging = root_logger.getLogger(__name__)


class TrieEngine(Engine):
    """ The Engine for an Agent.
    Holds a working memory, with rules, keeps track of proposed actions
    and the history of the agent. Performs actions that are registered
    """

    def __init__(self, modules=None, path=None, init=None):
        super().__init__(TrieWM, modules=modules, path=path, init=init)

    def load_file(self, filename):
        """ Given a filename, read it, and interpret it as an EL DSL string """
        assert(isinstance(filename, str))
        filename = abspath(expanduser(filename))
        logging.info("Loading: {}".format(filename))
        assert exists(filename), filename
        with open(filename) as f:
            # everything should be an assertion
            try:
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


    def register_ops(self, sentences):
        assert(isinstance(sentences, list))
        assert(all([isinstance(x, (Sentence, str)) for x,y in sentences]))
        assert(all([isinstance(y, ProductionOperator) for x,y in sentences]))

        # TODO: error on duplication
        for x,y in sentences:
            self._operators.add(x, leaf=y)


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
        alias_result = self._operators.query(head)
        if bool(alias_result) and isinstance(alias_result.nodes[0].value, Sentence):
            alias_words = alias_result.nodes[0].value.words
            op_sen = Sentence.build(alias_words + op_sen[1:].words)

        # extract operator from return context
        op_result = self._operators.query(op_sen)
        if bool(op_result):
            op = op_result.nodes[0].value
            assert(isinstance(op, ProductionOperator))
            return op

        raise Exception("Operator Not Found: {}".format(op_sen.pprint(seq_join=".")))

    def alias_module(self, sentence, alias_sentence):
        """
        Assert the sentence at the alias
        """
        assert(isinstance(sentence, Sentence))
        assert(isinstance(alias_sentence, Sentence))
        assert(len(alias_sentence) == 1)
        self._operators.add(alias_sentence, leaf=sentence)

    def extract_from_module(self, module):
        """
        DFS on a module to retrieve dsl fragments and operators
        Only searches descendents of the original module,
        and only those descendents' __init__ files.
        """
        base_path = module.__package__
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
            dsl_fragments += [y for x,y in mod_contents if isinstance(y, DSL_Fragment)]
            dsl_fragments += [y() for x,y in mod_contents if (not y == DSL_Fragment) and isinstance(y, type) and issubclass(y, DSL_Fragment)]

            # Get operators:
            components += [(curr_path + "." + x,  y) for x,y in mod_contents if isinstance(y, ProductionOperator)]
            components += [(curr_path + "." + x,  y()) for x,y in mod_contents if (not y == ProductionOperator) and isinstance(y, type) and issubclass(y, ProductionOperator)]


        # TODO: load any values needed for the operators?

        return components, dsl_fragments
