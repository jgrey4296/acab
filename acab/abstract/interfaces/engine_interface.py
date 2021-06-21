"""
Provide a number of individual interfaces for top level Engine functionality
"""
import abc
from dataclasses import dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

from acab.abstract.interfaces.dsl_interface import DSL_Interface, Bootstrapper
from acab.abstract.parsing.TrieBootstrapper import TrieBootstrapper

ModuleType   = 'Module'
Parser       = 'Parser'
Sentence     = 'Sentence'
DSL_Fragment = 'DSL_Fragment'

@dataclass
class RewindEngineInterface(metaclass=abc.ABCMeta):
    """
    Describes how an engine can be reverted to a previous state
    """
    # TODO  update with reloadable state of working memory
    prior_states : List[List['Sentence']] = field(default_factory=list)
    # named recall states of past kb states
    recall_states : Dict[str, List['Sentence']]= field(default_factory=dict)

    def rewind(self, val:Optional[Union[int, str]]) -> None:
        raise NotImplementedError()



    def save_state(self, name: Optional[str]=None):
        """ Copy the current string representation of the working memory,
        and any associated data """
        # TODO replace this with a down
        self.prior_states.append(self._working_memory.to_sentences())
        if name is not None:
            self.recall_state[name] = self._working_memory.to_sentences()


@dataclass
class ModuleLoaderInterface(metaclass=abc.ABCMeta):
    """ Describes how an engine loads ACAB/py modules """
    _loaded_modules       : Set[Any]          = field(init=False, default_factory=set)
    modules               : List[str]         = field(default_factory=list)
    def reload_all_modules(self):
        loaded = list(self._loaded_modules)
        self._loaded_modules.clear()
        self._load_modules(loaded)

    def load_modules(self, *modules: List[str]):
        """ Given ModuleInterface objects,
        store them then tell the working memory to load them
        return a list of dictionaries
        """
        return [self.load_module_values(x) for x in modules]

    def load_module_values(self, module_str: str):
        """
        Load a module, extract operators and dsl fragments from it,
        put the operators into the operators store,
        register the dsl fragments for later use

        Returns a working_memory query result of the module
        """
        # Prepare path
        # TODO use utility constants for joining and query
        if not isinstance(module_str, str):
            breakpoint()
            raise Exception("TODO: handle sentence -> str")
        # print semantics: basic+word join of "."
        # mod_str = str(module_sen)

        # Return early if already loaded
        if module_str in self._loaded_modules:
            logging.info("Module already loaded: {}".format(module_str))
            # TODO extract node from return context?
            return self._working_memory.query(module_str + "?")

        # Load
        try:
            the_module = import_module(module_str)
            # Extract
            operator_sentences, dsl_fragments = self.extract_from_module(the_module)
        except ModuleNotFoundError as e:
            raise AcabImportException(module_str) from None


        # Register DSL Fragments
        self._loaded_DSL_fragments[module_str] = dsl_fragments
        self.register_ops(operator_sentences)

        self._loaded_modules.add(module_str)
        # TODO extract node from return context?
        return self._working_memory.query(module_str+ "?")

    def register_ops(self, sentences: List[Sentence]):
        """
        Take a list of sentences where the leaf of each is an operator,
        and add to the wm
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

    def extract_from_module(self, module: ModuleType) -> Tuple[List[Sentence], List[DSL_Fragment]]:
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


@dataclass
class DSLBuilderInterface(metaclass=abc.ABCMeta):
    """ describes engine assembly of a parser from DSL Fragments """
    root_fragment        : DSL_Interface   = field()

    _bootstrap_parser    : Bootstrapper    = field(init=False, default_factory=TrieBootstrapper)
    _main_parser         : Parser          = field(init=False)
    _query_parser        : Parser          = field(init=False)
    _parsers_initialised : bool            = field(init=False, default=False)
    _loaded_DSL_fragments : Dict[Any, Any] = field(init=False, default_factory=dict)


    def build_DSL(self):
        """
        Using currently loaded modules, rebuild the usable DSL parser from fragments
        """
        self.clear_bootstrap()
        self.construct_parsers_from_fragments()
        self.initialised = True

    def construct_parsers_from_fragments(self):
        """ Assemble parsers from the fragments of the wm and loaded modules """
        # assert base parser
        self.root_fragment.assert_parsers(self._bootstrap_parser)

        fragments = [y for x in self._loaded_DSL_fragments.values() for y in x]
        assert(all([isinstance(x, DSL_Interface) for x in fragments]))

        for x in fragments:
            #Populate the trie
            x.assert_parsers(self._bootstrap_parser)

        for x in fragments:
            # Now query and populate the modules
            x.query_parsers(self._bootstrap_parser)

        # then assign main and query parsers from the base parser
        main_p, query_p = self.root_fragment.query_parsers(self._bootstrap_parser)
        self._main_parser = main_p
        self._query_parser = query_p

    def clear_bootstrap(self):
        self._bootstrap_parser = self._bootstrap_parser.__class__()
