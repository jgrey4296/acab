from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from os.path import abspath, exists, expanduser, split
import abc
from dataclasses import dataclass, field

from acab.abstract.interfaces.dsl_interface import DSL_Fragment_i, DSLBuilder_i
from acab.abstract.interfaces.semantic_interfaces import SemanticSystem_i
from acab.abstract.interfaces.printing_interfaces import PrintSystem_i
from acab.abstract.interfaces.module_loader_interface import ModuleLoader_i


# Decorator for Engine:
def EnsureInitialised(method):
    def fn(self, *args, **kwargs):
        if not self.initialised:
            raise AcabBaseException("DSL Not Initialised")

        return method(self, *args, **kwargs)

    fn.__name__ = method.__name__
    return fn

@dataclass
class AcabEngine_i(metaclass=abc.ABCMeta):

    # Root components to extend
    parser         : DSL_Fragment_i          = field()
    semantics      : SemanticSystem_i         = field()
    printer        : PrintSystem_i            = field()

    # Modules to load
    modules        : List[str]              = field(default_factory=list)
    # Files to load
    load_paths     : List[str]              = field(default_factory=list)
    init_strs      : List[str]              = field(default_factory=list)

    initialised    : bool                   = field(init=False, default=False)
    # Abstract fields, need to be instantiated:
    _dsl_builder   : DSLBuilder_i   = field(init=False)
    _module_loader : ModuleLoader_i = field(init=False)

    @EnsureInitialised
    def load_file(self, filename) -> bool:
        """ Given a filename, read it, and interpret it as an EL DSL string """
        assert(isinstance(filename, str))
        filename = abspath(expanduser(filename))
        logging.info("Loading: {}".format(filename))
        assert exists(filename), filename
        with open(filename) as f:
            # everything should be an assertion
            try:
                assertions = self._dsl_builder.parseFile(f)
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

    def save_file(self, filename:str, printer:PrintSystem_i=None):
        """ Dump the content of the kb to a file to reload later """
        assert(exists(split(abspath(expanduser(filename)))[0]))
        if printer is None:
            printer = self.printer

        as_sentences = self.semantics.to_sentences()
        as_strings = printer.pprint(*as_sentences)

        with open(abspath(expanduser(filename)), 'w') as f:
            f.write(as_strings)



    def to_sentences(self):
        """
        Triggers the working memory to produce a full accounting,
        in canonical style (able to be used by typechecker)
        All statements are output as leaves,
        and all paths with non-leaf statements convert to simple formats
        """
        return self.semantics.to_sentences()

    def pprint(self) -> str:
        sens = self.to_sentences()
        return self.printer.pprint(*sens)

    @abc.abstractmethod
    def __call__(self, thing, bindings=None):
        pass
