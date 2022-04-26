#!/usr/bin/env python3

import logging as logmod
from os.path import abspath, exists, expanduser, split
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast, TypeAlias)

from acab import types as AT
from acab.core.util.decorators.engine import EnsureEngineInitialised
from acab.error.semantic import AcabSemanticException
from acab.interfaces.engine import AcabEngine_i
from acab.interfaces.printing import PrintSystem_i

logging = logmod.getLogger(__name__)

ModuleComponents : TypeAlias = AT.ModuleComponents

class AcabEngineImpl(AcabEngine_i):
    @EnsureEngineInitialised
    def load_file(self, filename) -> bool:
        """ Given a filename, read it, and interpret it as an EL DSL string """
        assert(isinstance(filename, str))
        filename = abspath(expanduser(filename))
        logging.info("Loading: {}".format(filename))
        assertions = []
        # with open(filename) as f:
        # everything should be an assertion
        try:
            assertions = self._dsl.parse_file(filename)
            # Assert facts:
            for x in assertions:
                logging.info(f"File load assertion: {x}")
                self(x)
        except FileNotFoundError as err:
            logging.warning(f"{err}")
        except AcabSemanticException as err:
            logging.warning(f"Assertion Failed: {x}")

        return True

    def save_file(self, filename:str, *, printer:PrintSystem_i=None):
        """ Dump the content of the kb to a file to reload later """
        assert(exists(split(abspath(expanduser(filename)))[0]))
        if printer is None:
            printer = self.printer

        as_sentences = self.semantics.to_sentences()
        if not bool(as_sentences[0]):
            logging.info("Nothing to print")
            return

        as_strings = printer.pprint(*as_sentences)

        # TODO add modeline
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

    def pprint(self, *, target=None) -> str:
        """ Pass a value to the engine's printer """
        sens = target
        if sens is None:
            sens = self.to_sentences()

        if not bool(sens) or not bool(sens[0]):
            return ""

        return str(self.printer.pprint(*sens))

    def load_modules(self, *modules) -> list[ModuleComponents]:
        logging.info("Loading Modules")
        self._module_loader.load_modules(*modules)
        loaded_mods = list(self._module_loader.loaded_modules.values())
        # Initialise DSL
        self._dsl = self.dsl_builder([], [], [])
        self._dsl.register(self.parser)
        self._dsl.extend(loaded_mods)
        self._dsl.build()

        self.semantics.extend(loaded_mods)

        self.printer.extend(loaded_mods)

        # insert operator sentences
        logging.info("Asserting operators")
        ops = [y for x in loaded_mods for y in x.operators]
        self.semantics(*ops)

        # Now Load Text files:
        logging.info("Loading paths")
        for x in self.load_paths:
            self.load_file(x)

        return [self._module_loader[x] for x in modules]

