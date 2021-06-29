from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from dataclasses import dataclass, field

from acab.abstract.config.config import GET, AcabConfig, ConfigSpec
from acab.abstract.interfaces.semantic_interfaces import PrintSemantics
from acab.abstract.printing import consts as PC
from acab.abstract.printing import wrappers as PW
import acab.abstract.core.default_structure as DS
import acab.abstract.interfaces.value_interfaces as VI
from acab.abstract.core.values import Sentence
from acab.abstract.printing import consts as DSYM


config = GET()

SEN_SEN = Sentence.build([DS.SENTENCE_PRIM])

# Independent
class BasicPrinter(PrintSemantics):
    """ Simply print the str of anything passed in """

    def __call__(self, to_print, top=None):
        return str(to_print.name)

class PrimitiveTypeAwarePrinter(PrintSemantics):

    def add_transforms(self):
        return [PW._maybe_wrap_str,
                PW._maybe_wrap_regex,
                PW._maybe_wrap_var]

    def __call__(self, to_print, top=None):
        curr_str = [str(to_print.name)]
        return self.run_transforms(to_print, curr_str)

class ModalAwarePrinter(PrintSemantics):

    def add_transforms(self):
        return [PW._maybe_wrap_str,
                PW._maybe_wrap_regex,
                PW._maybe_wrap_var]


    def __call__(self, to_print, top=None):
        curr_str = [str(to_print.name)]
        transformed = self.run_transforms(to_print, curr_str)
        # lookup modal to care about from top.
        # TODO handle multi-modals
        modal = top.check('MODAL')
        if modal in to_print.data:
            transformed.append(to_print.data[modal])

        return transformed


class UUIDAwarePrinter(PrintSemantics):
    def add_transforms(self):
        return [PW._maybe_wrap_str,
                PW._maybe_wrap_regex,
                PW._maybe_wrap_var]


    def __call__(self, to_print, top=None):
        curr_str = [str(to_print.name)]
        transformed = self.run_transforms(to_print, curr_str)

        final = ["(", str(to_print.uuid), " : "] + transformed + [")"]
        return final


class ConstraintAwareValuePrinter(PrintSemantics):
    def add_transforms(self):
        return [PW._maybe_wrap_str,
                PW._maybe_wrap_regex,
                PW._maybe_wrap_var]

    def __call__(self, to_print, top=None):
        return_list = []
        curr_str = [str(to_print.name)]
        return_list.append(self.run_transforms(to_print, curr_str))

        if DS.CONSTRAINT in to_print.data:
            # TODO write a list_wrap func
            return_list.append("(")
            for constraint in to_print.data[DS.CONSTRAINT][:-1]:
                return_list.append(constraint)
                return_list.append(", ")

            return_list.append(to_print.data[DS.CONSTRAINT][-1])
            return_list.append(")")

        modal = top.check('MODAL')
        if modal in to_print.data:
            return_list.append(to_print.data[modal])


        return return_list


# Dependent
class BasicSentenceAwarePrinter(PrintSemantics):

    def __call__(self, to_print, top=None):
        assert(to_print.type == SEN_SEN)
        return_list = []

        # TODO add head λ
        return_list += to_print.words[:-1]
        # TODO use top.override instead here:
        # TODO preserve the uuid
        last_word = to_print.words[-1].copy()

        # TODO detect modals
        modal = top.check('MODAL')
        if modal in last_word.data:
            del last_word.data[modal]
        return_list.append(last_word)

        # Handle query
        if DS.QUERY in to_print.data and to_print.data[DS.QUERY]:
            return_list.append(DSYM.QUERY_SYM)

        return return_list


class ProductionComponentPrinter(PrintSemantics):

    def __call__(self, to_print, top=None):
        result = []
        # if sugared,
        # if to_print.sugared:
        #     pass

        # else
        result.append(DSYM.FUNC_SYM)
        result.append(to_print.value)
        result.append(" ")
        overriden = [top.override("_:OVERRIDE_VAR", x) for x in to_print.params]
        result += PW._sep_list(self, to_print, overriden, sep=" ")
        # TODO lookup arrow
        if bool(to_print.rebind):
            result.append(DSYM.REBIND_SYM)
            rebind_copy = to_print.rebind.copy()
            modal = top.check('MODAL')
            if modal in last_word.data:
                del rebind_copy.data[modal]

            result.append(rebind_copy)

        return result


# Abstraction
class ContainerPrinter(PrintSemantics):
    """ Production Containers """

    def __call__(self, to_print, top=None):
        # TODO add newlines, and END statement
        return to_print.clauses

class StructurePrinter(PrintSemantics):
    """ Ordered structures """

    def __call__(self, to_print, top=None):
        # TODO define order, add newlines
        return to_print.structure["query"] \
            + to_print.structure['transform'] \
            + to_print.structure['action']


class ComplexTypePrinter(PrintSemantics):
    """ A Top Level Orhcestrator """

    def __call__(self, to_print, top=None):
        pass


@dataclass
class ConfigBackedSymbolPrinter(PrintSemantics):
    """ Use an AcabConfig for lookup of provided
    symbol tuples.
    """
    overrides : Dict[Any, str] = field(default_factory=dict)
    _config   : AcabConfig     = field(default_factory=GET)

    def __call__(self, value, top=None):
        # Look the value up in overrides
        if value in self.overrides:
            return self.overrides[value]

        return self._config.value(value)
