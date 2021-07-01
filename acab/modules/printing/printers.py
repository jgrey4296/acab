from dataclasses import dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import acab.abstract.core.default_structure as DS
import acab.abstract.interfaces.value_interfaces as VI
from acab.abstract.config.config import GET, AcabConfig, ConfigSpec
from acab.abstract.core.values import Sentence, AcabStatement
from acab.abstract.interfaces.printing_interfaces import PrintSemantics
from acab.abstract.printing import consts as DSYM
from acab.abstract.printing import wrappers as PW

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

        if DS.NEGATION in to_print.data and to_print.data[DS.NEGATION]:
            return_list.append(DSYM.NEGATION_SYM)

        return_list += to_print.words[:-1]
        if not isinstance(to_print.words[-1], AcabStatement):
            return_list.append(PW._suppress_modal(top, to_print.words[-1]))
        else:
            return_list.append(to_print.words[-1])

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
        if bool(to_print.params):
            result.append(DSYM.SPACE)
            overriden = [PW._suppress_modal(top, x) for x in to_print.params]
            result += PW._sep_list(self, to_print, overriden, sep=".")

        if bool(to_print.rebind):
            result.append(DSYM.SPACE)
            result.append(DSYM.REBIND_SYM)
            result.append(DSYM.SPACE)
            result.append(PW._suppress_modal(top, to_print.rebind))

        return result


# Abstraction
class ContainerPrinter(PrintSemantics):
    """ Production Containers """

    def __call__(self, to_print, top=None):
        return PW._sep_list(self, to_print, to_print.clauses, sep="\n")

class StructurePrinter(PrintSemantics):
    """ Ordered structures """

    def __call__(self, to_print, top=None):
        # TODO define order, add newlines, tags
        result = []
        # print the name
        result.append(top.override("_:NO_MODAL", to_print))
        result.append(":")
        result += ["(", "::", "ρ", ")"]
        result.append(DSYM.CONTAINER_JOIN_P)
        for tag in to_print.tags:
            result.append(DSYM.TAG_SYM)
            result.append(tag)
            result.append(DSYM.CONTAINER_JOIN_P)

        if bool(to_print.tags):
            result.append(DSYM.CONTAINER_JOIN_P)

        if bool(to_print.structure[DS.QUERY_COMPONENT]):
            result += to_print.structure[DS.QUERY_COMPONENT]
            result.append(DSYM.CONTAINER_JOIN_P)
            result.append(DSYM.CONTAINER_JOIN_P)
        if bool(to_print.structure[DS.TRANSFORM_COMPONENT]):
            result += to_print.structure[DS.TRANSFORM_COMPONENT]
            result.append(DSYM.CONTAINER_JOIN_P)
            result.append(DSYM.CONTAINER_JOIN_P)
        if bool(to_print.structure[DS.ACTION_COMPONENT]):
            result += to_print.structure[DS.ACTION_COMPONENT]
            result.append(DSYM.CONTAINER_JOIN_P)
            result.append(DSYM.CONTAINER_JOIN_P)

        result.append(DSYM.END_SYM)
        return result

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
