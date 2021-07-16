from dataclasses import dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import acab.abstract.core.default_structure as DS
import acab.abstract.interfaces.value_interfaces as VI
from acab.abstract.config.config import GET, AcabConfig, ConfigSpec
from acab.abstract.core.values import Sentence, AcabStatement
from acab.abstract.interfaces.printing_interfaces import PrintSemantics
from acab.abstract.printing import default_symbols as DSYM
from acab.abstract.printing import wrappers as PW

config = GET()

SEN_SEN = Sentence.build([DS.SENTENCE_PRIM])

# Independent
class BasicPrinter(PrintSemantics):
    """ Simply print the str of anything passed in """

    def __call__(self, value, top=None):
        return str(value.name)

class PrimitiveTypeAwarePrinter(PrintSemantics):

    def add_transforms(self):
        return [PW._maybe_wrap_str,
                PW._maybe_wrap_regex,
                PW._maybe_wrap_var]

    def __call__(self, value, top=None):
        curr_str = [str(value.name)]
        return self.run_transforms(value, curr_str)

class ModalAwarePrinter(PrintSemantics):

    def add_transforms(self):
        return [PW._maybe_wrap_str,
                PW._maybe_wrap_regex,
                PW._maybe_wrap_var]


    def __call__(self, value, top=None):
        curr_str = [str(value.name)]
        transformed = self.run_transforms(value, curr_str)
        # lookup modal to care about from top.
        # TODO handle multi-modals
        modal = top.check('MODAL')
        if modal in value.data:
            transformed.append(value.data[modal])

        return transformed


class UUIDAwarePrinter(PrintSemantics):
    def add_transforms(self):
        return [PW._maybe_wrap_str,
                PW._maybe_wrap_regex,
                PW._maybe_wrap_var]


    def __call__(self, value, top=None):
        curr_str = [str(value.name)]
        transformed = self.run_transforms(value, curr_str)

        final = ["(", str(value.uuid), " : "] + transformed + [")"]
        return final


class ConstraintAwareValuePrinter(PrintSemantics):
    def add_transforms(self):
        return [PW._maybe_wrap_str,
                PW._maybe_wrap_regex,
                PW._maybe_wrap_var]

    def __call__(self, value, top=None):
        return_list = []
        curr_str = [str(value.name)]
        return_list.append(self.run_transforms(value, curr_str))

        if DS.CONSTRAINT in value.data:
            # TODO write a list_wrap func
            return_list.append("(")
            for constraint in value.data[DS.CONSTRAINT][:-1]:
                return_list.append(constraint)
                return_list.append(", ")

            return_list.append(value.data[DS.CONSTRAINT][-1])
            return_list.append(")")

        modal = top.check('MODAL')
        if modal in value.data:
            return_list.append(value.data[modal])


        return return_list


# Dependent
class BasicSentenceAwarePrinter(PrintSemantics):

    def __call__(self, value, top=None):
        assert(value.type == SEN_SEN)
        return_list = []

        if DS.NEGATION in value.data and value.data[DS.NEGATION]:
            return_list.append(DSYM.NEGATION_SYM)

        return_list += value.words[:-1]
        if not isinstance(value.words[-1], AcabStatement):
            return_list.append(PW._suppress_modal(top, value.words[-1]))
        else:
            return_list.append(value.words[-1])

        # Handle query
        if DS.QUERY in value.data and value.data[DS.QUERY]:
            return_list.append(DSYM.QUERY_SYM)

        return return_list


class ProductionComponentPrinter(PrintSemantics):

    def __call__(self, value, top=None):
        result = []
        # if sugared,
        # if value.sugared:
        #     pass

        # else
        result.append(DSYM.FUNC_SYM)
        result.append(value.value)
        if bool(value.params):
            result.append(DSYM.SPACE)
            overriden = [PW._suppress_modal(top, x) if not isinstance(x, Sentence) else x for x in value.params]
            result += PW._sep_list(self, value, overriden, sep=" ")

        if bool(value.rebind):
            result.append(DSYM.SPACE)
            result.append(DSYM.REBIND_SYM)
            result.append(DSYM.SPACE)
            result.append(PW._suppress_modal(top, value.rebind))

        return result


# Abstraction
class ContainerPrinter(PrintSemantics):
    """ Production Containers """

    def __call__(self, value, top=None):
        return PW._sep_list(self, value, value.clauses, sep="\n")
class StatementPrinter(PrintSemantics):

    def __call__(self, value, top=None):
        result = []
        result.append(value.name)
        result.append(":")
        result += ["(", "::"]
        if DS.SEMANTIC_HINT in value.data:
            result += [value.data[DS.SEMANTIC_HINT], ")"]
        else:
            result += [value.type, ")"]
        result.append(DSYM.CONTAINER_JOIN_P)
        result.append(PW._wrap_var_list(self, value.type, []))
        result.append([[x, DSYM.CONTAINER_JOIN_P] for x in  value.value])
        result.append(DSYM.CONTAINER_JOIN_P)
        result.append(DSYM.END_SYM)

        return result

class StructurePrinter(PrintSemantics):
    """ Ordered structures """

    def __call__(self, value, top=None):
        # TODO define order, add newlines, tags
        result = []
        # print the name
        result.append(top.override("_:NO_MODAL", value))
        result.append(":")
        result += ["(", "::", "ρ", ")"]
        result.append(DSYM.CONTAINER_JOIN_P)
        for tag in value.tags:
            result.append(DSYM.TAG_SYM)
            result.append(tag)
            result.append(DSYM.CONTAINER_JOIN_P)

        if bool(value.tags):
            result.append(DSYM.CONTAINER_JOIN_P)

        if bool(value.structure[DS.QUERY_COMPONENT]):
            result += value.structure[DS.QUERY_COMPONENT]
            result.append(DSYM.CONTAINER_JOIN_P)
            result.append(DSYM.CONTAINER_JOIN_P)

        if bool(value.structure[DS.TRANSFORM_COMPONENT]):
            result += value.structure[DS.TRANSFORM_COMPONENT]
            result.append(DSYM.CONTAINER_JOIN_P)
            result.append(DSYM.CONTAINER_JOIN_P)

        if bool(value.structure[DS.ACTION_COMPONENT]):
            result += value.structure[DS.ACTION_COMPONENT]
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
