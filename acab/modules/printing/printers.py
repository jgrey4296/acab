from dataclasses import dataclass, field
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)
from itertools import zip_longest, filterfalse, starmap

import acab.abstract.core.default_structure as DS
import acab.abstract.interfaces.value as VI
from acab.abstract.config.config import GET, AcabConfig, ConfigSpec
from acab.abstract.core.values import Sentence, AcabStatement
from acab.abstract.interfaces.printing import PrintSemantics_i
from acab.abstract.printing import default_symbols as DSYM
from acab.abstract.printing import wrappers as PW

config = GET()

ANNOTATIONS = [x.upper() for x in config.prepare("Print.Annotations", as_list=True)()]

SEN_SEN = Sentence.build([DS.SENTENCE_PRIM])

def grouper(iterable, n, fillvalue=None):
    """ Collect data into fixed-length chunks or blocks
    from https://docs.python.org/3/library/itertools.html
    """
    # grouper('ABCDEFG', 3, 'x') --> ABC DEF Gxx"
    args = [iter(iterable)] * n
    return zip_longest(*args, fillvalue=fillvalue)

# Independent
class AtomicPrinter(PrintSemantics_i):
    """ Simply print the str of anything passed in """

    def __call__(self, value, top=None, data=None):
        return str(value.name)

class PrimitiveTypeAwarePrinter(PrintSemantics_i):

    def add_transforms(self):
        return [PW._maybe_wrap_str,
                PW._maybe_wrap_regex,
                PW._maybe_wrap_var]

    def __call__(self, value, top=None, data=None):
        curr_str = [str(value.name)]
        return self.run_transforms(value, curr_str)

class ModalAwarePrinter(PrintSemantics_i):

    def add_transforms(self):
        return [PW._maybe_wrap_str,
                PW._maybe_wrap_regex,
                PW._maybe_wrap_var]


    def __call__(self, value, top=None, data=None):
        curr_str = [str(value.name)]
        transformed = self.run_transforms(value, curr_str)
        # lookup modal to care about from top.
        # TODO handle multi-modals
        transformed.append(top.override("_:MODAL", value, data=data))

        return transformed


class UUIDAwarePrinter(PrintSemantics_i):
    def add_transforms(self):
        return [PW._maybe_wrap_str,
                PW._maybe_wrap_regex,
                PW._maybe_wrap_var]


    def __call__(self, value, top=None, data=None):
        curr_str = [str(value.name)]
        transformed = self.run_transforms(value, curr_str)

        final = ["(", str(value.uuid), " : "] + transformed + [")"]
        return final



class AnnotationAwareValuePrinter(PrintSemantics_i):

    def add_transforms(self):
        return [PW._maybe_wrap_str,
                PW._maybe_wrap_regex,
                PW._maybe_wrap_var]

    def __call__(self, value, top=None, data=None):
        return_list = []

        curr_str = [str(value.name)]
        return_list.append(self.run_transforms(value, curr_str))
        return_list.append(top.override("_:ANNOTATIONS", value, data=data))

        # Pass data through to modal:
        return_list.append(top.override("_:MODAL", value, data=data))

        return return_list

class ModalPrinter(PrintSemantics_i):

    def __call__(self, value, top=None, data=None):
        return_list = []
        if bool(data) and "no_modal" in data and bool(data['no_modal']):
            return return_list

        modal = top.check('MODAL')
        if modal in value.data:
            return_list.append(value.data[modal])
        else:
            return_list.append(DSYM.FALLBACK_MODAL_SYM)

        return return_list

class AnnotationPrinter(PrintSemantics_i):

    def __call__(self, value, top=None, data=None):
        return_list = []
        # Add all annotations to the stack as necessary
        annotations_in_value = [x for x in ANNOTATIONS if x in value.data]
        if not bool(annotations_in_value):
            return []

        # Pretty Print annotations
        annotations_pp = []
        for annotation in annotations_in_value:
            signal = f"_:{annotation}"
            if signal in top.handlers:
                annotations_pp.append(top.pprint(top.override(signal, value)))
            else:
                annotations_pp.append(top.pprint(value.data[annotation]))

        # Then Join:
        annotations_pp = [x for x in annotations_pp if bool(x)]

        # To decide whether to add anything to main return here:
        if bool(annotations_pp):
            return_list.append("(")
            return_list += ", ".join(annotations_pp)
            return_list.append(")")

        return return_list

class ConstraintPrinter(PrintSemantics_i):

    def __call__(self, value, top=None, data=None):
        return_list = []
        for constraint in value.data[DS.CONSTRAINT]:
            return_list.append(constraint)
            return_list.append(", ")

        return_list.pop()
        return return_list

# Dependent
class BasicSentenceAwarePrinter(PrintSemantics_i):

    def __call__(self, value, top=None, data=None):
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


class ProductionComponentPrinter(PrintSemantics_i):

    def __call__(self, value, top=None, data=None):
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
            # TODO align these
            result.append(DSYM.SPACE)
            result.append(DSYM.REBIND_SYM)
            result.append(DSYM.SPACE)
            result.append(PW._suppress_modal(top, value.rebind))

        return result


# Abstraction
class ImplicitContainerPrinter(PrintSemantics_i):
    """ Production Containers """

    def __call__(self, value, top=None, data=None):
        result = [[DSYM.INDENT, x, DSYM.CONTAINER_JOIN_P] for x in  value.clauses]
        return result

class ExplicitContainerPrinter(PrintSemantics_i):
    """ Production Containers """

    def __call__(self, value, top=None, data=None):
        result = []

        result.append(top.override("_:ATOM", value, data={"no_modal": True}))
        # result += ["(::", value.type, ")", ":"]
        result += [":", DSYM.CONTAINER_JOIN_P]
        if bool(value.params):
            result.append(PW._wrap_var_list(self, value.type, []))
            result.append(DSYM.CONTAINER_JOIN_P)

        if bool(value.tags):
            result.append(top.override("_:TAGS", value.tags))
            result.append(DSYM.CONTAINER_JOIN_P)

        result.append([[DSYM.INDENT, x, DSYM.CONTAINER_JOIN_P] for x in  value.value])
        result.append(DSYM.END_SYM)
        result.append(DSYM.PRINT_SEPARATOR_P)
        return result

class StructurePrinter(PrintSemantics_i):
    """ Ordered structures """

    def __call__(self, value, top=None, data=None):
        # TODO define order, add newlines
        result = []
        # print the name
        result.append(top.override("_:ATOM", value, data={"no_modal": True}))
        # TODO parameterise this
        # result += ["(::", value.type, ")"]
        result.append(":")
        result.append(DSYM.CONTAINER_JOIN_P)
        if bool(value.tags):
            result.append(top.override("_:TAGS", value.tags))
            result.append(DSYM.CONTAINER_JOIN_P)

        if bool(value.structure[DS.QUERY_COMPONENT]):
            result.append(top.override("_:IMPLICIT_CONTAINER", value.structure[DS.QUERY_COMPONENT]))
            result.append(DSYM.CONTAINER_JOIN_P)

        if bool(value.structure[DS.TRANSFORM_COMPONENT]):
            result.append(top.override("_:IMPLICIT_CONTAINER", value.structure[DS.TRANSFORM_COMPONENT]))
            result.append(DSYM.CONTAINER_JOIN_P)

        if bool(value.structure[DS.ACTION_COMPONENT]):
            result.append(top.override("_:IMPLICIT_CONTAINER", value.structure[DS.ACTION_COMPONENT]))

        result.append(DSYM.END_SYM)
        result.append(DSYM.PRINT_SEPARATOR_P)
        return result

# Utility
@dataclass
class ConfigBackedSymbolPrinter(PrintSemantics_i):
    """ Use an AcabConfig for lookup of provided
    symbol tuples.
    """
    overrides : Dict[Any, str] = field(default_factory=dict)
    _config   : AcabConfig     = field(default_factory=GET)

    def __call__(self, value, top=None, data=None):
        # Look the value up in overrides
        if value in self.overrides:
            return self.overrides[value]

        return self._config.value(value)


class TagPrinter(PrintSemantics_i):
    """ Prints a set of tags, indented, as sentences
    prepended with the tag symbol, in strides of 4 """

    def __call__(self, value, top=None, data=None):
        assert(isinstance(value, (set, list))), value
        result = []
        # TODO: customize stride in config?
        for tags in grouper(sorted(value), 4):
            result.append(DSYM.INDENT)
            result.append(DSYM.TAG_SYM)
            result.append(Sentence.build([x for x in tags if x is not None]))
            result.append(DSYM.CONTAINER_JOIN_P)

        return result


class NoOpPrinter(PrintSemantics_i):

    def __call__(self, value, top=None, data=None):
        return []


class SimpleTypePrinter(PrintSemantics_i):

    def __call__(self, value, top=None, data=None):
        return_list = []
        type_str = str(value.data[DS.TYPE_INSTANCE])
        if type_str == "_:ATOM":
            return []
        return_list.append("::")
        return_list.append(type_str)
        return return_list
