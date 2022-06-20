from dataclasses import dataclass, field
from enum import Enum
from itertools import filterfalse, starmap, zip_longest
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import acab.core.defaults.value_keys as DS
import acab.interfaces.value as VI
from acab import AcabConfig
from acab.core.defaults import print_signals as DSig
from acab.core.defaults import print_symbols as DSYM
from acab.core.printing import basic
from acab.core.printing import wrappers as PW
from acab.core.value.instruction import Instruction
from acab.core.value.sentence import Sentence
from acab.interfaces.printing import PrintSemantics_i
from acab.interfaces.value import ValueFactory as VF

config = AcabConfig()

ANNOTATIONS = [config.prepare("Value.Structure", x, _type=Enum)() for x in config.prepare("Print.Annotations", _type=list)()]
ATOM_HINT   = DSig.ATOM
TYPE_BASE   = "_:" + config.prepare("Data", "TYPE_BASE")()

SEN_SEN     = VF.sen([DS.SENTENCE_PRIM])

def grouper(iterable, n, fillvalue=None):
    """ Collect data into fixed-length chunks or blocks
    from https://docs.python.org/3/library/itertools.html
    """
    # grouper('ABCDEFG', 3, 'x') --> ABC DEF Gxx"
    args = [iter(iterable)] * n
    return zip_longest(*args, fillvalue=fillvalue)

# Independent
class AtomicPrinter(basic.PrintSemanticsImpl, PrintSemantics_i):
    """ Simply print the str of anything passed in """
    def verify(self, instruction) -> bool:
        return True

    def __call__(self, value, top=None, data=None):
        return str(value.name)

class PrimitiveTypeAwarePrinter(basic.PrintSemanticsImpl, PrintSemantics_i):

    def verify(self, instruction) -> bool:
        return True

    def add_transforms(self):
        return [PW._maybe_wrap_str,
                PW._maybe_wrap_regex,
                PW._maybe_wrap_var]

    def __call__(self, value, top=None, data=None):
        curr_str = [str(value.name)]
        return self.run_transforms(value, curr_str)

class ModalAwarePrinter(basic.PrintSemanticsImpl, PrintSemantics_i):

    def verify(self, instruction) -> bool:
        return True

    def add_transforms(self):
        return [PW._maybe_wrap_str,
                PW._maybe_wrap_regex,
                PW._maybe_wrap_var]


    def __call__(self, value, top=None, data=None):
        curr_str = [str(value.name)]
        transformed = self.run_transforms(value, curr_str)
        # lookup modal to care about from top.
        # TODO handle multi-modals
        transformed.append(top.override(DSig.MODAL, value, data=data))

        return transformed


class UUIDAwarePrinter(basic.PrintSemanticsImpl, PrintSemantics_i):

    def verify(self, instruction) -> bool:
        return True

    def add_transforms(self):
        return [PW._maybe_wrap_str,
                PW._maybe_wrap_regex,
                PW._maybe_wrap_var]


    def __call__(self, value, top=None, data=None):
        curr_str = [str(value.name)]
        transformed = self.run_transforms(value, curr_str)

        final = ["(", str(value.uuid), " : "] + transformed + [")"]
        return final



class AnnotationAwareValuePrinter(basic.PrintSemanticsImpl, PrintSemantics_i):

    def add_transforms(self):
        return [PW._maybe_wrap_str,
                PW._maybe_wrap_regex,
                PW._maybe_wrap_var]

    def __call__(self, value, top=None, data=None):
        return_list = []

        curr_str = [value.name]
        return_list += (self.run_transforms(value, curr_str))
        return_list.append(top.override(DSig.ANNOTATIONS, value, data=data))

        # Pass data through to modal:
        return_list.append(top.override(DSig.MODAL, value, data=data))


        return return_list

class ModalPrinter(basic.PrintSemanticsImpl, PrintSemantics_i):

    def __call__(self, value, top=None, data=None):
        return_list = []
        if bool(data) and "no_modal" in data and bool(data['no_modal']):
            return return_list

        modal = top.check(DSig.MODAL)
        if modal in value.data:
            return_list.append(value.data[modal])
        else:
            return_list.append(DSYM.FALLBACK_MODAL_SYM)

        return return_list

class AnnotationPrinter(basic.PrintSemanticsImpl, PrintSemantics_i):

    """
    gets annotations, assigns them print registers,
    then sends them to print with a final AnnotationFinaliser call
    to use the registers if they have values.
    """
    def __call__(self, value, top=None, data=None):
        return_list = []
        # Add all annotations to the stack as necessary
        annotations_in_value = [x for x in ANNOTATIONS if x in value.data]
        if not bool(annotations_in_value):
            return []

        # Pretty Print annotations
        registers      = []
        for annotation in annotations_in_value:
            annotation_name = annotation.name if hasattr(annotation, "name") else annotation
            signal = annotation_name if annotation_name in top else False
            value  = value if signal else value.data[annotation]
            reg, ov = top.assign_to_register(signal, value)
            registers.append(reg)
            return_list.append(ov)

        if hasattr(value, "_acab_operator_sugar"):
            # TODO refine this
            reg, ov = top.assign_to_register(False, value._acab_operator_sugar)
            registers.append(reg)
            return_list.append(ov)

        return_list.append(top.override(DSig.ANNOTATIONS_FINAL, registers))
        return return_list

class AnnotationFinaliser(basic.PrintSemanticsImpl, PrintSemantics_i):
    """
    Grabs the print registers it is passed, and if they have values,
    wraps them in parens and returns them for the main printer to use
    """
    def __call__(self, registers, top=None, data=None):
        # Get each register and join it
        return_list = []
        joined = []
        for reg in registers:
            val = "".join([x for x in top.print_registers[reg]])
            if bool(val):
                joined.append(val)
    
        # To decide whether to add anything to main return here:
        if bool(joined):
            return_list.append("(")
            return_list += ", ".join(joined)
            return_list.append(")")

        return return_list

class ConstraintPrinter(basic.PrintSemanticsImpl, PrintSemantics_i):

    def __call__(self, value, top=None, data=None):
        return_list = []
        for constraint in value.data[DS.CONSTRAINT]:
            return_list.append(constraint)
            return_list.append(", ")

        return_list.pop()
        return return_list

# Dependent
class BasicSentenceAwarePrinter(basic.PrintSemanticsImpl, PrintSemantics_i):
    # TODO be able to specify pre and post annotations

    def __call__(self, value, top=None, data=None):
        assert(isinstance(value, VI.Sentence_i))
        return_list = []

        if DS.NEGATION in value.data and value.data[DS.NEGATION]:
            return_list.append(DSYM.NEGATION_SYM)

        return_list += value.words[:-1]
        if not isinstance(value.words[-1], Instruction):
            return_list.append(PW._suppress_modal(top, value.words[-1]))
        else:
            return_list.append(value.words[-1])

        # Handle query
        if DS.QUERY in value.data and value.data[DS.QUERY]:
            return_list.append(DSYM.QUERY_SYM)


        return return_list


class ProductionComponentPrinter(basic.PrintSemanticsImpl, PrintSemantics_i):

    def __call__(self, value, top=None, data=None):
        result = []
        # if sugared,
        # if value.sugared:
        #     pass

        # else
        result.append(DSYM.FUNC_SYM)
        result.append(value[0])
        if value[1] != "returns":
            result.append(DSYM.SPACE)
            params = value[1] if value[1][0] != "_:node" else value[1][1:]

            overriden = [PW._suppress_modal(top, x) if not isinstance(x, Sentence) else x for x in params]
            result += PW._sep_list(self, value, overriden, sep=" ")

        if value[-1] not in ("returns", "bool"):
            # TODO align these
            result.append(DSYM.SPACE)
            result.append(DSYM.REBIND_SYM)
            result.append(DSYM.SPACE)
            result.append(PW._suppress_modal(top, value[-1]))

        return result


# Abstraction
class ImplicitContainerPrinter(basic.PrintSemanticsImpl, PrintSemantics_i):
    """ Production Containers """

    def __call__(self, value, top=None, data=None):
        result = [[DSYM.INDENT, x, DSYM.CONTAINER_JOIN_P] for x in  value.clauses]
        return result

class ExplicitContainerPrinter(basic.PrintSemanticsImpl, PrintSemantics_i):
    """ Production Containers """

    def __call__(self, value, top=None, data=None):
        result = []

        result.append(top.override(ATOM_HINT, value, data={"no_modal": True}))
        # result += ["(::", value.type, ")", ":"]
        result += [":", DSYM.CONTAINER_JOIN_P]
        if bool(value.params):
            result.append(PW._wrap_var_list(self, value.type, []))
            result.append(DSYM.CONTAINER_JOIN_P)

        if bool(value.tags):
            result.append(top.override(DSig.TAGS, value.tags))
            result.append(DSYM.CONTAINER_JOIN_P)

        result.append([[DSYM.INDENT, x, DSYM.CONTAINER_JOIN_P] for x in  value.value])
        result.append(DSYM.END_SYM)
        result.append(DSYM.PRINT_SEPARATOR_P)
        return result

class StructurePrinter(basic.PrintSemanticsImpl, PrintSemantics_i):
    """ Ordered structures """

    def __call__(self, value, top=None, data=None):
        # TODO define order, add newlines
        result = []
        # print the name
        result.append(top.override(ATOM_HINT, value, data={"no_modal": True}))
        # TODO parameterise this
        # result += ["(::", value.type, ")"]
        result.append(":")
        result.append(DSYM.CONTAINER_JOIN_P)
        if bool(value.params):
            # TODO make this more robust
            result.append(DSYM.INDENT)
            result.append(DSYM.PARAM_WRAP)
            result.append(" ")
            result += [top.override(ATOM_HINT, x, data={"no_modal": True}) for x in value.params]
            result.append(" ")
            result.append(DSYM.PARAM_WRAP)
            result.append(DSYM.CONTAINER_JOIN_P)
            result.append(DSYM.CONTAINER_JOIN_P)

        if bool(value.tags):
            result.append(top.override(DSig.TAGS, value.tags))
            result.append(DSYM.CONTAINER_JOIN_P)

        for container in value.value:
            if bool(container):
                result.append(top.override(DSig.IMPLICIT_CONTAINER, container))
                result.append(DSYM.CONTAINER_JOIN_P)

        if result[-1] == DSYM.CONTAINER_JOIN_P:
            result.pop()

        result.append(DSYM.END_SYM)
        result.append(DSYM.PRINT_SEPARATOR_P)
        return result

# Utility
@dataclass
class ConfigBackedSymbolPrinter(basic.PrintSemanticsImpl, PrintSemantics_i):
    """ Use an AcabConfig for lookup of provided
    symbol tuples.
    """
    overrides : dict[Any, str] = field(default_factory=dict)
    _config   : AcabConfig     = field(default_factory=AcabConfig)

    def __call__(self, value, top=None, data=None):
        # Look the value up in overrides
        if value in self.overrides:
            return self.overrides[value]

        return self._config.value(value)


class TagPrinter(basic.PrintSemanticsImpl, PrintSemantics_i):
    """ Prints a set of tags, indented, as sentences
    prepended with the tag symbol, in strides of 4 """

    def __call__(self, value, top=None, data=None):
        assert(isinstance(value, (set, list, frozenset))), value
        result = []
        # TODO: customize stride in config?
        for tags in grouper(sorted(value), 4):
            result.append(DSYM.INDENT)
            result.append(DSYM.TAG_SYM)
            result.append(VF.sen([x for x in tags if x is not None]))
            result.append(DSYM.CONTAINER_JOIN_P)

        return result


class NoOpPrinter(basic.PrintSemanticsImpl, PrintSemantics_i):

    def __call__(self, value, top=None, data=None):
        return []


class SimpleTypePrinter(basic.PrintSemanticsImpl, PrintSemantics_i):

    def __call__(self, value, top=None, data=None):
        return_list = []
        # TODO or suppression types
        if value.type == TYPE_BASE:
            return []

        type_str = value.type
        return_list.append("::")
        # TODO handle aliasees
        return_list.append(type_str)
        return return_list
