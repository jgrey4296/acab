#!/usr/bin/env python3
from dataclasses import dataclass, field
from itertools import filterfalse, starmap, zip_longest
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import acab.core.value.default_structure as DS
import acab.interfaces.value as VI
from acab import AcabConfig
from acab.core.printing import basic
from acab.core.printing import default_symbols as DSYM
from acab.core.printing import wrappers as PW
from acab.core.util.decorators.util import HandleSignal
from acab.core.value.instruction import Instruction
from acab.core.value.sentence import Sentence
from acab.interfaces.printing import PrintSemantics_i

config = AcabConfig()

@HandleSignal("TYPE_INSTANCE")
class TypeAwareValuePrinter(basic.PrintSemanticsImpl):

    def __call__(self, value, top=None, data=None):
        return_list = []
        type_str = str(value.data[DS.TYPE_INSTANCE])
        if type_str == "_:ATOM":
            return []

        return_list.append("::")
        return_list.append(value.data[DS.TYPE_INSTANCE])

        return return_list


@HandleSignal("TYPE_DEF")
class TypeRecordPrinter(basic.PrintSemanticsImpl):

    def __call__(self, value, top=None, data=None):
        ret_list = []
        ret_list.append(top.override("ATOM", value, data={"no_modal" : True}))
        ret_list.append(":")
        ret_list.append(DSYM.PRINT_SEPARATOR_P)

        clauses = value.value
        if not bool(clauses):
            # Simple Typedef, end line
            return ret_list + [DSYM.PRINT_SEPARATOR_P]
        elif len(clauses) == 1:
            # Has one structure inside, do it all on one line
            ret_list += clauses
            ret_list.append(DSYM.END_SYM)
            return ret_list

        # Complex structure, clauses on sep lines
        for clause in clauses:
            ret_list.append("   ")
            ret_list.append(clause)
            ret_list.append(DSYM.PRINT_SEPARATOR_P)

        ret_list.append(DSYM.END_SYM)
        ret_list.append(DSYM.PRINT_SEPARATOR_P)

        return ret_list

@HandleSignal("SUM_TYPE")
class SumTypePrinter(basic.PrintSemanticsImpl):
    def __call__(self, value, top=None, data=None):
        return_list = ["SUMTYPE"]
        return return_list


@HandleSignal("OP_DEF")
class OperatorTypePrinter(basic.PrintSemanticsImpl):
    def __call__(self, value, top=None, data=None):
        return_list = ["OPTYPE"]
        return return_list

@HandleSignal("TYPE_CLASS")
class TypeClassPrinter(basic.PrintSemanticsImpl):
    def __call__(self, value, top=None, data=None):
        return_list = ["TYPECLASS"]
        return return_list
