#!/usr/bin/env python3
"""
Takes Modalities defined in config files using the sections:
[MODAL]            - Defines Modality Names
[Modal.{}]         - Defines Modal Enums and Default value
[Modal.{}.Symbols] - Defines Symbols for parsing and printing
"""
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar, Generic

from dataclasses import dataclass, field

import logging as root_logger
import pyparsing as pp
from enum import Enum, EnumMeta


logging = root_logger.getLogger(__name__)

def ModalConfig(self):
    """ Load and create MODAL section enums/defaults/symbols
    Any value in MODAL will cause to be processed:
    Modal.{value} [ENUM_VALUES, DEFAULT]
    Modal.{value}.Symbols
    """
    try:
        modal_names   = self.value("MODAL", as_list=True)
        logging.info("Initialising Modalities: {}".format(" ".join(modal_names)))
        for name in modal_names:
            new_enum           = Enum(name, self.value(f"Modal.{name}", "ENUM_VALUES"))
            default            = self.value(f"Modal.{name}", "DEFAULT")
            symbol_dict        = self.value(f"Modal.{name}.Symbols", as_dict=True)
            symbol_enum_lookup = {syntax: (name, new_enum[val.upper()]) for val, syntax in symbol_dict.items()}
            print_lookup       = {e_val[1]: syntax for syntax, e_val in symbol_enum_lookup.items()}

            # Ensure consistent and distinct modal sets
            assert(not any([x in self.syntax_extension for x in symbol_enum_lookup.keys()]))
            assert(len(new_enum) == len(symbol_enum_lookup))

            # Update the modal consts
            self.enums[name]    = new_enum
            self.defaults[name] = new_enum[default.upper()]
            self.syntax_extension.update(symbol_enum_lookup)
            self.printing_extension.update(print_lookup)

    except AssertionError as err:
        logging.exception("Inconsistent Modality defined: {}".format(name))
