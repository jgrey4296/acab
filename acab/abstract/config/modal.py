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

from acab.abstract.config.config import AcabConfig

logging = root_logger.getLogger(__name__)

# Merging modalities
# TODO make this a hook for the config class itself

# Get the modalities
# Get the Symbol collection:
# MODAL_LANGUAGE = "".join(MODAL_PREP.values()).replace(" ","")

@dataclass
class ModalConfig:
    modal_enums : Dict[str, EnumMeta]                 = field(init=False, default_factory=dict)
    modal_defaults : Dict[str, Enum]                  = field(init=False, default_factory=dict)
    modal_printing : Dict[Enum, str]                  = field(init=False, default_factory=dict)
    modal_syntax_lookup : Dict[str, Tuple[str, Enum]] = field(init=False, default_factory=dict)

    # Generate enums for each modality:
    def __post_init__(self):
        try:
            modal_names   = self.value("MODAL", as_list=True)
            logging.info("Initialising Modalities: {}".format(" ".join(modal_names)))
            for name in modal_names:
                new_enum           = Enum(name, self.value("Modal.{}".format(name), "ENUM_VALUES"))
                default            = self.value("Modal.{}".format(name), "DEFAULT")
                symbol_dict        = self.value("Modal.{}.Symbols".format(name), as_dict=True)
                symbol_enum_lookup = {syntax: (name, new_enum[val.upper()]) for val, syntax in symbol_dict.items()}


            # Ensure consistent and distinct modal sets
            assert(not any([x in self.modal_syntax_lookup for x in symbol_enum_lookup.keys()]))
            assert(len(new_enum) == len(symbol_enum_lookup))

            # Update the modal consts
            self.modal_enums[name]    = new_enum
            self.modal_defaults[name] = new_enum[default.upper()]
            self.modal_syntax_lookup.update(symbol_enum_lookup)
            self.modal_printing.update({e_val[1]: syntax for syntax, e_val in symbol_enum_lookup.items()})

        except AssertionError as err:
            logging.exception("Inconsistent Modality defined: {}".format(name))
