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

import logging as root_logger
import pyparsing as pp
from enum import Enum, EnumMeta

from acab.abstract.config.config import AcabConfig

logging = root_logger.getLogger(__name__)

util = AcabConfig.Get()

# Merging modalities
# TODO make this a hook for the config class itself

# Get the modalities
# Get the Symbol collection:
# MODAL_LANGUAGE = "".join(MODAL_PREP.values()).replace(" ","")
modal_names   = util.value("MODAL", as_list=True)
logging.info("Initialising Modalities: {}".format(" ".join(modal_names)))

# Generate enums for each modality:
MODAL_ENUMS: Dict[str, EnumMeta] = {}
MODAL_DEFAULTS: Dict[str, Enum]  = {}
MODAL_PRINTING: Dict[Enum, str]  = {}

SYNTAX_LOOKUP: Dict[str, Tuple[str, Enum]]   = {}

try:
    for name in modal_names:
        new_enum           = Enum(name, util.value("Modal.{}".format(name), "ENUM_VALUES"))
        default            = util.value("Modal.{}".format(name), "DEFAULT")
        symbol_dict        = util.value("Modal.{}.Symbols".format(name), as_dict=True)
        symbol_enum_lookup = {syntax: (name, new_enum[val.upper()]) for val, syntax in symbol_dict.items()}


        # Ensure consistent and distinct modal sets
        assert(not any([x in SYNTAX_LOOKUP for x in symbol_enum_lookup.keys()]))
        assert(len(new_enum) == len(symbol_enum_lookup))

        # Update the modal consts
        MODAL_ENUMS[name]    = new_enum
        MODAL_DEFAULTS[name] = new_enum[default.upper()]
        SYNTAX_LOOKUP.update(symbol_enum_lookup)
        MODAL_PRINTING.update({e_val[1]: syntax for syntax, e_val in symbol_enum_lookup.items()})

except AssertionError as err:
    logging.exception("Inconsistent Modality defined: {}".format(name))
