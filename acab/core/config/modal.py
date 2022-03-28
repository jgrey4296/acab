#!/usr/bin/env python3
"""
Takes Modalities defined in config files using the sections:
[MODAL]            - Defines Modality Names
[Modal.{}]         - Defines Modal Enums and Default value
[Modal.{}.Symbols] - Defines Symbols for parsing and printing
"""
import logging as root_logger
from dataclasses import dataclass, field
from enum import Enum, EnumMeta
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import pyparsing as pp

logging = root_logger.getLogger(__name__)


def modal_config(self):
    """ Load and create MODAL section enums/defaults/symbols
    Any value in MODAL will cause to be processed:
    Modal.{value} [ENUM_VALUES, DEFAULT]
    Modal.{value}.Symbols
    """
    try:
        modal_spec  = self.prepare("MODAL", as_list=True)
        modal_names = self.value(modal_spec)
        logging.info("Initialising Modalities: {}".format(" ".join(modal_names)))
        for name in modal_names:
            new_spec           = self.prepare(f"Modal.{name}", "ENUM_VALUES")
            default_spec       = self.prepare(f"Modal.{name}", "DEFAULT")
            symbol_spec        = self.prepare(f"Modal.{name}.Symbols", as_dict=True)
            new_enum           = Enum(name, self.value(new_spec))
            default            = self.value(default_spec)
            symbol_dict        = self.value(symbol_spec)
            symbol_enum_lookup = {syntax: new_enum[val.upper()] for val, syntax in symbol_dict.items()}
            print_lookup       = {e_val: syntax for syntax, e_val in symbol_enum_lookup.items()}

            # Ensure consistent and distinct modal sets
            assert(not any([x in self.syntax_extension for x in symbol_enum_lookup.keys()]))
            assert(len(new_enum) == len(symbol_enum_lookup))

            # Update the modal consts
            self.enums[name]    = new_enum
            self.defaults[name] = new_enum[default.upper()]
            self.syntax_extension.update(symbol_enum_lookup)
            self.printing_extension.update(print_lookup)

    # pylint: disable=unused-variable
    except AssertionError as err:
        logging.exception("Inconsistent Modality defined: {}".format(name))
        raise err from err
    except KeyError as err:
        logging.exception("Missing Key found")
        raise err from err

    logging.info("Modality Load Completed")
