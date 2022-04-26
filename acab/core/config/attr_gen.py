#!/usr/bin/env python3
"""
Provides run time generated attributes for accessing config values.
So can be either:
config.prepare("section", "value", actions=...)
Or:
config.attr.section.value
"""
from __future__ import annotations

import abc
import re
from dataclasses import InitVar, dataclass, field
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)
from configparser import SectionProxy
import logging as logmod
logging = logmod.getLogger(__name__)

if TYPE_CHECKING:
    # tc only imports
    pass


STR_RE   = re.compile(r"^[\"'](.+?)[\"']$")
NUM_RE   = re.compile(r"^[0-9]+$")
TRUE_RE  = re.compile("^True$")
FALSE_RE = re.compile("^False$")

class AttrSection:
    """
    A Section/Subsection of config values
    """

    def __init__(self, name):
        self.name  = name
        self._added_keys = []

    def __repr__(self):
        return f"<AttrSection {self.name} ({len(self._added_keys)})>"

    def __getitem__(self, k):
        if isinstance(k, slice):
            return [getattr(self, x) for x in self._keys]
        elif isinstance(k, str):
            return getattr(self, k)
        else:
            return []

    def _add(self, key, value):
        assert(key not in self._added_keys or getattr(self, key) == value)
        self._added_keys.append(key)
        setattr(self, key, value)

    @property
    def _keys(self):
        return list(self._added_keys)

class AttrGenerator:
    """
    Generates attributes from a read configparser
    Nests sections automatically,
    converting `[a.test.section]` in a config file
    to (a:Section).(test:Section).(section:Section)

    """

    def __init__(self, config: ConfigParser):
        self.__config = config
        self._added_keys = []

    def __repr__(self):
        return "<Generated Attribute Access to Config Files>"

    @property
    def sections(self):
        return self._added_keys

    def _generate(self):
        logging.debug("Generating Config Attributes")
        for section in self.__config.sections():
            logging.debug("Attr Section: {}", section)
            sec_obj : SectionProxy  = self.__config[section]
            subsections : List[str] = section.split(".")

            current = self.__generate_subsections(subsections)
            self.__generate_values(current, sec_obj)
        logging.debug("Config Attributes Generated")
        logging.debug("Config Attrbute Sections: {}", self.sections)


    def __generate_subsections(self, section_names: list[Any]) -> AttrSection:
        current : AttrSection|AttrGenerator = self

        for name in section_names:
            subsection = getattr(current, name, AttrSection(name))
            if not hasattr(current, name):
                setattr(current, name, subsection)
                current._added_keys.append(name)

            current = subsection

        return current


    def __generate_values(self, current:AttrSection, section: SectionProxy):
        for k,v in section.items():
            if v is None:
                v = k
            matches = STR_RE.match(v)
            if matches:
                v = matches[1]
            elif NUM_RE.match(v):
                v = int(v)
            elif TRUE_RE.match(v):
                v = True
            elif FALSE_RE.match(v):
                v = False
            elif "\n" in v and v != "\n":
                v = v.split("\n")

            current._add(k, v)
