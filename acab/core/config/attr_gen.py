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
        self._added_keys = set()

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
        existing = getattr(self, key, None)
        if existing and existing != value:
            logging.warning("Config Attr Generator Override: {} : {} -> {}", key, getattr(self, key), value)
        self._added_keys.add(key)
        setattr(self, key, value)

    @property
    def _keys(self):
        return list(self._added_keys)
    def __contains__(self, k):
        return k in self._keys

class AttrGenerator:
    """
    Generates attributes from a read configparser
    Nests sections automatically,
    converting `[a.test.section]` in a config file
    to (a:Section).(test:Section).(section:Section)

    does not check for overriden values, use config.prepare/value for that
    """

    def __init__(self, config: ConfigParser):
        self.__config = config
        self._added_keys = set()

    def __repr__(self):
        return "<Generated non-overriden Attribute Access to Config Files>"

    def __bool__(self):
        return bool(self._added_keys)

    @property
    def sections(self):
        return self._added_keys

    def _generate(self):
        logging.debug("Generating Config Attributes")
        for section in self.__config.sections():
            logging.debug("Attr Section: {}", section)
            leaf_section = self.__generate_subsections(section)
            sec_obj : SectionProxy  = self.__config[section]
            self.__generate_values(leaf_section, sec_obj)

        logging.debug("Config Attributes Generated")
        logging.debug("Config Attrbute Sections: {}", self.sections)


    def __generate_subsections(self, section: str) -> AttrSection:
        """
        Convert a configreader  to a trie of AttrSections (AS).
        Ready to add values into the leaf section.
        eg:
        [A.Config.Section]
        ->
        AS(A).AS(Config).AS(Section)
        """
        section_names : List[str] = section.split(".")
        current       : AttrSection|AttrGenerator = self

        for name in section_names:
            # Get or create the section
            subsection = getattr(current, name, AttrSection(name))
            if not hasattr(current, name):
                setattr(current, name, subsection)
                current._added_keys.add(name)

            current = subsection

        return current


    def __generate_values(self, current:AttrSection, section: SectionProxy):
        """
        Given an AttrSection(AS) and the configreader's sectionproxy,
        add the key/value pairs (or if only keys, key/key pairs)
        to the AttrSection.

        Converts integers and bools, strips surrounding quote marks,
        and removes newlines from values

        eg:
        [A_Section]
        key = value
        key2
        ->
        AS(A_Section, {key: value, key2: key2})
        """
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
