"""
Structure Verification in Config Files

"""

# TODO check primitives?
# verify Parse.Structure -> Symbols+Aliases unify

import logging as root_logger
from dataclasses import dataclass, field
from enum import Enum, EnumMeta
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

import pyparsing as pp

logging = root_logger.getLogger(__name__)

from acab.abstract.config.config import AcabConfig
from acab.error.acab_config_exception import AcabConfigException
from acab.abstract.decorators.util import registerOn

@registerOn(AcabConfig)
def check_structure(self):
    try:
        constraints = self.prepare("Config.Constraints", as_dict=True)()
        for src, tgt in constraints.items():
            logging.info(f"Config Structure Check: {src} -> {tgt}")
            source = get_param_or_list(self, src)
            target = get_param_or_list(self, tgt)

            bad_params = [x for x in source if x not in target]
            if any(bad_params):
                raise AcabConfigException("Structure Mismatch",
                                          rest=[src,
                                                " ".join(bad_params),
                                                tgt])


    except AcabConfigException as err:
        logging.warning(str(err))


def get_param_or_list(conf, spec_string):
    if "/" in spec_string:
        sec, param = spec_string.split("/")
        if param == "default":
            param = param.upper()
        return conf.prepare(sec, param)().split(" ")

    return list(conf.prepare(spec_string, as_dict=True)().keys())
