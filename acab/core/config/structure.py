"""
Structure Verification in Config Files

"""

# TODO check primitives?
# verify Parse.Structure -> Symbols+Aliases unify

import logging as logmod
from dataclasses import dataclass, field
from enum import Enum, EnumMeta
from typing import (Any, Callable, ClassVar, Dict, Generic, Iterable, Iterator,
                    List, Mapping, Match, MutableMapping, Optional, Sequence,
                    Set, Tuple, TypeVar, Union, cast)

logging = logmod.getLogger(__name__)

from acab.core.config.config import AcabConfig
from acab.error.config import AcabConfigException
from acab.core.decorators.util import registerOn

@registerOn(AcabConfig)
def check_structure(self):
    """
    a mixin extension of AcabConfig to verify the loaded config files information
    """
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
