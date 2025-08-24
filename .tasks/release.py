#!/usr/bin/env python3
"""
Release script

"""
# Imports:

# ##-- stdlib imports
import datetime
import enum
import functools as ftz
import itertools as itz
import logging as logmod
import pathlib as pl
import re
import time
import collections
import contextlib
import hashlib
from copy import deepcopy
from uuid import UUID, uuid1
from weakref import ref
import atexit # for @atexit.register
import faulthandler
# ##-- end stdlib imports

import sys
import sh

# ##-- types
# isort: off
# General
import abc
import collections.abc
import typing
import types
from typing import cast, assert_type, assert_never
from typing import Generic, NewType, Never
from typing import no_type_check, final, override, overload
# Protocols and Interfaces:
from typing import Protocol, runtime_checkable
# isort: on
# ##-- end types

# ##-- type checking
# isort: off
if typing.TYPE_CHECKING:
    from typing import Final, ClassVar, Any, Self
    from typing import Literal, LiteralString
    from typing import TypeGuard
    from collections.abc import Iterable, Iterator, Callable, Generator
    from collections.abc import Sequence, Mapping, MutableMapping, Hashable

    from jgdv import Maybe
## isort: on
# ##-- end type checking

##-- logging
logging = logmod.getLogger(__name__)
##-- end logging

# Vars:

# Body:


def main():
    """
    Generalised release process
    """
    bump_level   : str
    new_version  : str
    ##--|
    match sh.git("--no-pager", "diff").strip():
        case "":
            pass
        case _:
            print("There are unstaged changes")
            exit(-1)

    match sys.argv:
        case [*_, "--bump", str() as bump_level]:
            current_version = sh.uv("version", "--short").strip()
            sh.uv("version", "--bump", bump_level)
            # Get new version
            new_version = sh.uv("version", "--short").strip()
            print(f"{current_version} -> {new_version}")
        case _:
            print("No Bump Level Provided")
            exit(-1)

    assert(bool(new_version))
    print("Generating changelog updates")
    sh.uv("run", "--frozen", "towncrier", "build", "--yes")
    print("Committing")
    sh.git("add", "--all")
    sh.git("commit", "-m", f"[Release]: {new_version}")
    print("Tagging")
    sh.git("tag", new_version)

##-- ifmain
if __name__ == "__main__":
    main()
##-- end ifmain
