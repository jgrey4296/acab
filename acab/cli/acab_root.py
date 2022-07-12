#/usr/bin/env python3
"""
Root CLI Access for acab

Usage:
acab                 : list available commands
acab env             : list environment variables acab recognises
acab template        : list available templates
acab template [name] : print the template to stdout, for redirecting to a file
acab project  [name] : create a default acab project directory structure

Run `acabr` for the REPL

"""
from __future__ import annotations

import abc
from os import environ
import argparse
import logging as logmod
from os.path import splitext
from copy import deepcopy
from dataclasses import InitVar, dataclass, field
from importlib.resources import files, Path
from re import Pattern
from typing import (TYPE_CHECKING, Any, Callable, ClassVar, Final, Generic,
                    Iterable, Iterator, Mapping, Match, MutableMapping,
                    Protocol, Sequence, Tuple, TypeAlias, TypeGuard, TypeVar,
                    cast, final, overload, runtime_checkable)
from uuid import UUID, uuid1
from weakref import ref

from .repl_main import main_repl

parser = argparse.ArgumentParser(formatter_class=argparse.RawDescriptionHelpFormatter,
                                 epilog = "\n".join(["General Access to acab utilities"]))
parser.add_argument('instruction', default="list", nargs='?')
parser.add_argument('args', nargs="*")

args = parser.parse_args()

if TYPE_CHECKING:
    # tc only imports
    pass


logging = logmod.getLogger(__name__)

# Env Vars
ACAB_TEMPLATE_PATHS = "ACAB_TEMPLATE_PATHS"

base_template_path       = files("acab.__templates")
name_exclusions     = [".DS_Store", "__init__.py", "__init__.pyc"]

template_paths= [base_template_path]
if ACAB_TEMPLATE_PATHS in environ:
    potentials = (Path(x) for x in environ[ACAB_TEMPLATE_PATHS].split(":"))
    template_paths += (x for x in potentials if x.exists())

templates_dict = {}
for source in template_paths:
    templates_dict.update({splitext(x.name)[0]: x for x in source.iterdir() if x.is_file() and x.name not in name_exclusions})

def get_template(name:None|str):
    if name not in templates_dict:
        print("Template Not Found: ", name)
    else:
        print(templates_dict[name].read_text())

def main():
    match args.instruction, len(args.args):
        # Default
        case "list", _:
            print("Acab Utilities:")
            print("acab                 : list available commands")
            print("acab template        : list available templates")
            print("acab template [name] : print the template to stdout, for redirecting to a file")
            print("acab project  [name] : create a default acab project directory structure")
            print("acab repl            : run the repl")
        # Template Commands
        case "t" | "template", 0:
            print("Available Templates:")
            print("\t%s" % "\n\t".join(templates_dict.keys()))
        case "t" | "template", 1:
            print(get_template(args.args[0]))
        case "t" | "template", _:
            print("Template Error: Too many Args")
        # Project Commands
        case "p" | "project", 1:
            target = Path(args.args[0]).resolve()
            try:
                assert(target.parent.exists()), "Parent Doesn't Exist"
                assert(not target.exists()), "Directory Already Exists"
                print("Making Template in: ", args.args)
                target.mkdir()
                # TODO make the project structure
            except AssertionError as err:
                print(err)
        case "p" | "project", _:
            print("Project Error: Need a single name")
        case "r" | "repl", _:
            print("Run `acabr` for the REPL")
        # List Env Vars
        case "e" | "env", _:
            print("Acab ENV Vars:")
            print(f"\t{ACAB_TEMPLATE_PATHS}")
        # Else
        case _:
            print(f"Unrecognised: {args.instruction}")



if __name__ == '__main__':
    main()
