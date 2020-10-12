"""
Utilities for printing out information
All deprecated now in favour of
print_semantics.py
wrappers.py
default_handlers.py
"""
from re import Pattern
from acab.config import AcabConfig

util = AcabConfig.Get()

#Setup
# TODO register additional constraints
def register_modal(a_dict, reset=False):
    """ Register how to print a modal operator
    ie: For exclusion logic: DOT -> ".", EX -> "!"
    """
    raise DeprecationWarning("Use Print Semantics instead")


def register_primitive(a_dict, reset=False):
    """
    Register additional primitive formats

    The two base primitive examples are:
    REGEX: /{}/
    STRING: "{}"
    """
    raise DeprecationWarning("Use Print Semantics instead")


def register_constraint(*constraints, reset=False):
    """
    Register constraints / annotations of a value to represent for a value:
    eg: x (::a.type), or y(>2)...
    """
    raise DeprecationWarning("Use Print Semantics Instead")

def register_class(cls, func):
    """
    Register a function which prints a specific class

    Enables separating print logic from the class
    """
    raise DeprecationWarning("Use Print Semantics Instead")


def register_obvious_types(*type_instances):
    """ Register Type Instances which do not need pretty printing
    eg: ATOMs and INTs don't need it: an_atom(::atom) 2(::number.int)
    """
    raise DeprecationWarning("Use Print Semantics instead")

def default_opts(*args, **kwargs):
    """ Create the default options for pprint """
    raise DeprecationWarning("Use print semantics instead")

def pprint(obj, opts=None, **kwargs):
    """ Top Level PPrint Routine, looks up class type
    in registered pprints and calls that, with default opts if necessary """
    raise DeprecationWarning("Use Print Semantics instead")

def print_value(value, opts):
    raise DeprecationWarning("Use Print Semantics instead")

def print_sequence(seq, opts):
    """ Pretty Print a Sequence, ie: things like sentences """
    raise DeprecationWarning("Use Print Semantics instead")

def print_container(container, opts):
    raise DeprecationWarning("Use Print Semantics instead")

def print_statement(statement, opts):
    raise DeprecationWarning("Use Print Semantics instead")

def print_fallback(fallback_list):
    raise DeprecationWarning("Use Print Semantics instead")

def print_operator(operator, opts):
    """ Op Fix is the count of vars to print before printing the op.
    eg: op_fix=0 : + 1 2 3 ...
        op_fix=2 : 1 2 + 3 ...
    """
    raise DeprecationWarning("Use Print Semantics instead")

def print_operator_rebind(operator, opts):
    raise DeprecationWarning("Use Print Semantics instead")

def print_operator_wrap(operator, opts):
    raise DeprecationWarning("Use Print Semantics instead")
