#!/usr/bin/env python3
# https://docs.python.org/3/library/abc.html
# from https://realpython.com/python-interface/
import abc

@dataclass
class ConfigSpec():
    """ Dataclass to describe a config file value,
    and any transforms it needs prior to use """

    section : str                = field()
    key     : Optional[str]      = field(default=None)
    actions : List[CA.actions_e] = field(default_factory=list)
    as_list : bool               = field(default=False)
    as_dict : bool               = field(default=False)
    as_enum : bool               = field(default=False)
    as_bool : bool               = field(default=False)

    def __call__(self):
        inst = AcabConfig.Get()
        return inst(self)

    def __hash__(self):
        return hash(f"{self.section}:{self.key}")


class Config_i(metaclass=acb.ABCMeta):
    pass
