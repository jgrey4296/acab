from py_rule.abstract.mod_interface import ModuleSpecifiction


class NumberSpecification(ModuleSpecification):
    """ A Module that provides numbers """

    def __init__(self):
        super(NumberSpecification, self).__init__()

    def parse_string(self, string):
        """ Takes a String, parses it into Data format """
        raise NotImplementedError()

    def construct_operators(self):
        """ Use this to call operator constructors """
        raise NotImplementedError()

    def init_strings(self):
        """ Return any strings to parse as
        part of the modules initialisation.
        Defining values etc that can now be parsed by
        the hotloaded value and statement parsers """
        raise NotImplementedError()


