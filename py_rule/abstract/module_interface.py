"""
The Module Interface definition that modules need to enact.
Comes in two parts: The Parser, and the data

"""
import logging as root_logger

from .value import PyRuleValue

logging = root_logger.getLogger(__name__)


class ModuleInterface:
    """ A Module specification.
    Should be constructed in a module's __init__,
    into a MODULE_SPEC variable.

    Implement:
    parse_string,
    init_strings
    assert/query _parsers
    """


    def __init__(self):
        # A Parser has to provide a parser combinator to
        # integrate into the Working Memory Lanuage.
        # (By Default Exclusion Trie).
        # The Combinator *must* return a tuple:
        # ("typestr", data)
        # TODO add printing lookup dictionaries
        pass

    def parse_string(self, string):
        """ Takes a String, parses it into Data format """
        raise NotImplementedError()

    def init_strings(self):
        """ Return any strings to parse as
        part of the modules initialisation.
        Defining values etc that can now be parsed by
        the hotloaded value and statement parsers """
        raise NotImplementedError()

    def assert_parsers(self, parser_trie):
        """
        Assert parsers from this module for integration later
        ie: values.number <= number_parser
        values.$time      <= time_parser
        operators.set.add <=  set_add_op
        hotloads.value    <= HOTLOAD_VALUES
        """
        logging.warning("Module lacks parser assertions: {}".format(self.__class__))

    def query_parsers(self, parser_trie):
        """
        Query the now complete parser trie for hotloads
        values.$xs?
        hotloads.values!$p(~= /values/)?

        parser.or($xs) -> $y
        parser.assign $p $y

        """
        logging.warning("Module lacks parser queries: {}".format(self.__class__))
