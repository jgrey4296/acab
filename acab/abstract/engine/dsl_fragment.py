"""
The DSL fragment definition that enables extension of the base language
Registers and uses parser fragments.

"""
import logging as root_logger

from acab.abstract.core.core_abstractions import AcabValue

logging = root_logger.getLogger(__name__)


class DSL_Fragment:
    """ DSL Fragment specification.

    The way to plug a sub-dsl into the working memory language.
    Implement:
    parse_string,
    init_strings
    assert/query _parsers
    """

    # TODO pass the type system in to be extended?

    def __init__(self):
        """
        A Parser has to provide a parser combinator to
        integrate into the Working Memory Lanuage.
        (By Default Exclusion Trie).
        The Combinator *must* return a tuple:
        ("typestr", data)
        """
        pass

    def parse_string(self, string):
        """ Takes a String, parses it into Data format """
        raise NotImplementedError()

    def register_printers(self):
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
        values.time      <= time_parser
        operators.set.add <=  set_add_op
        hotloads.value    <= HOTLOAD_VALUES
        """
        logging.debug("Module lacks parser assertions: {}".format(self.__class__))

    def query_parsers(self, parser_trie):
        """
        Query the now complete parser trie for hotloads
        values.$xs?
        hotloads.values!$p(~= /values/)?

        parser.or($xs) -> $y
        parser.assign $p $y

        """
        logging.debug("Module lacks parser queries: {}".format(self.__class__))


    def verify(self):
        logging.debug("No Verification specified")
