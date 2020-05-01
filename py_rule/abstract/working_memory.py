"""
The abstract form of a working memory

Working Memory is responsible for the core agent capabilities.
It provides an ontology structure, parsers for that structure,
and integrates modules into those parsers.

The canonical working memory is the Trie_WM.
It provides an Exclusion Logic Trie data structure,
Parsers to define sentences and rules in that data structure,
and can load modules to extend its capabilities.

The working memory, at core, can Add, Retract, and Query facts.

From a module it loads Value, Statement, and Annotation parsers.

"""
import pyparsing as pp
import logging as root_logger

from .mod_interface import ModuleSpecification

logging = root_logger.getLogger(__name__)


class WorkingMemory:
    """ The Abstract Working Memory """

    def __init__(self):
        self._have_added_types = False
        self._have_built_operators = False
        self._module_hotload_provision = {}

    def __str__(self):
        """ Print the working memory as a reparseable string """
        raise NotImplementedError()

    def __eq__(self, other):
        raise NotImplementedError()


    def add_modules(self, mods):
        """ Add types into the parser """
        assert(all([isinstance(x, ModuleSpecification) for x in mods]))


        # setup hotloads of the module
        dummy = [x.insert_hotloads(self._module_hotload_provision) for x in mods]

        # Construct operators:
        dummy = [x.construct_operators() for x in mods]

        # Add values parsers:
        val_parsers = [y for x in mods for y in x.value_parsers]
        or_d_types = pp.Or([x for x in val_parsers if x is not None])
        self._insert_into_values_parser(or_d_types)

        # add statement parsers:
        statement_parsers = [y for x in mods for y in x.statement_parsers]
        or_d_statements = pp.Or([x for x in statement_parsers if x is not None])
        self._insert_into_statement_parser(or_d_statements)

        # Add annotation parsers:
        annotation_parsers = [y for x in mods for y in x.annotation_parsers]
        or_d_statements = pp.Or([x for x in annotation_parsers if x is not None])
        self._insert_into_annotations_parser(or_d_statements)

        # Init module values
        mod_init_strings = [y for x in mods for y in x.init_strings() if y is not None]
        if bool(mod_init_strings):
            strings = "\n\n".join(mod_init_strings)
            self.add(strings)

        # TODO Setup printing lookups


    def build_operator_parser(self):
        """ This is used to build the parsers of operators,
        once all module's operators have been constructed
        """
        if self._have_built_operators:
            logging.warning("Should only build operators once")
        self._have_built_operators = True
        # if you haven't added types by now, tough:
        self._have_added_types = True
        self._build_operator_parser()

    # Methods to implement:
    def add(self, data):
        raise NotImplementedError()

    def retract(self, data):
        raise DeprecationWarning()

    def query(self, ctxs=None, engine=None):
        raise NotImplementedError()

    def _insert_into_values_parser(self, parser):
        """ Inserts new value types that can be parsed in a sentence
        Should look like FP.HOTLOAD_VALUES << or_d_parser
        """
        raise NotImplementedError()

    def _insert_into_statement_parser(self, parser):
        """ Inserts new statements entirely """
        raise NotImplementedError()

    def _insert_into_annotations_parser(self, parser):
        """ Inserts new annotations for values.
        Should look like FP.HOTLOAD_ANNOTATIONS << or_d_parser
        """
        raise NotImplementedError()

    def _build_operator_parser(self):
        """ This Method calls each parser component's
        'build_operators' function, that populates Forward defined
        parser combinators *after* modules are loaded.
        This ensures operators are included in the parsers """
        raise NotImplementedError()
