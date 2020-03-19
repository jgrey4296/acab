"""
The abstract form of a working memory
"""
import pyparsing as pp
from .mod_interface import ModuleSpecification


class WorkingMemory:
    """ The Abstract WorkingMemory """

    def __init__(self):
        self._have_added_types = False
        self._have_built_operators = False

    def add_modules(self, mods):
        """ Add types into the parser """
        if self._have_added_types:
            raise ImportError("Can Only expand working memory types once")
        assert(all([isinstance(x, ModuleSpecification) for x in mods]))
        self._added_types = True

        # Construct operators:
        [x.construct_operators() for x in mods]

        # Add values parsers:
        val_parsers = [y for x in mods for y in x.get_value_parsers()]
        or_d_types = pp.Or([x for x in val_parsers if x is not None])
        self._insert_into_values_parser(or_d_types)

        # add statement parsers:
        statement_parsers = [y for x in mods for y in x.get_statement_parsers()]
        or_d_statements = pp.Or([x for x in statement_parsers if x is not None])
        self._insert_into_statement_parser(or_d_statements)

        # Add annotation parsers:
        annotation_parsers = [y for x in mods for y in x.get_annotation_parsers()]
        or_d_statements = pp.Or([x for x in annotation_parsers if x is not None])
        self._insert_into_annotations_parser(or_d_statements)

        # Init module values
        mod_init_strings = [y for x in mods for y in x.init_strings() if y is not None]
        if bool(mod_init_strings):
            strings = "\n\n".join(mod_init_strings)
            self.add(strings)

    def build_operator_parser(self):
        """ This is used to build the parsers of operators,
        once all module's operators have been constructed
        """
        if self._have_built_operators:
            raise ImportError("Can only build operators once")
        self._have_built_operators = True
        # if you haven't added types by now, tough:
        self._have_added_types = True
        self._build_operator_parser()

    # Methods to implement:
    def __eq__(self, other):
        raise NotImplementedError()

    def add(self, data):
        raise NotImplementedError()

    def retract(self, data):
        raise NotImplementedError()

    def query(self, data):
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
