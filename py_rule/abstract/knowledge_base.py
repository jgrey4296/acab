"""
The abstract form of the knowledge base
"""
import pyparsing as pp
from .mod_interface import ModuleSpecification

class KnowledgeBase:
    """ The Abstract KnowledgeBase """

    def __init__(self):
        self._have_added_types = False
        self._have_built_operataors = False

    def add_modules(self, mods):
        """ Add types into the parser """
        if not self._have_added_types:
            raise Exception("Can Only expand knowledge base types once")
        assert(all([isinstance(x, ModuleSpecification) for x in mods]))
        self._added_types = True
        #Construct operators:
        [x.construct_operators() for x in mods]

        #Add values parsers:
        parsers = [x.get_parser() for x in mods]
        or_d_types = pp.Or([x for x in parsers if x is not None])
        self._insert_into_values_parser(or_d_types)

        # TODO: add definitions

    def build_operator_parser(self):
        """ This is used to build the parsers of operators,
        once all module's operators have been constructed
        """
        if not self._have_built_operators:
            raise Exception("Can only build operators once")
        self._have_built_operators = True
        # if you haven't added types by now, tough:
        self._have_added_types = True
        self._build_operator_parser()

    # Methods to implement:
    def __eq__(self):
        raise Exception("Abstract Eq Method")

    def add(self, data):
        raise Exception("Abstract Add Method")

    def retract(self, data):
        raise Exception("Abstract Retract Method")

    def query(self, data):
        raise Exception("Abstract Query Method")

    def _insert_into_values_parser(self, parser):
        """
        Should look like FP.OTHER_VALS << or_d_parser
        """
        raise Exception("Abstract Method")

    def _build_operator_parser(self):
        """ This Method calls each parser component's
        'build_operators' function, that populates Forward defined
        parser combinators *after* modules are loaded.
        This ensures operators are included in the parsers """
        raise Exception("Abstract build operators method")
