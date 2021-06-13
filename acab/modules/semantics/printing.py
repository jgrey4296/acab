
from acab.abstract.config.config import GET
from acab.abstract.interfaces.semantic_interfaces import PrintSemantics

from acab.abstract.printing import consts as PC
from acab.abstract.printing import default_handlers as DH
from acab.abstract.printing import util as PU
from acab.abstract.printing import wrappers as PW

config = GET()

class BasicPrinter(PrintSemantics):
    """ Simply print the str of anything passed in """

    def __call__(self, to_print):
        return str(to_print.name)

class PrimitiveTypeAwarePrinter(PrintSemantics):

    def add_transforms(self):
        return [PW._maybe_wrap_str,
                PW._maybe_wrap_regex,
                PW._maybe_wrap_var]

    def __call__(self, to_print):
        curr_str = str(to_print.name)
        return self.run_transforms(to_print, curr_str)

class ModalAwarePrinter(PrintSemantics):

    def add_transforms(self):
        return [PW._maybe_wrap_str,
                PW._maybe_wrap_regex,
                PW._maybe_wrap_var]

    def __call__(self, to_print):
        curr_str = str(to_print.name)
        transformed = self.run_transforms(to_print, curr_str)

        # Lookup modal
        # if no modal, use fallback modal

        return transformed

class BasicSentenceAwarePrinter(PrintSemantics):


    def __call__(self, to_print):
        assert(to_print.type == PC.SEN_SEN)
        results = [self.sub_map['DEFAULT'](x) for x in to_print.words]

        # TODO use fallback modal?
        return self.use(PC.SEN_JOIN_P).join(results)


class ConstraintSentenceAwarePrinter(PrintSemantics):
    """ Queries and constraints """

    def __call__(self, to_print):
        pass

class TransformAwarePrinter(PrintSemantics):

    def __call__(self, to_print):
        pass

class UUIDAwarePrinter(PrintSemantics):
    def __call__(self, to_print):
        pass

class ContainerAwarePrinter(PrintSemantics):
    """ Production Containers """

    def __call__(self, to_print):
        pass

class StructureAwarePrinter(PrintSemantics):
    """ Ordered structures """

    def __call__(self, to_print):
        pass


class ComplexTypeAwarePrinter(PrintSemantics):
    """ A Top Level Orhcestrator """

    def __call__(self, to_print):
        pass
