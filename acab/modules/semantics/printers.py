
from acab.abstract.config.config import GET
from acab.abstract.interfaces.semantic_interfaces import PrintSemantics
from acab.abstract.printing import consts as PC
from acab.abstract.printing import wrappers as PW

config = GET()

# Independent
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
                PW._maybe_wrap_var,
                PW._maybe_wrap_modals]

    def __call__(self, to_print):
        curr_str = str(to_print.name)
        transformed = self.run_transforms(to_print, curr_str)

        return transformed


class UUIDAwarePrinter(PrintSemantics):
    def add_transforms(self):
        return [PW._maybe_wrap_str,
                PW._maybe_wrap_regex,
                PW._maybe_wrap_var,
                PW._maybe_wrap_modals]

    def __call__(self, to_print):
        curr_str = str(to_print.name)
        transformed = self.run_transforms(to_print, curr_str)

        return f"({transformed} : {to_print.uuid})"


# Dependent
class BasicSentenceAwarePrinter(PrintSemantics):

    def add_transforms(self):
        return [PW._remove_trailing_modal]

    def __call__(self, to_print):
        assert(to_print.type == PC.SEN_SEN)
        results = [self.lookup(x)(x) for x in to_print.words]
        joined  = self.use(PC.SEN_JOIN_P).join(results)
        transformed = self.run_transforms(to_print, joined)

        return transformed


class ConstraintSentenceAwarePrinter(PrintSemantics):
    """ Queries and constraints """

    def __call__(self, to_print):
        pass

class TransformAwarePrinter(PrintSemantics):

    def __call__(self, to_print):
        pass


# Abstraction
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
