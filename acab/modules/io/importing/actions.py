from acab.abstract.core.production_abstractions import ProductionOperator

from acab.error.acab_import_exception import AcabImportException


class ImportQuery(ProductionOperator):
    """ Treats clauses as py import statements """
    def __call__(self, ctxs=None, engine=None):
        try:
            return engine.load_modules(*self.clauses)
        except AcabImportException:
            return []


class ModuleExtractTransform(ProductionOperator):
    """ Extracts values matching a predicate from a module """
    def __call__(self, pred, mod, data=None, engine=None):
        # Filter the module by the predicate,
        # returning only those that pass
        raise NotImplementedError()


class OperatorMassNameSubst(ProductionOperator):
    """
    Take a bag of operators, and apply a regex to their registered names.
    Eg: {ActionAdd, ActionSub, ActionMul} /Action// -> {Add,Sub,Mul}
    """
    def __call__(self, ops, name_regex, data=None, engine=None):
        raise NotImplementedError()


class FlattenTrie(ProductionOperator):
    """
    Take a Trie of operators, and flatten it.
    Eg: flatten {mod.actions.add, mod.transforms.flatten} -> {mod.add, mod.flatten}
    """
    def __call__(self, trie, data=None, engine=None):
        raise NotImplementedError()


class AliasAction(ProductionOperator):
    """ Alias a set of actions into a TagEnv
    Constructs TagEnv if necessary,
    appends if already exists
    """
    def __call__(self, target, source, data=None, engine=None):
        engine.register_operator_dict(target, source)


class TagEnvironment:
    """
    A Dictionary of Operators,
    which using the Tag of in a rule
    sets the environment to active,
    enabling access and use of the operators
    """
    pass
