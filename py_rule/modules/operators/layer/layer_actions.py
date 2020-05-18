from py_rule.abstract.action import ActionOp
from py_rule.abstract.transform import TransformOp
from py_rule.abstract.rule import Rule
from py_rule.abstract.contexts import Contexts


class LayerRunRules(TransformOp):

    def __init__(self):
        super(LayerRunRules, self).__init__()

    def __call__(self, rules, data=None, engine=None):
        if not isinstance(rules, list):
            rules = [rules]
        assert(all([isinstance(x, Rule) for x in rules]))

        rule_results = []
        for x in rules:
            rule_results += x(ctxs=[data], engine=engine)

        return rule_results


class LayerRunAgenda(TransformOp):

    def __call__(self, agenda, *params, data=None, engine=None):
        if data is None:
            data = {}
        # rebind passed in parameters from the caller (ie: layer),
        # to the agenda's parameters
        rebound = Contexts.rebind_across_contexts(agenda._vars,
                                                  params, data)

        return agenda(ctxs=[rebound], engine=engine)


class LayerPerform(ActionOp):

    def __call__(self, proposals, data=None, engine=None):
        for x,y in proposals:
            y._action(ctxs=[x], engine=engine)
