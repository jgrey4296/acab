from py_rule.abstract.layer import LayerAction


class LayerRunRules(LayerAction):

    def __init__(self):
        super(LayerRunRules, self).__init__()

    def __call__(self, rules, data=None, engine=None):
        assert(all([isinstance(x, Rule) for x in rules]))

        rule_results = []
        for x in rules:
            rule_results += x(ctxs=[data], engine=engine)

        return rule_results


class LayerRunAgenda(LayerAction):

    def __call__(self, agenda, *params, data=None, engine=None):
        if data is None:
            data = [{}]
        # rebind passed in parameters from the caller (ie: layer),
        # to the agenda's parameters
        rebound = util.rebind_across_contexts(agenda._vars,
                                              params,
                                              data[0])
        return agenda(ctxs=[rebound], engine=engine)


class LayerPerform(LayerAction):

    def __call__(self, proposals, data=None, engine=None):
        for x,y in proposals:
            y._action(x, data=data, engine=engine)
