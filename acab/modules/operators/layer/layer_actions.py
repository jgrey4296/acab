from acab.core.data.production_abstractions import ProductionOperator


class LayerRunRules(ProductionOperator):

    def __call__(self, rules, data=None, engine=None):
        if not isinstance(rules, list):
            rules = [rules]
        assert(all([isinstance(x, Rule) for x in rules]))

        rule_results = []
        for x in rules:
            rule_results += x(ctxs=[data], engine=engine)

        return rule_results


class LayerRunAgenda(ProductionOperator):

    def __call__(self, agenda, *params, data=None, engine=None):
        if data is None:
            data = {}
        # rebind passed in parameters from the caller (ie: layer),
        # to the agenda's parameters
        rebound = Contexts.rebind_across_contexts(agenda.params,
                                                  params, data)

        return agenda(ctxs=[rebound], engine=engine)


class LayerPerform(ProductionOperator):

    def __call__(self, proposals, data=None, engine=None):
        for x,y in proposals:
            y._action(ctxs=[x], engine=engine)
