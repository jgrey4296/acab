def build_query(s, loc, toks):
    query = ProductionContainer(value=toks[:],
                                data={SEMANTIC_HINT_V: QUERY_SEM_HINT})
    return (PDS.QUERY, query)

def build_action_component(s, loc, toks):
    params = []
    if PDS.LEFT in toks:
        params.append(toks[PDS.LEFT])
    if PDS.RIGHT in toks:
        params = toks[PDS.RIGHT][:]
    op = toks[PDS.OPERATOR][0]
    filtered_params = [x[0] if len(x) == 1 else x for x in params]
    return ProductionComponent(value=op, params=filtered_params, sugared=PDS.LEFT in toks)


def build_action(s, loc, toks):
    # TODO: check this
    clauses = [x if isinstance(x, ProductionComponent)
               else ProductionComponent(value=Sentence.build([PDS.DEFAULT_ACTION]), params=[x]) for x in toks]

    act = ProductionContainer(value=clauses,
                              data={SEMANTIC_HINT_V : ACTION_SEM_HINT})

    return (PDS.ACTION, act)

def build_transform_component(s, loc, toks):
    params = []
    if PDS.LEFT in toks:
        params.append(toks[PDS.LEFT][0])
    params += toks[PDS.RIGHT][:]

    op = toks[PDS.OPERATOR][0]
    if isinstance(op, str):
        op = Sentence.build([op])

    rebind = toks[PDS.TARGET][0]
    filtered_params = [x[0] if len(x) == 1 else x for x in params]

    return ProductionComponent(value=op,
                               params=filtered_params,
                               rebind=rebind,
                               sugared=PDS.LEFT in toks)

def build_transform(s, loc, toks):
    trans = ProductionContainer(value=toks[:],
                                data={SEMANTIC_HINT_V : TRANSFORM_SEM_HINT})
    return (PDS.TRANSFORM, trans)

def build_rule(s, loc, toks, sem_hint=None):
    # Get Conditions
    if PDS.QUERY in toks:
        c = toks[PDS.QUERY][0][1]
        assert(isinstance(c, ProductionContainer))
    else:
        c = None

    # Get Transform
    if PDS.TRANSFORM in toks:
        t = toks[PDS.TRANSFORM][0][1]
        assert(isinstance(t, ProductionContainer))
    else:
        t = None

    # Get Action
    if PDS.ACTION in toks:
        a = toks[PDS.ACTION][0][1]
        assert(isinstance(a, ProductionContainer))
    else:
        a = None

    structure = {
        PDS.QUERY     : c,
        PDS.TRANSFORM : t,
        PDS.ACTION    : a
        }

    if sem_hint is None:
        sem_hint = RULE_SEM_HINT


    rule = ProductionStructure(structure=structure,
                               data={SEMANTIC_HINT_V: sem_hint})
    return (rule.type, rule)
