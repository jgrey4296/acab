# TODO factor out build_rule

def make_agenda(s, loc, toks):
    rule_type_str, as_rule = build_rule(toks, sem_hint=AGENDA_SEM_HINT)
    return  (PDS.AGENDA, as_rule)

def make_layer(s, loc, toks):
    rule_type_str, as_rule = build_rule(toks, sem_hint=LAYER_SEM_HINT)
    return  (PDS.LAYER, as_rule)


def make_pipeline(s, loc, toks):
    rule_type_str, as_rule = build_rule(toks, sem_hint=PIPELINE_SEM_HINT)
    return  (PDS.PIPELINE, as_rule)
