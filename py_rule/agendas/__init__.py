"""
Agendas control the results of running a set of Rules.

ProposedActions = Eval(RuleSet, State)
Then:
SelectedAction = anAgenda(ProposedActions)

Agendas then are a filter/rank/transform function on (grouped) actions
"""
