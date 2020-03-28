"""
Layer Parser

Should define a layer as a combination of rule selectors
and a sequence of agendas

eg:
Γ::a.layer:
	a.rule.selector.$x?
	another.rule.selector.$y?

	$x -> primary.agenda
	$y -> secondary.agenda

end

"""
