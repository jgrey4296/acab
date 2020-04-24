"""

A Transform operator for within a rule:

match $x            -> $y:
	value           -> value2
	$var            -> $var2
	@var.sub.query? -> subvar
	sumtype         -> ...
	$x(>2)?         -> ...
	_               -> ...
end


"""
