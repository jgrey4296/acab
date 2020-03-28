"""
Pipeline Parser

Should define a pipline as a set of layers.
These will be verified to provide a flow from base inputs to outputs
by the rules they select

eg:
φ::a.pipeline.example:
	an.output.layer
	an.input.layer
	a.second.layer
	a.third.layer

end

Questions:
Where should modules be specified to be imported?
Where should actions be restricted?
Where should cleanup, and pipeline/layer state change be described?
Where to specify api connections?

"""
