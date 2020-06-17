"""
Create a numpy (v1.19) generator value
and operators to draw from it using different distributions

σ::RNGGen: seed.$x

a.string.rng(::RNGGen).seed."the seed"

"""
import numpy as np

from acab.abstract.value import AcabValue


class RNGGen(AcabValue):

    def __init__(self, seed=None, data=None):
        # TODO convert string to int for seed
        rng = np.random.default_rng(seed=seed)
        super(RNGGen, self).__init__(rng, "rng", data=data)
        self._data["RNG_SEED"] = seed

        # TODO convert seed to string for output


    def pprint(self, opts, **kwargs):
        # TODO output an rng with seed
        pass

# Operators:  https://numpy.org/devdocs/reference/random/generator.html
# draw (distribution params)
# draw (distribution targetvar)
