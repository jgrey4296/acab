# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar

from acab.abstract.core.value import AcabValue
from acab.abstract.core.sentence import Sentence
from acab.abstract.data.node import AcabNode
from acab.abstract.data.contexts import Contexts
from acab.abstract.data.structure import DataStructure
from acab.abstract.data.node_semantics import AcabNodeSemantics

from acab.config import AcabConfig

util = AcabConfig.Get()

NEGATION_S = util("Parsing.Structure", "NEGATION_S")
CONSTRAINT_S = util("Parsing.Structure", "CONSTRAINT_S")
AT_BIND_S = util("Parsing.Structure", "AT_BIND_S")

class AcabStructureSemantics:
    # TODO Locate listeners in semantics not WM

    def __init__(self, node_semantics : AcabNodeSemantics, node_type=AcabNode):
        self._ns = node_semantics
        self._node_type = node_type

    def set_node_type(self, node_type : AcabNode):
        self._node_type = node_type

    def set_node_semantics(self, ns : AcabNodeSemantics):
        self._ns = ns


    def add(self, structure : DataStructure, to_add : List[Sentence]) -> List[AcabNode]:
        """ Inserting a coherent set of sentences into the structure """
        raise NotImplementedError()

    def get(self, structure : DataStructure, sentence) -> List[AcabNode]:
        """ Getting a path of nodes corresponding to the sentence """
        raise NotImplementedError()

    def contain(self, structure, sentence) -> bool:
        """ Can the sentence be found in the structure """
        raise NotImplementedError()

    def delete(self, structure, sentence) -> List[AcabNode]:
        """ Remove a sentence from the structure """
        raise NotImplementedError()


    def query(self, structure, clause : Sentence, ctxs : Contexts, engine : 'Engine'):
        """ Answer a clause asked of the data structure """
        # TODO is this part of call semantics?
        # open / closed world
        # depth / breath search
        # match as pattern?
        # return type
        raise NotImplementedError()


    def down(self, data_structure : DataStructure) -> List[Sentence]:
        """ Take a complex data structure down to basic sentences """
        raise NotImplementedError()

    def lift(self, sentences : List[Sentence]) -> DataStructure:
        """ Raise a set of sentences into a data structure """
        raise NotImplementedError()



#--------------------------------------------------

def retrieve_potentials(query_word, pair):
    # TODO should this be a node semantic?
    potentials = []
    data, node = pair
    # Handle query on var
    if query_word.is_var and query_word.name not in data:
        potentials = node._children.values()
    elif query_word.is_var:
        assert(query_word.name in data)
        value = data[query_word.name]
        if node.has_child(value):
            potentials.append(node.get_child(value))

    elif node.has_child(query_word):
        potentials.append(node.get_child(query_word))


    return potentials


def run_subbinds(node, regexs, data, i):
    # TODO Factor this into queryop subbind?
    invalidated = False
    new_data = {}
    for regex in regexs:
        # TODO: expand regex usng data first
        result = search(regex._params[0].value, node.name)
        if result is None:
            invalidated = True
            break

        for k, v in result.groupdict().items():
            if k not in data:
                new_data[k] = v
            elif data[k] != v:
                invalidated = True
                break

    if invalidated:
        node = None
        final_data = None
    else:
        final_data = data.copy()
        final_data.update(new_data)

    return (i, final_data, node)

def validate_and_split_constraints(word, ctx=None, engine=None):
    """ Split tests into (alphas, betas, sub_binds),
    Also connect Components to actual operators
    """
    # TODO make this a node semantic
    if CONSTRAINT_S not in word._data:
        return ([], [], [], set())

    comps = [word.verify(ctx=ctx, engine=engine) for word in word._data[CONSTRAINT_S] if isinstance(word, QueryComponent)]
    others = set([word for word in word._data[CONSTRAINT_S] if not isinstance(word, QueryComponent)])
    alphas = []
    betas = []
    sub_binds = []
    for c in comps:
        if c.is_sub_bind_test:
            sub_binds.append(c)
        elif c.is_alpha_test:
            alphas.append(c)
        else:
            betas.append(c)

    return (alphas, betas, sub_binds, others)

def _test_word(query_word, query_context):
    """ query word, with a sequence of tests,
    in all available bind groups, BFS manner
    """
    assert(not query_word.is_at_var)
    engine = query_context._engine
    # TODO: validate on activate context too
    alphas, betas, regexs, annotations = validate_and_split_constraints(query_word, engine=engine)
    callable_annotations = [word for word in annotations if hasattr(word, "__call__")]
    data_set = {word : d for word,d in enumerate(query_context.pairs())}
    pairs  = enumerate(query_context.pairs())
    query_context.clear()


    potentials = [(i, d[0], passing) for i,d in pairs for passing in retrieve_potentials(query_word, d)]
    # Apply Tests
    # TODO: test type instance if not ATOM
    passing = [run_subbinds(n, regexs, d, i) for i,d,n in potentials
               if test_alphas(n, alphas, d, engine=engine)
               and test_betas(n, betas, d, engine=engine)
               and test_annotations(n, callable_annotations, d, engine)]

    to_add = [add_var_to_context(i, query_word, d, n) for i,d,n in passing if n is not None]

    query_context.append(*to_add, fail_dict=data_set)
    return annotations


def test_alphas(node, comps, data=None, engine=None):
    """ Run alpha tests against word retrieved value """
    return all([word(node, data=data, engine=engine) for word in comps])

def test_betas(node, comps, data=None, engine=None):
    """ Run word beta tests against word retrieved value, with supplied bindings """
    return all([word(node, data=data, engine=engine) for word in comps])

def test_annotations(node, comps, data=None, engine=None):
    return all([word(node, data=data, engine=engine) for word in comps])
