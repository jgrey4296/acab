# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match, AliasType
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar

from acab.abstract.core.value import AcabValue
from acab.abstract.core.sentence import Sentence
from acab.abstract.data.node import AcabNode
from acab.abstract.data.trie import Trie
from acab.abstract.data.contexts import Contexts

class AcabStructureSemantics:
    # TODO Locate listeners in semantics not WM

    def __init__(self, node_semantics):
        assert(isinstance(node_semantics, AcabNodeSemantics))
        self._ns = node_semantics


    def add(self, structure : DataStructure, to_add : List[Sentence]) -> List[AcabNode]
        """ Inserting a coherent set of sentences into the structure """
        raise NotImplementedError()

    def get(self, structure : DataStructure, sentence) -> List[AcabNode]:
        """ Getting a path of nodes corresponding to the sentence """
        raise NotImplementedError()

    def contain(self, structure, sentence) -> Bool:
        """ Can the sentence be found in the structure """
        raise NotImplementedError()

    def delete(self, structure, sentence) -> List[AcabNode]:
        """ Remove a sentence from the structure """
        raise NotImplementedError()


    def query(self, structure, clause : Sentence, ctxs : Contexts, engine : AcabEngine):
        """ Answer a clause asked of the data structure """
        # TODO is this part of call semantics?
        # open / closed world
        # depth / breath search
        # match as pattern?
        # return type
        pass


    def down(self, data_structure : DataStructure) -> List[Sentence]:
        """ Take a complex data structure down to basic sentences """
        raise NotImplementedError()

    def lift(self, sentences : List[Sentence]) -> DataStructure:
        """ Raise a set of sentences into a data structure """
        raise NotImplementedError()


class BasicTrieSemantics(AcabStructureSemantics):

    def __init__(self, node_semantics, node_type,
                 leaf_type=None, update_data=None, update_func=None):
        super(BasicTrieSemantics, self).__init__(node_semantics)

        self._node_type = node_type
        self._leaf_type = leaf_type or node_type
        self._update_data = {}
        self._update_func = update_func or lambda a,b,c: a

        if update_data is not None:
            self._update_data.update(update_data)

    def add(self, structure, to_add, leaf_data=None):
        assert(isinstance(structure, Trie))
        assert(isinstance(to_add, list))

        to_add = sorted(to_add,
                        key=lambda x: NEGATION_S in x._data and x._data[NEGATION_S])
        if leaf_data is None:
            leaf_data = {}

        # Get the root
        current = None
        current_path = []
        for sen in to_add:
            current = structure.root
            if NEGATION_S in sen._data and sen._data[NEGATION_S]:
                self.delete(structure, sen)
                continue

            # Add to nodes
            for word in to_add:
                current = self._ns.add(current, word)
                self._update_func(current, curr_path, self._update_data)
                current_path.append(current)

            current._data.update(leaf_data)
            # TODO Register new nodes with structure weak index

        return current_path[:-1]

    def get(self, structure, sentence):
        assert(isinstance(structure, Trie))
        assert(isinstance(sentence, Sentence))

        # Get the root
        current = structure._root
        # Get Nodes
        for word in to_add:
            current = self._ns.get(current, word)
            if current is None:
                return []

        return [current]

    def contain(self, structure, sentence):
        return bool(self.get(structure, sentence))

    def delete(self, structure, sentence):
        retrieved = self.get(structure, sentence[:-1])
        return_list = []
        if bool(retrieved):
            removed = self._ns.delete(retrieved[0], sentence[-1])
            if removed is not None:
                return_list.append(removed)

        return return_list

    def query(self, structure, query, ctxs, engine):
        initial_context = Contexts(start_node=structure.root,
                                   bindings=ctxs, engine=engine)

        pos, neg = query.split_clauses()
        try:
            for clause in pos:
                self._clause_query(structure, clause, initial_context, engine)
            for clause in neg:
                self._clause_query(structure, clause, initial_context, engine, is_negative=True)
        except AcabSemanticsException as e:
            logging.warning(str(e))
            initial_context.demote_failures()
            # TODO set context query_history and remainder
        finally:
            return initial_context


    def down(self, data_structure):
        output = []
        queue = [([], x) for x in data_structure._root]

        while bool(queue):
            curr_path, current_node = queue.pop(0)
            total_path = curr_path + [current_node.value]
            if not bool(current_node) or isinstance(current_node.value, AcabStatement):
                if leaf_predicate is None or leaf_predicate(current_node):
                    as_sentence = Sentence(total_path)
                    output.append(as_sentence)

            if bool(current_node):
                queue += [(total_path, x) for x in current_node]

        return output


    def _start_word_semantics(self, structure, contexts, clause):
        binding = None
        if word.is_at_var:
            binding = word.value

        contexts.force_node_position(target=structure.root,
                                     binding=binding)

        if binding is not None:
            return clause[1:]

        return clause


    def _collapse_semantics(self, ctxs, collapse_set):
         if bool(collapse_set):
            contexts.collapse(collapse_on)

    def _negation_semantics(self, contexts, clause):
        if clause._data[NEGATION_S]:
            contexts.invert()

    def _failure_semantics(self, contexts, clause):
        # add all failures back in, if theres a default value
        if FALLBACK_S in clause._data and bool(clause._data[FALLBACK_S]):
            contexts.promote_failures(clause._data[FALLBACK_S])
        else:
            contexts.demote_failures()


    def _clause_query(self, structure, clause : Sentence, contexts, engine):
        """ Test a single clause,
        annotating contexts upon success and failure """
        logging.debug("Testing Clause: {}".format(repr(clause)))

        clause = self._start_word_semantics(structure, contexts, clause)
        # Go down from the root by query element:
        # For each word of the clause sentence, eg: a. in a.b.word
        collapse_on = set()
        for word in clause:
            logging.info("Testing node: {}".format(repr(word)))
            logging.info("Current Contexts: {}".format(len(contexts)))
                        # test each active alternative
            annotations = _test_word(word, contexts)

            if CTX_OP.collapse in annotations and word.is_var:
                collapse_on.add(word.name)

            # TODO add in context growth restrictions?
            if not bool(contexts):
                break

        self._collapse_semantics(contexts, collapse_on)
        self._negation_semantics(contexts, clause)
        self._failure_semantics(contexts, clause)

        if not bool(contexts):
            raise AcabSemanticException("No successful contexts", clause)

        return contexts


class RDFSemantics(AcabStructureSemantics):
    """
    Semantics for Subject Predicate Object Triples
    """
    def __init__(self, node_semantics, node_type,
                 leaf_type=None, update_data=None, update_func=None):
        super(BasicTrieSemantics, self).__init__(node_semantics)

        self._node_type = node_type
        self._leaf_type = leaf_type or node_type
        self._update_data = {}
        self._update_func = update_func or lambda a,b,c: a

        if update_data is not None:
            self._update_data.update(update_data)

    def add(self, structure, to_add, leaf_data=None):
        assert(isinstance(structure, Trie))
        assert(isinstance(to_add, Sentence))

        # Get the root
        current = structure.root
        current_path = []
        # Add to nodes
        for word in to_add:
            current = self._ns.add(current, word)
            self._update_func(current, curr_path, self._update_data)
            current_path.append(current)

            # TODO Register new nodes with structure weak index

        return current

    def get(self, structure, sentence):
        pass

    def contain(self, structure, sentence):
        pass

    def delete(self, structure, sentence):
        pass

    def query(self, structure, clause, ctxs, engine):
        pass

    def down(self, data_structure):
        pass

    def lift(self, sentences):
        pass


class ReteSemantics(AcabStructureSemantics):
    """
    Semantics of a compiled rete net
    """
    def __init__(self, node_semantics, node_type,
                 leaf_type=None, update_data=None, update_func=None):
        super(BasicTrieSemantics, self).__init__(node_semantics)

        self._node_type = node_type
        self._leaf_type = leaf_type or node_type
        self._update_data = {}
        self._update_func = update_func or lambda a,b,c: a

        if update_data is not None:
            self._update_data.update(update_data)

    def add(self, structure, to_add, leaf_data=None):
        assert(isinstance(structure, Trie))
        assert(isinstance(to_add, Sentence))

        # Get the root
        current = structure.root
        current_path = []
        # Add to nodes
        for word in to_add:
            current = self._ns.add(current, word)
            self._update_func(current, curr_path, self._update_data)
            current_path.append(current)

            # TODO Register new nodes with structure weak index

        return current

    def get(self, structure, sentence):
        pass

    def contain(self, structure, sentence):
        pass

    def delete(self, structure, sentence):
        pass

    def query(self, structure, clause, ctxs, engine):
        pass

    def down(self, data_structure):
        pass

    def lift(self, sentences):
        pass


class TypingSemantics(AcabStructureSemantics):
        def __init__(self, node_semantics):
        assert(isinstance(node_semantics, AcabNodeSemantics))
        self._ns = node_semantics


    def add(self, structure : DataStructure, to_add : List[Sentence]) -> List[AcabNode]
        """ Inserting a coherent set of sentences into the structure """
        raise NotImplementedError()

    def get(self, structure : DataStructure, sentence) -> List[AcabNode]:
        """ Getting a path of nodes corresponding to the sentence """
        raise NotImplementedError()

    def contain(self, structure, sentence) -> Bool:
        """ Can the sentence be found in the structure """
        raise NotImplementedError()

    def delete(self, structure, sentence) -> List[AcabNode]:
        """ Remove a sentence from the structure """
        raise NotImplementedError()


    def query(self, structure, clause : Sentence, ctxs : Contexts, engine : AcabEngine):
        """ Answer a clause asked of the data structure """
        # TODO is this part of call semantics?
        # open / closed world
        # depth / breath search
        # match as pattern?
        # return type
        pass


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
        if value in node:
            potentials.append(node.get_child(value))

    elif query_word in node:
        potentials.append(node.get_child(query_word))


    return potentials

def add_var_to_context(i, query_word, new_data, passing_node):
    """ Prepare a tuple for adding to the context """
    # TODO move this to Contexts
    assert(isinstance(query_word, AcabValue))
    assert(isinstance(passing_node, AcabNode))

    if query_word.is_var:
        new_data[query_word.name] = passing_node.value
        new_data[AT_BIND_S + query_word.name] =  passing_node

    return (i, new_data, passing_node)


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
