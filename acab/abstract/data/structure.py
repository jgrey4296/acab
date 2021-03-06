
class DataStructure:
    """
    The Abstract DataStructure Class
    Anything that wants to use StructureSemantics has to fulfill this

    """
    def __init__(self):
        self._root = None


    def query(self, path : Sentence, semantics=None) -> Contexts:
        """ Simple Query: Given a basic path,
        either return the leaf, or None """
        assert(isinstance(semantics, AcabStructureSemantics))
        current = self._root

        return semantics.get(path)

    def query_with_remainder(self, path):
        """
        Query the trie, getting the best leaf along the path
        TODO rename Least Upper Bound?
        eg: For Trie(a.b.c.d.e).query_longest_match(a.b.c.q) -> (c, d.e)
        """
        current = self._root
        queue = [word for word in path]
        while bool(queue):
            # TODO extract query semantics
            current = queue.pop(0)
            logging.debug("Searching {} for: {}".format(str(current), word))
            if current.has_child(word):
                current = current.get_child(word)
            else:
                return (current, queue)
        return (current, queue)


    def contextual_query(self, clause, contexts=None):
        """ Test a single clause,
        annotating contexts upon success and failure """
        assert(isinstance(clause, Sentence))
        if contexts is None:
            contexts = Contexts()

        logging.debug("Testing Clause: {}".format(repr(clause)))
        # Return to root unless clause has a head @ binding
        binding_val = None
        if clause[0].is_at_var:
            binding_val = clause[0].value

        contexts.force_node_position(target=self.root, binding=binding_val)

        # Go down from the root by query element:
        # For each word of the clause sentence, eg: a. in a.b.word
        collapse_on = set()
        for word in clause:
            logging.info("Testing node: {}".format(repr(word)))
            logging.info("Current Contexts: {}".format(len(contexts)))
            if not bool(contexts):
                break

            if word.is_at_var:
                continue

            # test each active alternative
            annotations = _test_word(word, contexts)

            if CTX_OP.collapse in annotations and word.is_var:
                collapse_on.add(word.name)

            # TODO add in context growth restrictions?

        if bool(collapse_on):
            contexts.collapse(collapse_on)

        return contexts


    def match_as_pattern(self, possible_matches, match_func):
        """ Given a trie/node of possible matches, return only actual matches,
        Treating self as the pattern
        """
        if isinstance(possible_matches, Trie):
            possible_matches = possible_matches.root

        if not isinstance(possible_matches, AcabNode):
            raise AcabBaseException()

        final_matches = []
        pattern_nodes = list(possible_matches._children.values())
        # (current pattern position, available choices, match state)
        queue = [(word, pattern_nodes, []) for word in self.root._children.values()]

        while bool(queue):
            current, available_nodes, match_state = queue.pop(0)

            matching_nodes = match_func(current, available_nodes)
            for node in matching_nodes:
                next_match_state = match_state + [(current, node)]
                next_available = list(node._children.values())

                if bool(current):
                    next_patterns = list(current._children.values())
                    queue += [(word, next_available, next_match_state) for word in next_patterns]
                else:
                    final_matches.append(next_match_state)

        return final_matches




####################
# Utilities
def retrieve_potentials(query_word, pair):
    # TODO extract semantics
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
