#!/usr/bin/env python3
import acab.abstract.interfaces.semantic_interfaces as SI

Sentence = 'Sentence'

# TODO factor out contexts
class QueryHandlers(SI.SemanticHandler):
    """ Utility sub semantic behaviour """

    def _start_word_semantics(self, structure, contexts, clause):
        binding = None
        if clause[0].is_at_var:
            binding = clause[0].value

        contexts.force_node_position(target=structure.root, binding=binding)

        if binding is not None:
            return clause[1:]

        return clause

    def _collapse_semantics(self, ctxs, collapse_set):
        if bool(collapse_set):
            ctxs.collapse(collapse_set)

    def _negation_semantics(self, contexts, clause):
        if NEGATION_S in clause.data and clause.data[NEGATION_S]:
            contexts.invert()

    def _failure_semantics(self, contexts, clause):
        # add all failures back in, if theres a default value
        if QUERY_FALLBACK_S in clause.data and bool(clause.data[QUERY_FALLBACK_S]):
            contexts.promote_failures(clause.data[QUERY_FALLBACK_S])
        else:
            contexts.demote_failures()

    def _clause_query(self, structure, clause: Sentence, contexts, engine):
        """ Test a single clause,
        annotating contexts upon success and failure """
        logging.debug("Testing Clause: {}".format(repr(clause)))

        clause = self._start_word_semantics(structure, contexts, clause)
        #  o down from the root by query element:
        # For each word of the clause sentence, eg: a. in a.b.word
        collapse_on = set()
        for word in clause:
            tests, annotations = self._validate_and_split_constraints(
                word, ctx=contexts, engine=engine
            )
            # This is hardcoded currently
            if CTX_OP.collapse in annotations and word.is_var:
                collapse_on.add(word.name)

            logging.debug("Testing node: {}".format(repr(word)))
            logging.debug("Current Contexts: {}".format(len(contexts)))
            node_groups, ancestor_tracker = contexts.group_by_type()
            # Pair each context triple with a semantics to use
            group_semantics = {}
            for x,y in node_groups.items():
                sem = self.retrieve_semantics(x)
                if sem not in group_semantics:
                    group_semantics[sem] = []
                group_semantics[sem] += y

            # test each active alternative
            passing_candidates = [
                r
                for sem, triples in group_semantics.items()
                for r in sem.test_candidates(word, triples, tests, engine)
            ]

            # Merge then add
            contexts.clear()
            contexts.append(passing_candidates, ancestor_tracker)

            # TODO add in context growth restrictions?
            if not bool(contexts):
                break

        self._collapse_semantics(contexts, collapse_on)
        self._negation_semantics(contexts, clause)
        self._failure_semantics(contexts, clause)

        if not bool(contexts):
            raise AcabSemanticException("No successful contexts", str(clause))

        return contexts

    def _validate_and_split_constraints(self, word, ctx=None, engine=None):
        """ Split tests into (alphas, betas, sub_binds),
        Also connect Components to actual operators
        """
        if CONSTRAINT_S not in word.data:
            return (None, set())

        constraints = word.data[CONSTRAINT_S]
        annotations = set()
        callable_annotations = []
        alphas = []
        betas = []
        sub_binds = []
        variable_ops = []
        for c in constraints:
            if not isinstance(c, ProductionComponent) and hasattr(c, "__call__"):
                callable_annotations.append(c)
            # intentionally not elif:
            if not isinstance(c, ProductionComponent):
                annotations.add(c)
            # intentionally elifs:
            elif c.is_var:
                variable_ops.append(c)
            elif c.is_sub_bind_test:
                sub_binds.append(c)
            elif c.is_alpha_test:
                alphas.append(c)
            else:
                betas.append(c)

        return (
            (alphas, betas, sub_binds, callable_annotations, variable_ops),
            annotations,
        )



    def _get(self, structure, sentence):
        assert isinstance(structure, AcabStruct)
        assert isinstance(sentence, Sentence)

        # Get the root
        current = structure.root
        # Get Nodes
        for word in sentence:
            mapping = self.retrieve_semantics(type(current))
            current = mapping.get(current, word)
            if current is None:
                return []

        return [current]
