#!/usr/bin/env python3
#!/usr/bin/env python3
import logging as root_logger
# https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html
from typing import List, Set, Dict, Tuple, Optional, Any
from typing import Callable, Iterator, Union, Match
from typing import Mapping, MutableMapping, Sequence, Iterable
from typing import cast, ClassVar, TypeVar


from acab.core.data.node import AcabNode
from acab.core.data.value import AcabValue
from acab.core.data.sentence import Sentence
from acab.modules.semantics.query_semantic_mixin import QuerySemanticMixin

from acab.core.parsing.consts import ATOM_V

from acab.interfaces import semantic as SI

from acab.modules.analysis.typing import exceptions as te
from acab.modules.analysis.typing import util
from acab.modules.semantics.basic_node_semantics import BasicNodeSemantics

logging = root_logger.getLogger(__name__)

# Log messages to use, because they are long:
LOG_MESSAGES = {}
LOG_MESSAGES['curr_def']         = "Current Definition to Validate: {} : {}"
LOG_MESSAGES['curr_use_set']     = "Current Usage Set: {}"
LOG_MESSAGES['match_type_usage'] = "Matching Type {} onto usage set"
LOG_MESSAGES['mult_child']       = "Current Def has multiple children, checking for conflicts in structure"
LOG_MESSAGES['no_children']      = "Val: No Children, assigning type: {} to {}"
LOG_MESSAGES['validate_top']     = "Validating: {} on {}"

class TypingSemantics(BasicNodeSemantics):
    """
    Thoughts:
    1) distinguish between structure preserving/disturbing semantics,
    2) Annotation semantics as structure preserving without ownership
    3)

    semantics.core : blah end          # create initial semantics
    semantics.core->subcore : bloo end # chain core into subcore
    semantics.core2 : agg end          # create a second semantics
    semantics!core : awef end          # enforce only core 1 (preserve subcore)

    other.semantic.module->semantics!core # chain other.. into core
    # Then, if you get any in the chain:
    run semantics.core   # gets chain of other..->..core->..


    -> : move on L/R abstraction
    .! : move on binary, distrupting vertical abstraction

    Others could be:
    $[a.b.c] : capturing?
    (> <)    : observed?

    """

    def up(self, sen, constructor, world):
        # Lift sentence to correct form
        # assignment: set assigned type, register var
        # definition: prepare definition trie
        pass

    def check(self, world, sentence=None, contextual=False):
        # Check the entire world, or a specific sentence
        pass
    def apply(self, sentence, world):
        #
        pass
    def compare(self, sen1, sen2):
        # Compare two sentences
        pass
    def add(self, sentences, world):
        # add sentences to the typing world
        pass

    def unify(self, sen1, sen2):
        # unify two sentences
        pass

class AssignmentNodeSemantics(BasicNodeSemantics):

    def word(self, word: AcabValue, constructor: Callable) -> AcabNode:
        """ The Most Basic Lift """
        assert(isinstance(word, AcabValue))
        # constructor will default to type bottom if word.type is none
        _type = word.type
        return constructor(word, _type=_type)

class DefinitionNodeSemantics(BasicNodeSemantics):

    def up(self, word : AcabValue, constructor : Callable) -> AcabNode:
        """ The Most Basic Lift """
        assert(isinstance(word, AcabValue))
        return constructor(word)


    def add(self, node : AcabNode, word: AcabValue, node_constructor : Callable) -> Tuple[bool, AcabNode]:
        assert(isinstance(node, AcabNode))
        assert(isinstance(word, AcabValue))

        is_new_node = False
        result = self.get(node, word)

        if result is None:
            result = self.up(word, node_constructor)
            node.add(result)
            is_new_node = True
        elif (isinstance(word, TypeDefinition)
              and result.definition is not None
              and result.definition.structure != word.structure):
            raise te.TypeRedefinitionException(result.definition)


        return True, result

class StructNodeSemantics(BasicNodeSemantics):
    # TODO Locate listeners in semantics not WM

    def __init__(self, node_semantics : QuerySemanticMixin, node_type=AcabNode):
        self._ns = node_semantics
        self._node_type = node_type

    def set_node_type(self, node_type : AcabNode):
        self._node_type = node_type

    def set_node_semantics(self, ns : QuerySemanticMixin):
        self._ns = ns


    def add(self, structure : AcabStruct, to_add : list[Sentence]) -> list[AcabNode]:
        """ Inserting a coherent set of sentences into the structure """
        raise NotImplementedError()

    def get(self, structure : AcabStruct, sentence) -> list[AcabNode]:
        """ Getting a path of nodes corresponding to the sentence """
        raise NotImplementedError()

    def contain(self, structure, sentence) -> bool:
        """ Can the sentence be found in the structure """
        raise NotImplementedError()

    def delete(self, structure, sentence) -> list[AcabNode]:
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


    def filter_candidates(self, structure, candidates, match_func):
        raise NotImplementedError()
class VarNodeSemantics(BasicNodeSemantics):

    def up(self, word: AcabValue, constructor: Callable) -> AcabNode:
        """ Lifting a value to a data node """
        # could include vocabulary tracking a la spacy
        assert(isinstance(word, AcabValue))
        # constructor will default to type bottom if word.type is none
        _type = word.type
        return constructor(word, _type=_type)
