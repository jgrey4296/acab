from acab.abstract.interfaces import semantic_interfaces as SI

class TypingSemantics(SI.SemanticSystem, SI.StructureSemantics):
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
