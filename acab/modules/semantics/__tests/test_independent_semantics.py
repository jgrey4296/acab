#!/opt/anaconda3/envs/acab/bin/python
from os.path import expanduser, abspath
import sys
sys.path.append(abspath(expanduser("~/github/acab")))

import logging
import unittest
import unittest.mock as mock
from os.path import split, splitext

import acab

config = acab.setup()

from acab.abstract.core.acab_struct import BasicNodeStruct
from acab.abstract.core.node import AcabNode
from acab.abstract.core.values import AcabValue, Sentence
from acab.error.acab_base_exception import AcabBaseException
from acab.error.acab_semantic_exception import AcabSemanticException

from acab.modules.semantics.independent import (BasicNodeSemantics,
                                                ExclusionNodeSemantics)

EXOP         = config.value("MODAL", "exop")
EXOP_enum    = config.enums[EXOP]

class IndependentSemanticTests(unittest.TestCase):

    # InDependent: Node/Exclusion
    # Insert/Remove/Accessible/Get/Up/Down
    ## everything equal except insert for basic v exclusion
    def test_basic_node_insert(self):
        sem = BasicNodeSemantics()
        # create two nodes
        first = sem.make(AcabValue("first"))
        second = sem.make(AcabValue("second"))
        third = sem.make(AcabValue("third"))
        # insert one into the other
        sem.insert(first, second)
        sem.insert(first, third)
        # Verify
        self.assertTrue(second in first)
        self.assertTrue(third in first)

    def test_exclusion_node_insert(self):
        sem = ExclusionNodeSemantics()
        # create two sub nodes
        first = sem.make(AcabValue("first"), data={EXOP: EXOP_enum.EX})
        second = sem.make(AcabValue("second"))
        third = sem.make(AcabValue("third"))
        # insert one into the other
        sem.insert(first, second)
        sem.insert(first, third)
        # Verify
        self.assertFalse(second in first)
        self.assertTrue(third in first)

    def test_basic_node_insert_fail(self):
        sem = BasicNodeSemantics()
        # create two nodes
        first = sem.make(AcabValue("first"))
        second = sem.make(AcabValue("second"))
        # insert one into the other
        sem.insert(first, second)
        with self.assertRaises(AcabSemanticException):
            sem.insert(first, second)



    def test_basic_access_all(self):
        sem = BasicNodeSemantics()
        # create two nodes
        first  = sem.make(AcabValue("first"))
        second = sem.make(AcabValue("second"))
        third  = sem.make(AcabValue("third"))
        # insert one into the other
        sem.insert(first, second)
        sem.insert(first, third)

        accessed = sem.access(first, None, get_all=True)
        self.assertEqual(len(accessed), 2)

    def test_basic_access_specific(self):
        sem = BasicNodeSemantics()
        # create two nodes
        first  = sem.make(AcabValue("first"))
        second = sem.make(AcabValue("second"))
        third  = sem.make(AcabValue("third"))
        # insert one into the other
        sem.insert(first, second)
        sem.insert(first, third)

        accessed = sem.access(first, AcabValue("second"))
        self.assertEqual(len(accessed), 1)
        self.assertEqual(accessed[0].value, "second")

    def test_exclusion_access(self):
        sem = ExclusionNodeSemantics()
        # create two nodes
        first = sem.make(AcabValue("first"))
        second = sem.make(AcabValue("second"), data={EXOP: EXOP_enum.EX})
        # insert one into the other
        sem.insert(first, second)
        accessed = sem.access(first, AcabValue("second", data={EXOP: EXOP_enum.EX}))
        self.assertEqual(len(accessed), 1)
        self.assertEqual(accessed[0].value, "second")
    def test_exclusion_access_fail(self):
        sem = ExclusionNodeSemantics()
        # create two nodes
        first = sem.make(AcabValue("first"))
        second = sem.make(AcabValue("second"), data={EXOP: EXOP_enum.DOT})
        # insert one into the other
        sem.insert(first, second)
        accessed = sem.access(first, AcabValue("second", data={EXOP: EXOP_enum.EX}))
        self.assertEqual(len(accessed), 0)


    def test_basic_access_fail(self):
        sem = BasicNodeSemantics()
        # create two nodes
        first  = sem.make(AcabValue("first"))
        second = sem.make(AcabValue("second"))
        third  = sem.make(AcabValue("third"))
        # insert one into the other
        sem.insert(first, second)
        sem.insert(first, third)

        accessed = sem.access(first, AcabValue("non-existent"))
        self.assertEqual(len(accessed), 0)

    def test_basic_remove(self):
        sem = BasicNodeSemantics()
        # create two nodes
        first  = sem.make(AcabValue("first"))
        second = sem.make(AcabValue("second"))
        third  = sem.make(AcabValue("third"))
        # insert one into the other
        sem.insert(first, second)
        sem.insert(first, third)

        self.assertTrue(second in first)
        sem.remove(first, AcabValue("second"))
        self.assertFalse(second in first)

    def test_basic_remove_fail(self):
        sem = BasicNodeSemantics()
        # create two nodes
        first  = sem.make(AcabValue("first"))
        second = sem.make(AcabValue("second"))
        third  = sem.make(AcabValue("third"))
        # insert one into the other
        sem.insert(first, second)
        sem.insert(first, third)

        self.assertTrue(second in first)
        with self.assertRaises(AcabSemanticException):
            sem.remove(first, AcabValue("fourth"))

    # Dependent  : Trie/FSM/ASP
    # Insert/Remove/Query/Trigger



if __name__ == '__main__':
    unittest.main()
