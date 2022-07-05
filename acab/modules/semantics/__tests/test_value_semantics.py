#!/opt/anaconda3/envs/acab/bin/python
import sys
from os.path import abspath, expanduser

sys.path.append(abspath(expanduser("~/github/acab")))
import logging as logmod
import unittest
import unittest.mock as mock
from os.path import split, splitext

logging = logmod.getLogger(__name__)

import warnings

import acab

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    config = acab.setup()

    import acab.core.defaults.value_keys as DS
    from acab.core.data.acab_struct import BasicNodeStruct
    from acab.core.data.node import AcabNode
    from acab.core.value.instruction import Instruction
    from acab.core.value.sentence import Sentence
    from acab.core.value.value import AcabValue
    from acab.error.base import AcabException
    from acab.error.semantic import AcabSemanticException
    from acab.modules.semantics.values import (BasicNodeSemantics,
                                            ExclusionNodeSemantics)

EXOP         = config.prepare("MODAL", "exop")()
EXOP_enum    = config.enums[EXOP]


class ValueSemanticTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        LOGLEVEL      = logmod.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        cls.file_h        = logmod.FileHandler(LOG_FILE_NAME, mode="w")

        cls.file_h.setLevel(LOGLEVEL)
        logging = logmod.getLogger(__name__)
        logging.root.setLevel(logmod.NOTSET)
        logging.root.addHandler(cls.file_h)
        logging.root.handlers[0].setLevel(logmod.WARNING)

    @classmethod
    def tearDownClass(cls):
        logmod.root.removeHandler(cls.file_h)

    # InDependent: Node/Exclusion
    # Insert/Remove/Accessible/Get/Up/Down
    ## everything equal except insert for basic v exclusion
    def test_basic_node_insert(self):
        """ Check basic node insertion using semantics """
        sem = BasicNodeSemantics()
        # create two nodes
        first  = sem.make(AcabValue("first"))
        second = sem.make(AcabValue("second"))
        third  = sem.make(AcabValue("third"))
        # insert one into the other
        sem.insert(first, second)
        sem.insert(first, third)
        # Verify
        self.assertTrue(second in first)
        self.assertTrue(third in first)

    def test_exclusion_node_insert(self):
        """ Check basic exclusion semantic insertion """
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
        """ Check repeated insertion fails """
        sem = BasicNodeSemantics()
        # create two nodes
        first = sem.make(AcabValue("first"))
        second = sem.make(AcabValue("second"))
        # insert one into the other
        sem.insert(first, second)
        with self.assertRaises(AcabSemanticException):
            sem.insert(first, second)



    def test_basic_access_all(self):
        """ Check semantics provides access to all child nodes """
        sem = BasicNodeSemantics()
        # create two nodes
        first  = sem.make(AcabValue("first"))
        second = sem.make(AcabValue("second"))
        third  = sem.make(AcabValue("third"))
        # insert one into the other
        sem.insert(first, second)
        sem.insert(first, third)

        accessed = sem.access(first, None)
        self.assertEqual(len(accessed), 2)

    def test_basic_access_specific(self):
        """ Check semantics provide access to specific children """
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
        """ Check exclusion semantics provides access to specific children """
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
        """ Check exclusion semantics enforces correct modality on access of children """
        sem = ExclusionNodeSemantics()
        # create two nodes
        first = sem.make(AcabValue("first"))
        second = sem.make(AcabValue("second"), data={EXOP: EXOP_enum.DOT})
        # insert one into the other
        sem.insert(first, second)
        with self.assertRaises(AcabSemanticException):
            sem.access(first, AcabValue("second", data={EXOP: EXOP_enum.EX}))


    def test_basic_access_fail(self):
        """ Check access of non existent children fails """
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
        """ Check removing nodes via semantics works """
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
        """ Check removing non existent children fails """
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


    def test_node_sentence_containment(self):
        sem = BasicNodeSemantics()
        # create two nodes
        first  = sem.make(AcabValue("first"))
        second = sem.make(Sentence(["second", "third"]))
        # insert one into the other
        new_node = sem.insert(first, second)
        self.assertIn(Sentence(["second", "third"]), first)


        

    @unittest.skip("broken")
    def test_node_access_str_vs_atom(self):
        """ Check accessing a str vs an atom is differentiated """
        sem        = BasicNodeSemantics()
        root       = sem.make(AcabValue("root"))
        atom_value = sem.make(AcabValue("value"))
        str_value  = sem.make(AcabValue(atom_value.value, data={DS.TYPE_INSTANCE : "string"}))
        self.assertNotEqual(atom_value.value.type, str_value.value.type)
        sem.insert(root, atom_value)
        result = sem.access(root, str_value.value)

        self.assertFalse(result)

    @unittest.skip("broken")
    def test_node_insert_str_vs_atom(self):
        """ Check inserting a str vs an atom is differentiated """
        sem        = BasicNodeSemantics()
        root       = sem.make(AcabValue("root"))
        atom_value = sem.make(AcabValue("value"))
        str_value  = sem.make(AcabValue(atom_value.value, data={DS.TYPE_INSTANCE : "string"}))
        self.assertNotEqual(atom_value.value.type, str_value.value.type)
        sem.insert(root, atom_value)
        self.assertEqual(len(root), 1)
        sem.insert(root, str_value)
        self.assertEqual(len(root), 2)


if __name__ == '__main__':
    unittest.main()
