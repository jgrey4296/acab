#https://docs.python.org/3/library/unittest.html
from os.path import splitext, split
import unittest
import unittest.mock as mock
import logging as root_logger
logging = root_logger.getLogger(__name__)

from acab import setup
config = setup()

from acab.core.data.value import Sentence
from acab.core.data.value import AcabValue, Instruction

from acab.core.data import instruction as PO

BIND_S               = config.prepare("Value.Structure", "BIND")()
OPERATOR_TYPE_PRIM_S = config.prepare("Type.Primitive", "OPERATOR")()

class StatementTests(unittest.TestCase):
    """ Test the construction of production abstractions """

    @classmethod
    def setUpClass(cls):
        LOGLEVEL = root_logger.DEBUG
        LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
        root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

        console = root_logger.StreamHandler()
        console.setLevel(root_logger.INFO)
        root_logger.getLogger('').addHandler(console)
        logging = root_logger.getLogger(__name__)

    #----------
    def test_init_operator(self):
        """ Check an operator can be created, and is of the correct type """
        op = PO.ProductionOperator()
        self.assertIsInstance(op, PO.ProductionOperator)
        # TODO OPERATOR, COMPONENT
        self.assertEqual(op.type, Sentence.build([OPERATOR_TYPE_PRIM_S]))

    def test_component_init(self):
        """ Check a component can be created """
        val = PO.ProductionComponent(value=Sentence.build(["testop"]))
        self.assertIsInstance(val, PO.ProductionComponent)
        self.assertIsInstance(val, AcabValue)

    def test_component_with_params(self):
        """ Check a component can be created with parameters """
        val = PO.ProductionComponent(value=Sentence.build(["testop"]), params=[AcabValue("a"), AcabValue("b")])
        self.assertIsInstance(val, PO.ProductionComponent)
        self.assertEqual(len(val.params), 2)

    def test_component_with_str_params(self):
        """ Check a component can be created with string parameters """
        val = PO.ProductionComponent(value=Sentence.build(["testop"]), params=["a", "b"])
        self.assertIsInstance(val, PO.ProductionComponent)
        self.assertEqual(len(val.params), 2)

    @unittest.skip
    def test_container_init(self):
        pass

    @unittest.skip
    def test_structure_init(self):
        pass

    def test_component_op(self):
        """ Test a components operator can be recovered """
        val = PO.ProductionComponent(value=Sentence.build(["testop"]))
        self.assertEqual(val.op, Sentence.build(["testop"]))


    def test_apply_parameters(self):
        """ Test a component can have parameters applied to it, creating a new component """
        val = PO.ProductionComponent(value=Sentence.build(["testop"]))
        self.assertEqual(len(val.params), 0)
        copied = val.apply_params(["a","test"])
        self.assertNotEqual(val, copied)
        self.assertEqual(len(val.params), 0)
        self.assertEqual(len(copied.params), 2)
        self.assertNotEqual(val.uuid, copied.uuid)


    def test_component_to_sentences(self):
        comp = PO.ProductionComponent(value=Sentence.build(["Test", "Path", "Op"]),
                                      params=[AcabValue.safe_make("x"),
                                              AcabValue.safe_make("y")],
                                              rebind=AcabValue.safe_make("blah"))

        as_sen = comp.to_sentences()
        self.assertEqual(as_sen, "ProductionComponent")
        self.assertIn('Operator', as_sen)
        self.assertEqual(as_sen['Operator'], "_:Test.Path.Op")
        self.assertIn('Params', as_sen)
        self.assertEqual(as_sen['Params'], "_:x.y")
        self.assertIn('Rebind', as_sen)

    def test_component_to_sentences_no_params(self):
        comp = PO.ProductionComponent(value=Sentence.build(["Test.Op.Path"]))

        as_sen = comp.to_sentences()
        self.assertEqual(as_sen, "ProductionComponent")
        self.assertEqual(as_sen[0], "Operator")
        self.assertEqual(as_sen['Operator'], "_:Test.Op.Path")

    def test_component_from_sentences(self):
        comp = PO.ProductionComponent(value=Sentence.build(["Test", "Path", "Op"]),
                                      params=[AcabValue.safe_make("x"),
                                              AcabValue.safe_make("y")],
                                              rebind=AcabValue.safe_make("blah"))

        as_sen = comp.to_sentences()
        comp2 = PO.ProductionComponent.from_sentences([as_sen])[0]
        self.assertEqual(comp.value, comp2.value)
        self.assertEqual(comp.params, comp2.params)
        self.assertEqual(comp.rebind, comp2.rebind)

    def test_multi_component_from_sentences(self):
        comp = PO.ProductionComponent(value=Sentence.build(["Test", "Path", "Op"]),
                                      params=[AcabValue.safe_make("x"),
                                              AcabValue.safe_make("y")],
                                              rebind=AcabValue.safe_make("blah"))

        as_sen = comp.to_sentences()
        as_sen2 = comp.to_sentences()

        comp2 = PO.ProductionComponent.from_sentences([as_sen, as_sen2])
        self.assertEqual(len(comp2), 2)
        self.assertEqual(comp.value, comp2[1].value)
        self.assertEqual(comp.params, comp2[1].params)
        self.assertEqual(comp.rebind, comp2[1].rebind)

    def test_container_to_sentences(self):
        pass
