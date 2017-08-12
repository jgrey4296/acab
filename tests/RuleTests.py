import unittest
import logging
from test_context import pyRule
import pyRule.wme as W
import pyRule.Comparisons as C
class TestRules(unittest.TestCase):
      
    def setUp(self):
        self.fb = W.FactBase()
        
    
    def tearDown(self):
        self.fb = None

    def test_initial(self):
        self.assertIsNotNone(self.fb)
        self.assertIsInstance(self.fb, W.FactBase)

    def test_wme_creation(self):
        """ Check a wme is created in the simplest case """
        data = {"a": 2, "b": "blah"}
        wme = W.WME(data)
        self.assertTrue(wme._data["a"] == 2)
        self.assertTrue(wme._data["b"] == "blah")
        data["a"] = 5
        self.assertEqual(wme._data["a"], 2)
        self.assertEqual(wme._assertTime, 0)
        self.assertIsNotNone(wme._hash)
        
    def test_assert(self):
        """ Ensure wmes can be added to the factbase """
        data = {"a": 2, "b":3}
        wme = self.fb.assertWME(data)[0]
        self.assertTrue(len(self.fb) == 1)
        self.assertIsInstance(wme,W.WME)

    def test_assert_fail(self):
        """ Can't Assert a wme twice """
        data = W.WME({"a": 2, "b": 3})
        wme = self.fb.assertWME(data)
        with self.assertRaises(Exception):
            self.fb.assertWME(data)
        
    def test_retract(self):
        """ Ensure wmes can be removed from factbase """
        data = {"a": 2, "b":3}
        wme = self.fb.assertWME(data)[0]
        self.assertTrue(len(self.fb) == 1)
        self.fb.retractWME(wme)
        self.assertTrue(len(self.fb) == 0)

    def test_retract_fail(self):
        """ Ensure complaint if retracting a bad type """
        data = {"a": 2, "b":3}
        with self.assertRaises(Exception):
            self.fb.retractWME(data)

    def test_retract_twice_fail(self):
        """ ensure quiet failure on double retraction """ 
        data = {"a": 2}
        wme = self.fb.assertWME(data)[0]
        self.fb.retractWME(wme)
        response = self.fb.retractWME(wme)
        self.assertEqual(response, 0)
            
    def test_query(self):
        """ initial query test, one clause, one wme """
        data = {"a": 2, "b":3}
        self.fb.assertWME(data)
        #Clauses are tuples
        query = W.Query(("a",C.EQ,2))
        response = self.fb.query(query)
        self.assertIsInstance(response, W.Contexts)
        self.assertTrue(response)

        
    def test_query_fail(self):
        """ ensure query fails in the simplest case: no matching wme """
        data = {"a": 2, "b":3}
        self.fb.assertWME(data)
        #Clauses are tuples
        query = W.Query(("a",C.EQ,5))
        response = self.fb.query(query)
        self.assertIsInstance(response, W.Contexts)
        self.assertFalse(response)

    def test_query_fail2(self):
        """ Ensure queries don't just look at the first field """
        data = {"a": 5, "b":3}
        self.fb.assertWME(data)
        #Clauses are tuples
        query = W.Query(("b",C.EQ,5))
        response = self.fb.query(query)
        self.assertIsInstance(response, W.Contexts)
        self.assertFalse(response)

    def test_query_neq(self):
        """ Test not equal alpha test """
        data = {"a": 2, "b": 3}
        self.fb.assertWME(data)
        query = W.Query(("a",C.NEQ, 5))
        response = self.fb.query(query)
        self.assertTrue(response)
        
    def test_query_multi_clause_wme_agnostic(self):
        """ Test a query of multiple clauses that can match on
        a single wme """
        query = W.Query(("a",C.EQ,2), ("b",C.EQ,3))
        data = {"a": 2, "b": 3}
        self.fb.assertWME(data)
        response = self.fb.query(query)
        self.assertTrue(response)

    def test_query_multi_clause_single_wme(self):
        """ Test a query of multiple clauses that must match
        on a single wme """
        query = W.Query([("a",C.EQ,2), ("b",C.EQ,3)])
        data = {"a": 2, "b": 3}
        self.fb.assertWME(data)
        response = self.fb.query(query)
        self.assertTrue(response)

    def test_query_multi_clause_multi_wme(self):
        """ Test a multi clause query that must match on two different
        wmes """
        query = W.Query(("a",C.EQ,2), ("b",C.EQ,3))
        data = {"a": 2}
        data2 = {"b":3}
        self.fb.assertWME(data, data2)
        response = self.fb.query(query)
        self.assertTrue(response)

    def test_query_wme_lacks_field_of_query(self):
        """ Test a query on a wme that lacks the appropriate field """ 
        query = W.Query(("a",C.EQ, 2))
        data = {"b": 2}
        self.fb.assertWME(data)
        response = self.fb.query(query)
        self.assertFalse(response)
        
    def test_query_bind(self):
        """ Test a multi clause binding query """
        query = W.Query([("a","#x"), ("b", C.EQ, 2)],
                      ("c",C.EQ, "#x"))
        self.fb.assertWME({"a":1, "b":2}, {"c":1})
        response = self.fb.query(query)
        self.assertTrue(response)

    def test_query_bind_unequal_fail(self):
        """ Test a bound test can fail """
        query = W.Query([("a","#x"), ("b", C.EQ, 2)],
                      ("c",C.EQ, "#x"))
        self.fb.assertWME({"a":1, "b":2}, {"c":6})
        response = self.fb.query(query)
        self.assertFalse(response)

    def test_query_bind_lacking_fail(self):
        """ Test a bind fail on a wme that lacks the field """
        query = W.Query([("a","#x"), ("b", C.EQ, 2)],
                      ("d",C.EQ, "#x"))
        self.fb.assertWME({"a":1, "b":2}, {"c":6})
        response = self.fb.query(query)
        self.assertFalse(response)

    def test_query_realistic(self):
        """ Test a simple query using more realistic data """
        self.fb.assertWME({"name":"bob"}, {"name": "bill"},
                          {"name": "jill"})
        query = W.Query(("name",C.EQ,"bob"),("name",C.EQ,"bill"))
        response = self.fb.query(query)
        self.assertTrue(response)

    def test_query_realistic(self):
        """ A Second pseudo realistic data query test """
        self.fb.assertWME({"name":"bob"}, {"name": "bill"},
                          {"name": "jill"})
        query = W.Query([("name","#x"),("name",C.EQ,"bob")],
                      [("name","#y"), ("#y",C.NEQ,"#x"),
                       ("name", C.NEQ, "bill")])
        response = self.fb.query(query)
        self.assertTrue(response)
        self.assertEqual(response._alternatives[0][0]["#y"], "jill")

    def test_query_realistic2(self):
        """ A final psuedo-realistic data query test """
        self.fb.assertWME({"name":"bob"}, {"name": "bill"},
                          {"name": "jill"})
        query = W.Query([("name","#x"),("name",C.EQ,"bob")],
                      [("name","#y"), ("#y",C.NEQ,"#x"),
                       ("name", C.NEQ, "#x"),
                      ("name",C.NEQ, "bill")])
        response = self.fb.query(query)
        self.assertTrue(response)
        self.assertEqual(response._alternatives[0][0]["#y"], "jill")

    def test_forall_negation_simple(self):
        """ Test a query that requires a wme to NOT exist """
        query = W.Query(W.Query(("name",C.EQ,"bob"), negated=True))
        self.fb.assertWME({"name":"bill"})
        response = self.fb.query(query)
        self.assertTrue(response)
        self.assertEqual(len(response._alternatives), 1)

    def test_forall_negation_test_compound(self):
        """ Test a multi clause negation """
        query = W.Query(W.Query([("name",C.EQ,"bob"),("age",C.EQ,20)], negated=True))
        self.fb.assertWME({"name" : "bob", "age" : 15})
        response = self.fb.query(query)
        self.assertTrue(response)
        self.assertEqual(len(response._alternatives), 1)

    def test_forall_negation_fail(self):
        """ Test a negation that fails """
        query = W.Query(W.Query(("name",C.EQ,"bob"), negated=True))
        self.fb.assertWME({"name":"bob"})
        response = self.fb.query(query)
        self.assertFalse(response)
    
    def test_forall_multi_clause_negation_fail(self):
        """ Test a multi clause negation that fails """
        query = W.Query(W.Query([("name",C.EQ,"bob"),("age",C.EQ,20)], negated=True))
        self.fb.assertWME({"name":"bob","age":20})
        response = self.fb.query(query)
        self.assertFalse(response)
    
    

if __name__ == "__main__":
      LOGLEVEL = logging.DEBUG
      logFileName = ".log"
      logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
      console = logging.StreamHandler()
      console.setLevel(logging.INFO)
      logging.getLogger('').addHandler(console)
      unittest.main()
