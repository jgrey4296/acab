import unittest
import logging
from test_context import py_rule
from py_rule.data_structures.time.arc import Arc
from py_rule.data_structures.time.event import Event
from py_rule.data_structures.time.pattern import Pattern, PatternSeq
from py_rule.data_structures.time.parsing import parser as tp
from py_rule.data_structures.time.utils import Time as t
import IPython

class TestTime(unittest.TestCase):

    def setUp(self):
        return 1

    def tearDown(self):
        return 1

    #----------
    # ARC TESTS
    def test_arc_creation(self):
        an_arc = Arc(t(0,1), t(1,1))
        self.assertIsNotNone(an_arc)

    def test_arc_contains_true(self):
        an_arc = Arc(t(0,1), t(1,1))
        self.assertTrue(t(1,4) in an_arc)
        self.assertTrue(t(3,4) in an_arc)

    def test_arc_contains_false(self):
        an_arc = Arc(t(0,1), t(1,2))
        self.assertFalse(t(3,4) in an_arc)

    #get the size from the arc
    def test_arc_size(self):
        an_arc = Arc(t(1,4), t(1,2))
        self.assertEqual(an_arc.size(), t(1,4))

    def test_arc_equality(self):
        an_arc = Arc(t(1,4), t(2,3))
        an_arc_2 = Arc(t(1,4), t(2,3))
        self.assertEqual(an_arc, an_arc_2)

    def test_arc_equality_failure(self):
        an_arc = Arc(t(1,4), t(2,3))
        an_arc_2 = Arc(t(2,4), t(2,3))
        self.assertNotEqual(an_arc, an_arc_2)

    def test_arc_bound(self):
        an_arc = Arc(t(1,4), t(1,2))
        another_arc = Arc(t(1,3), t(2,3))
        combined = an_arc.bound(another_arc)
        combined_other = another_arc.bound(an_arc)
        self.assertEqual(combined, combined_other)
        self.assertEqual(combined.start, t(1,4))
        self.assertEqual(combined.end, t(2,3))


    #--------------------
    # EVENT TESTS
    def test_event_creation(self):
        anEvent = Event(Arc(t(0,1), t(1,1)), "a")
        self.assertIsNotNone(anEvent)

    #call an event
    def test_event_call(self):
        anEvent = Event(Arc(t(0,1), t(1,1)), "a")
        callResult = anEvent(t(1,2))
        self.assertEqual(len(callResult), 1)

    def test_event_call_outside_range(self):
        anEvent = Event(Arc(t(0,1), t(1,1)), "a")
        callResult = anEvent(t(2,1))
        self.assertEqual(len(callResult), 0)

    #call an event that holds a pattern
    def test_event_call_pattern(self):
        aPattern = Pattern(Arc(t(0,1),t(1,1)),
                           [ Event(Arc(t(0,1),t(1,2)), "a"),
                             Event(Arc(t(1,2),t(1,1)), "b"),
                           ])
        anEvent = Event(Arc(t(0,1), t(1,1)), aPattern, True)
        callResult = anEvent(t(1,2))
        self.assertEqual(len(callResult), 1)
        self.assertEqual(callResult[0].values, "b")

    #get the base set
    def test_event_base(self):
        anEvent = Event(Arc(t(0,1),t(1,1)), "a")
        base = anEvent.base()
        self.assertEqual(len(base), 2)

    def test_event_base_pattern(self):
        aPattern = Pattern(Arc(t(0,1),t(1,1)),
                                [ Event(Arc(t(1,4),t(1,2)), "a"),
                                  Event(Arc(t(1,6),t(3,8)), "b") ])
        anEvent = Event(Arc(t(0,1),t(1,1)), aPattern, True)
        base = anEvent.base()
        self.assertEqual(len(base), 6)

    #get the key
    def test_event_get_key(self):
        anEvent = Event(Arc(t(0,1),t(1,1)), "a")
        key = anEvent.key()
        self.assertEqual(key, t(0,1))

    #sort by key
    def test_event_sort_by_key(self):
        events = [ Event(Arc(t(1,2),t(1,1)), "a"),
                   Event(Arc(t(0,1),t(1,2)), "b"),
                   Event(Arc(t(2,1),t(3,1)), "c")]
        sorted_events = sorted(events, key=lambda x: x.key())
        values = [x.values for x in sorted_events]
        self.assertEqual(values, ["b","a","c"])

    #check contains
    def test_event_contains(self):
        anEvent = Event(Arc(t(0,1),t(1,2)), "a")
        self.assertTrue(t(1,4) in anEvent)
        self.assertFalse(t(3,4) in anEvent)


    #--------------------
    # PATTERN TESTS
    def test_pattern_creation(self):
        aPattern = Pattern(Arc(t(0,1), t(1,1)), [])
        self.assertIsNotNone(aPattern)

    #call empty
    def test_pattern_call_empty(self):
        aPattern = Pattern(Arc(t(0,1),t(1,1)), [])
        self.assertEqual(len(aPattern(t(0,1))), 0)

    #call with events
    def test_pattern_call(self):
        aPattern = Pattern(Arc(t(0,1), t(1,1)),
                                [ Event(Arc(t(0,1),t(1,2)), "a"),
                                  Event(Arc(t(1,2),t(1,1)), "b")])
        res = aPattern(t(1,2))
        self.assertEqual(len(res), 1)
        self.assertEqual(res[0].values, "b")

    #call with patterns
    def test_pattern_call_with_internal_pattern_start(self):
        aPattern = Pattern(Arc(t(0,1), t(1,2)),
                                [ Event(Arc(t(0,1), t(1,2)), "a"),
                                  Event(Arc(t(1,2),t(1,1)), "b") ])

        mainPattern = Pattern(Arc(t(0,1), t(1,1)),
                                   [ Event(Arc(t(0,1), t(1,1)), "c"),
                                     aPattern ])

        result = mainPattern(t(0,1), True)
        self.assertEqual(len(result), 2)
        self.assertTrue("a" in result)
        self.assertTrue("c" in result)
        self.assertFalse("b" in result)

    def test_pattern_call_with_internal_pattern_scale(self):
        aPattern = Pattern(Arc(t(0,1), t(1,2)),
                                [ Event(Arc(t(0,1), t(1,2)), "a"),
                                  Event(Arc(t(1,2),t(1,1)), "b") ])

        mainPattern = Pattern(Arc(t(0,1), t(1,1)),
                                   [ Event(Arc(t(0,1), t(1,1)), "c"),
                                     aPattern ])

        #checks the internal pattern is scaled appropriately:
        result = mainPattern(t(1,4), True)
        self.assertEqual(len(result), 2)
        self.assertFalse("a" in result)
        self.assertTrue("b" in result)
        self.assertTrue("c" in result)

    def test_pattern_call_with_internal_pattern_end(self):
        aPattern = Pattern(Arc(t(0,1), t(1,2)),
                                [ Event(Arc(t(0,1), t(1,2)), "a"),
                                  Event(Arc(t(1,2),t(1,1)), "b") ])

        mainPattern = Pattern(Arc(t(0,1), t(1,1)),
                                   [ Event(Arc(t(0,1), t(1,1)), "c"),
                                     aPattern ])

        #checks the internal pattern ends appropriately:
        result = mainPattern(t(1,2), True)
        self.assertEqual(len(result), 1)
        self.assertTrue("c" in result)
        self.assertFalse("a" in result)
        self.assertFalse("b" in result)

    #call with patterns in events
    def test_pattern_call_with_patterns_in_events(self):
        aPattern = Pattern(Arc(t(0,1), t(1,1)),
                                [ Event(Arc(t(0,1), t(1,2)), "a"),
                                  Event(Arc(t(1,2),t(1,1)),  "b") ])

        mainPattern = Pattern(Arc(t(0,1), t(1,1)),
                                   [ Event(Arc(t(0,1), t(1,1)), "c"),
                                     Event(Arc(t(1,4), t(3,4)),
                                           aPattern, True) ])

        #checks the internal pattern ends appropriately:
        result = mainPattern(t(0,2), True)
        self.assertTrue("c" in result)
        self.assertFalse("a" in result)
        result2 = mainPattern(t(1,4), True)
        self.assertTrue("c" in result2)
        self.assertTrue("a" in result2)
        result3 = mainPattern(t(1,2), True)
        self.assertTrue("c" in result3)
        self.assertFalse("a" in result3)
        self.assertTrue("b" in result3)
        result4 = mainPattern(t(3,4), True)
        self.assertTrue("c" in result4)
        self.assertFalse("a" in result4)
        self.assertFalse("b" in result4)

    #get the key
    def test_pattern_get_key(self):
        aPattern = Pattern(Arc(t(3,8),t(6,8)),
                           [ Event(Arc(t(0,1),t(1,1)), "a")])

        self.assertEqual(aPattern.key(), t(3,8))

    #check contains
    def test_pattern_contains(self):
        aPattern = Pattern(Arc(t(3,8),t(6,8)),
                                [ Event(Arc(t(0,1),t(1,1)), "a")])

        self.assertTrue(t(4,8) in aPattern)
        self.assertFalse(t(7,8) in aPattern)

    #get the base set
    def test_pattern_denominator_simple(self):
        aPattern = tp.parse_string("[[ a b c ]]")
        denom = aPattern.denominator()
        self.assertEqual(denom, 3)

    def test_pattern_denominator_simple_2(self):
        aPattern = tp.parse_string("[[a b c d]]")
        denom = aPattern.denominator()
        self.assertEqual(denom, 4)

    def test_pattern_denominator_nested(self):
        aPattern = tp.parse_string("[[a b [c d]]]")
        denom = aPattern.denominator()
        self.assertEqual(denom, 6)

    def test_pattern_denominator_parallel(self):
        aPattern = tp.parse_string("[[a b, c d]]")
        denom = aPattern.denominator()
        self.assertEqual(denom, 2)

    #pretty print pattern
    def test_pattern_seq__double_cycle(self):
        p1 = tp.parse_string("[[a b c d]]")
        p2 = tp.parse_string("[[e f g h]]")
        p3 = PatternSeq(Arc(t(0,1),t(2,1)),
                             [ p1,p2 ])

        # IPython.embed(simple_prompt=True)
        self.assertEqual(p3(t(0,1), True)[0], "a")
        # self.assertEqual(p3(t(1,1), True)[0], "e")
        self.assertEqual(p3(t(7,4), True)[0], "h")

    def test_pattern_iterator(self):
        aPattern = tp.parse_string("[[a b [c d] e]]")
        for i,x in zip([0,1,2,3,4], aPattern.iter()):
            self.assertEqual(x[0], ["a","a",
                                    "b", "b",
                                    "c", "d",
                                    "e", "e"][i])

    def test_pattern_iterator_loop(self):
        aPattern = tp.parse_string("[[ a b ]]")
        for i,x in zip(range(8), aPattern.iter()):
            self.assertEqual(x[0], (["a", "b"] * 4)[i])

    def test_pattern_iterator_events(self):
        aPattern = tp.parse_string("[[ a b c ]]")
        for i,x in zip(range(4), aPattern.iter(False)):
            self.assertIsInstance(x[0], Event)

    def test_pattern_add(self):
        first_pattern = tp.parse_string("[[a b]]")
        second_pattern = tp.parse_string("[[c d]]")
        combined = first_pattern + second_pattern
        for i,x in zip([0, 1, 2, 3, 4, 5],
                       combined.iter()):
            self.assertEqual(x[0],
                            "a b c d a b".split(" ")[i])

    def test_pattern_add_twice(self):
        first_pattern = tp.parse_string("[[a b]]")
        second_pattern = tp.parse_string("[[c d]]")
        combined = first_pattern + second_pattern
        combined_2 = combined + second_pattern
        for i,x in zip([0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
                       combined_2.iter()):
            self.assertEqual(x[0],
                            "a b c d c d a b c d".split(" ")[i])

    def test_pattern_stack(self):
        first_pattern = tp.parse_string("[[a b]]")
        second_pattern = tp.parse_string("[[c d]]")
        combined = first_pattern * second_pattern
        for i,x in zip([0, 1, 2, 3],
                       combined.iter()):
            self.assertEqual(x[0],
                             "a b a b".split(" ")[i])
            self.assertEqual(x[1],
                             "c d c d".split(" ")[i])

    def test_pattern_subtract(self):
        return None

    def test_pattern_format(self):
        return None


    #--------------------
    # PARSER TESTS
    #Parse a pattern
    def test_parse_simple(self):
        aPattern = tp.parse_string("[[ a b c ]]")
        self.assertIsInstance(aPattern, Pattern)
        self.assertEqual(len(aPattern.components), 3)

    def test_parse_balance_failure(self):
        with self.assertRaises(Exception):
            tp.parse_string("[a b c")

    def test_parse_balance_failure_nested(self):
        with self.assertRaises(Exception):
            tp.parse_string("[[a b [c d]]")

    #Parse a nested pattern
    def test_parse_nested_simple(self):
        aPattern = tp.parse_string("[[ a b [c d]]]")
        self.assertEqual(len(aPattern.components), 3)
        self.assertIsInstance(aPattern.components[2].values, Pattern)

    def test_parse_parallel_nested(self):
        aPattern = tp.parse_string("[[ a b , [c d] e]]")
        self.assertEqual(len(aPattern.components), 2)
        self.assertEqual(len(aPattern.components[1].values.components), 2)

    #parse a pretty printed pattern

if __name__ == "__main__":
      #use python $filename to use this logging setup
      LOGLEVEL = logging.INFO
      logFileName = "log.test_time"
      logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
      console = logging.StreamHandler()
      console.setLevel(logging.INFO)
      logging.getLogger().addHandler(console)
      unittest.main()
      #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
