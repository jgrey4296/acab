# Python Based Rule Engine
Based on a trie/Exclusion logic base
    
# DSL
The rule engine uses an exclusion logic DSL, allowing querying, binding, assertion, retraction, and printing.
Custom actions can be registered in the form of: function(engine, params)
    
```
    .a.test.rule:
        .a."string test"?
        .a.simple.test?,
        .an!exclusive.test?,
        .a.bound.$test?,
        .a.comparison.$test(>20)?,
        ~.a.negative.test?
            
        $test + 20 -> $rebound,
        $rebound * 2 -> $finalBind
        $rebound + 20
            
        +(.an.assertion)
        +(.a.bound.assertion.$finalBind)
        +(.bound.values.remain.$rebound)
        -(.a.retraction)
            
    end
```

