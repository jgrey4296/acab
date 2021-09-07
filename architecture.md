# ACAB Architecture

Acab divides neatly into Parsers, Semantics, and Printers.
Parsers provide an extensible way to convert text into a form usable by Acab.
Printers do the inverse.
Semantics take internal representations and transform them.

## Top Level Module
Splits into abstract, error, and modules.

Abstract covers interfaces and basic components Acab uses.
Errors are the types of errors and exceptions Acab can raise.
Modules covers implementations you can actually use.

## Abstract.interfaces

### Context
Describes the means of managing and testing query results

### Value
Describes the basic value wrapper and Sentence wrapper used throughout Acab.

### Data
Describes the Node type and the Structure it is used in.

### DSL
Describes how fragments of DSLs get combined into a single parser DSL

### Engine
Describes the highest level access to Acab.

### Module Loader
Describes how Acab modules are loaded for use in an Engine.

### Handler System
Describes the core transition system and components.

#### Printing
Describes specific extensions of the transition system form, for turning internal data
into readable strings.

#### Semantic
Describes specific extensions of the transtion system form, for internal processing of 
values and data.


## Abstract.config
An extension of pythons built in ConfigParser, 
to enable declarative use of config values.

## Abstract.core
Implementations of the data and value interfaces,
and specific value types for values as operators and rules.

## Abstract.parsing
Implementations and helpers for parsing.

## Abstract.printing
Utility functions and defaults for printing.
