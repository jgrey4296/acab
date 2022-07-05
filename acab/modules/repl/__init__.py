"""
The ACAB REPL for command line use.

repl.py : The file to run. Is a basic loop.

ReplParser.py : The parser which either recognises commands,
or passes commands through to the loaded engine.

repl_commands.py : The location for individual commands.
Uses the IR from the parser to trigger a simple function which has been
registered to repl_commands.repl_commands   .

Repl commands pair with a ReplE enum.
They take the active Engine, and some data (as a dict),
and return a new, updated or modified engine,
and any response data (as the input data dict, modified).

"""
