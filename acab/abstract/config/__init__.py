#!/usr/bin/env python3
"""
Acab.abstract.config

Configuration *must* be loaded before the rest of acab.
It loads defaults from config files, sets up, and verifies modalities

This ensures that missing values error at startup

*Required* Config Files sections:
[DEFAULT]
[Data]
[Type.Primitive]
[Value.Structure]
[Structure.Components]
[Symbols]
[MODAL]
[Modal.{}.Symbols]
[Module.{}]
[Parse.Structure]
[Parse.Patterns]
[Aliases]
[Print.Data]
[Print.Patterns]

"""
