"""
Structure Verification in Config Files

"""

# TODO check primitives?
# verify Parse.Structure -> Symbols+Aliases unify


# 1) Enforce Groups:
required_config_grps =["DEFAULT",
                       "SEMANTICS",
                       "MODAL",
                       "Data",
                       "Type.Primitive",
                       "Value.Structure",
                       "Structure.Components",
                       "Symbols",
                       "Aliases",
                       "Modal.Symbols"]


# 2) any MODAL must have:
required_modal_grps = ["Modal.{name}",
                       "Modal.{name}.Symbols",
                       "Modal.{name}.ENUM_VALUES"]
