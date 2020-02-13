"""
Engine to load, run, and verify a Pipeline

"""


class PipelineEngine:
    """  """

    def __init__(self):
        # The knowledge base to work with
        self._kb = None
        # Pipeline processing layers:
        # Likely of (ruleset, assessment_strategy, result_strategy)
        self._layers = []

        # Verification data:
        self._dialects = {}
        self._io = None
        self._field_interface = None
        self._crosscut_specs = None
        self._time_specs = None
        self._data_flow_specs = None
        self._channel_specs = None

    def __str__(self):
        return ""

    def __repr__(self):
        return ""

    def verify(self):
        # Verify the Pipeline is consistent wrt its spec
        return False

    def tick(self):
        return None

    def load_file(self, filename):
        return None

    def load_imports(self, import_string):
        return None

    def load_dialect(self, dialect_string):
        return None

    def load_types(self, type_string):
        return None

    def load_IO_interface(self, interface_string):
        return None

    def load_field_interface(self, field_interface_string):
        return None

    def load_rule_spec(self, rule_spec_string):
        return None

    def load_crosscut_spec(self, crosscut_spec_string):
        return None

    def load_carrier_spec(self, carrier_spec_string):
        return None

    def load_time_spec(self, time_spec_string):
        return None

    def load_data_flow_spec(self, data_flow_spec_string):
        return None

    def load_channel_spec(self, channel_spec_string):
        return None
