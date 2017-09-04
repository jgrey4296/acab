class WME:
      """ A Fact token """

      def __init__(self, data={}, assertTime=0):
          self._assertTime = assertTime
          self._data = data.copy()
          self._hash = hash(str(self._data) + str(self._assertTime))

      def __repr__(self):
            return repr(self._data)
          
      def __hash__(self):
          return self._hash

