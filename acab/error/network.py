from .base import AcabBasicException

@dataclass(repr=False)
class AcabNetworkException(AcabBasicException):
    """ The base exception for network errors """
    pass
