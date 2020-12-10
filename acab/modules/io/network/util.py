from acab.abstract.config.config import AcabConfig

config = AcabConfig.Get()

DEFAULT_PORT       = config("Module.Network", "DEFAULT_PORT")
DEFAULT_BLOCKSIZE  = config("Module.Network", "DEFAULT_BLOCKSIZE")
DEFAULT_HEADERSIZE = config("Module.Network", "DEFAULT_HEADERSIZE")
DEFAULT_BACKLOG    = config("Module.Network", "DEFAULT_BACKLOG")
DEFAULT_HOST       = config("Module.Network", "DEFAULT_HOST")
