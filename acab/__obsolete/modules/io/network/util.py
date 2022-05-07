from acab.core.config.config import AcabConfig

config = AcabConfig()

DEFAULT_PORT       = config.prepare("Module.Network", "DEFAULT_PORT")()
DEFAULT_BLOCKSIZE  = config.prepare("Module.Network", "DEFAULT_BLOCKSIZE")()
DEFAULT_HEADERSIZE = config.prepare("Module.Network", "DEFAULT_HEADERSIZE")()
DEFAULT_BACKLOG    = config.prepare("Module.Network", "DEFAULT_BACKLOG")()
DEFAULT_HOST       = config.prepare("Module.Network", "DEFAULT_HOST")()
