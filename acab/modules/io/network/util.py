from acab.abstract.config.config import AcabConfig

util = AcabConfig.Get()

DEFAULT_PORT       = util("Module.Network", "DEFAULT_PORT")
DEFAULT_BLOCKSIZE  = util("Module.Network", "DEFAULT_BLOCKSIZE")
DEFAULT_HEADERSIZE = util("Module.Network", "DEFAULT_HEADERSIZE")
DEFAULT_BACKLOG    = util("Module.Network", "DEFAULT_BACKLOG")
DEFAULT_HOST       = util("Module.Network", "DEFAULT_HOST")
