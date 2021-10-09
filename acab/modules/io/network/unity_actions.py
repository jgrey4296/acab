import logging as root_logger

from acab.core.data.production_abstractions import ProductionOperator

from .unity_server import UnityServer
from . import util

logging = root_logger.getLogger(__name__)

class IOSend(ProductionOperator):

    def __call__(self, *params, data=None, engine=None):
        # First param = UnityServer Value

        # create message

        # Add message to server queue

        return


class IOFlush(ProductionOperator):

    def __call__(self, *params, data=None, engine=None):
        # flush the server's messages out
        return


class IOListen(ProductionOperator):

    def __call__(self, *params, data=None, engine=None):
        # Listen then assert responses

        return


class IOClose(ProductionOperator):

    def __call__(self, *params, data=None, engine):
        # Close the server
        return


class IOBuildServer(ProductionOperator):

    def __call__(self, *params, data=None, engine=None):
        # Create a UnityServer ready to assert
        return
