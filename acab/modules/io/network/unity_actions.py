import logging as root_logger

from acab.abstract.rule.transform import TransformOp
from acab.abstract.rule.action import ActionOp

from .unity_server import UnityServer
from . import util

logging = root_logger.getLogger(__name__)

class IOSend(ActionOp):

    def __call__(self, *params, data=None, engine=None):
        # First param = UnityServer Value

        # create message

        # Add message to server queue

        return


class IOFlush(ActionOp):

    def __call__(self, *params, data=None, engine=None):
        # flush the server's messages out
        return


class IOListen(ActionOp):

    def __call__(self, *params, data=None, engine=None):
        # Listen then assert responses

        return


class IOClose(ActionOp):

    def __call__(self, *params, data=None, engine):
        # Close the server
        return


class IOBuildServer(TransformOp):

    def __call__(self, *params, data=None, engine=None):
        # Create a UnityServer ready to assert
        return
