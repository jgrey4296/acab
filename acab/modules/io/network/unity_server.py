from enum import Enum
from hashlib import md5
from json.decoder import JSONDecodeError
import json
import logging as root_logger
import socket

from acab.core.data.values import AcabValue

from . import util

class UnityServer(AcabValue):
    """ A Server to connect to unity """

    #The types of messages supported
    MESSAGE_T = Enum("Message Types", "HANDSHAKE INFO ACTION AI_GO AI_COMPLETE QUIT PAYLOAD RESEND", start=0)
    MESSAGE_T_LOOKUP = {x.value : x for x in MESSAGE_T}

    def __init__(self,
                 port=util.DEFAULT_PORT,
                 host=util.DEFAULT_HOST,
                 backlog=util.DEFAULT_BACKLOG,
                 blockSize=util.DEFAULT_BLOCKSIZE,
                 headerSize=util.DEFAULT_HEADERSIZE):

        self._theSocket  = None
        self._host = host
        self._port = port
        self._backlog = backlog
        #Static size of all headers, padded with '!'s at end
        self._headerSize = headerSize
        #the amount of data to read at a time for payloads
        self._blockSize = blockSize

        #queues of messages to process
        self._fromClientMessages = []
        self._toClientMessages = []
        #Unfinished segments of data
        self._data_segments = ""

        self._accepted_client = None
        self._listen_for_clients = True
        self._listen_for_data = True


    def NET_HEADER(self, m_type, payload_size=0, data="", hash=""):
        """ Utility to easily create a header to send over the network """
        assert(m_type in UnityServer.MESSAGE_T)
        if data is not "":
            hash = self.getMD5(data)
        header = { "size" : payload_size, "iden" : m_type.value, "data" : data, "hash" : hash }
        header_str = json.dumps(header)
        return header_str

    def NET_ACTION(self, payload):
        """ Utility to easily create an action command to send over the network """
        payload_length = len(payload)
        header = self.NET_HEADER(UnityServer.MESSAGE_T.ACTION, payload_length, data=payload)
        return header


    #Methods:
    def setup(self):
        logging.info("Setting up Python Server")
        self._theSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self._theSocket.bind((self._host, self._port))
        self._theSocket.listen(self._backlog)

    # TODO: setup_pair
    # setup a pair of sockets for testing basic connection methods

    def close(self):
        logging.info("Closing Socket")
        if self._accepted_client is not None:
            self._accepted_client.close()
        if self._theSocket is not None:
            self._theSocket.close()
        self._listen_for_clients = False
        self._listen_for_data = False


    def listen_for_client(self):
        logging.info("Listening")
        if self._theSocket is None:
            raise Exception("Socket is not setup")

        if self._listen_for_clients:
            #Loop for a connection
            client, address = self._theSocket.accept()
            self._accepted_client = client
            self._listen_for_data = True
            self._listen_for_clients = False
            logging.info("Client connected on: {}.".format(str(address)))

    def listen_for_data:
        logging.info('Waiting for data')
        #RECEIVE HEADER:
        header = ""
        while len(header) < self._headerSize:
            #loop until enough data for a header has been received
            header += self._accepted_client.recv(self._headerSize).decode()
            logging.info('Header: {}'.format(str(header)))
        #Consume the header, either do something,
        #or consume the payload afterwards
        self.consume_header(header)
        #After consuming the received data, send out any response messages,
        #packed into a single string
        self.flush_response()


    def consume_header(self, data):
        """ Consume a header, in prep for more data possibly """
        assert(len(data) == self._headerSize);
        trimmed = data.strip("! ")
        decoded = json.loads(trimmed);
        assert('size' in decoded)
        assert('iden' in decoded)
        assert('hash' in decoded)
        decoded['iden'] = UnityServer.MESSAGE_T_LOOKUP[decoded['iden']]
        logging.info("Received a header, Type: {}".format(decoded['iden']))

        if (decoded['iden'] != UnityServer.MESSAGE_T.INFO):
            self.process_header(decoded)
        else:
            self.process_info(decoded)
        logging.info("Finished consuming header")


    def process_header(self, data):
        """ Given a Header that needs no payload, act upon it """
        iden = data['iden']
        if iden == UnityServer.MESSAGE_T.HANDSHAKE:
            self.respond(self.NET_ACTION("testing blahhhh"))
            self.respond(self.NET_HEADER(UnityServer.MESSAGE_T.HANDSHAKE))
        elif iden == UnityServer.MESSAGE_T.QUIT:
            self.close()
        elif iden == UnityServer.MESSAGE_T.AI_GO:
            logging.info("Starting an AI Tick")
            # TODO change this
            #Start an AI tick here
            # self.toClientMessages = self.linked_engine.tick(self.fromClientMessages)
            #TODO: then send the queued messages
            for m in self._toClientMessages:
                self.respond(self.NET_ACTION(m))
            #then send AI_COMPLETE header
            self.respond(self.NET_HEADER(UnityServer.MESSAGE_T.AI_COMPLETE))
            self._fromClientMessages = []
            self._toClientMessages = []
        else:
            raise Exception("Unrecognised header type received")
        logging.info("Finished processing header")

    def process_info(self, data):
        """ Given an info header, listen for the payload """
        logging.info("Listening for info payload")
        amount_to_listen_for = data['size']
        assert(amount_to_listen_for > 0)
        md5_from_header = data['hash']
        received = ""
        while len(received) < amount_to_listen_for:
            listenAmount = min(self_.blockSize, amount_to_listen_for - len(received))
            received += self._accepted_client.recv(listenAmount).decode()
        md5_from_data = self.getMD5(received)
        assert(md5_from_header == md5_from_data)

        self._fromClientMessages.append(received)
        logging.info("Payload was: {}".format(received))


    def respond(self, data):
        """ Adds data to the queue to send """
        self._toClientMessages.append(data)

    def flush_response(self):
        """ Actually sends all queued messages out as a single string """
        logging.info("Flushing Response")
        if not bool(self._toClientMessages):
            return

        packed = bytearray("|".join(self._toClientMessages) + "\n", "utf-8")
        self._accepted_client.sendall(packed)
        self._toClientMessages.clear()
        logging.info("Sent data")


    def getMD5(self, s):
        """ Craete the md5 hash of a string for error checking """
        obj = md5(s.encode())
        return obj.hexdigest().upper()


    def __call__(self, proposals, engine, **kwargs):
        """
        Fire the agenda: Send the proposals out,
        receive information in, and insert it into the engine
        """
        # Select all proposals that are IOSends
        # Send them out
        # Get messages
        # return them as actions
        return
