"""
Module for connecting to Unity using network
"""

import logging as root_logger
import socket
import json
from enum import Enum
from json.decoder import JSONDecodeError
from hashlib import md5

from pyRule.trie import TrieEngine

logging = root_logger.getLogger(__name__)
####################
DEFAULT_PORT = 50000
DEFAULT_BLOCKSIZE = 1024
DEFAULT_HEADERSIZE = 128
DEFAULT_BACKLOG = 10
DEFAULT_HOST = "localhost"

class UnityServer:
    """ A Server to connect to unity """

    #The types of messages supported
    MESSAGE_T = Enum("Message Types", "HANDSHAKE INFO ACTION AI_GO AI_COMPLETE QUIT PAYLOAD RESEND", start=0)
    MESSAGE_T_LOOKUP = {x.value : x for x in MESSAGE_T}

    def __init__(self,
                 port=DEFAULT_PORT,
                 host=DEFAULT_HOST,
                 backlog=DEFAULT_BACKLOG,
                 blockSize=DEFAULT_BLOCKSIZE,
                 headerSize=DEFAULT_HEADERSIZE,
                 linked_engine=None):
        self.theSocket  = None
        self.host = host
        self.port = port
        self.backlog = backlog
        #Static size of all headers, padded with '!'s at end
        self.headerSize = headerSize
        #the amount of data to read at a time for payloads
        self.blockSize = blockSize

        #queues of messages to process
        self.fromClientMessages = []
        self.toClientMessages = []
        #Unfinished segments of data
        self.data_segments = ""

        self.accepted_client = None
        self.listen_for_clients = True
        self.listen_for_data = True

        #The AI engine that the network traffic drives
        self.linked_engine = linked_engine

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
        self.theSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.theSocket.bind((self.host, self.port))
        self.theSocket.listen(self.backlog)

    def close(self):
        logging.info("Closing Socket")
        if self.accepted_client is not None:
            self.accepted_client.close()
        if self.theSocket is not None:
            self.theSocket.close()
        self.listen_for_clients = False
        self.listen_for_data = False


    def listen_for_headers(self):
        logging.info("Listening")
        if self.theSocket is None:
            raise Exception("Socket is not setup")
        try:
            while self.listen_for_clients:
                #Loop for a connection
                client, address = self.theSocket.accept()
                self.accepted_client = client
                self.listen_for_data = True
                logging.info("Client connected on: {}.".format(str(address)))
                while self.listen_for_data:
                    #loop to actually recieve data
                    logging.info("--------------------")
                    logging.info('Waiting for data')
                    #RECEIVE HEADER:
                    header = ""
                    while len(header) < self.headerSize:
                        #loop until enough data for a header has been received
                        header += self.accepted_client.recv(self.headerSize).decode()
                    logging.info('Header: {}'.format(str(header)))
                    #Consume the header, either do something,
                    #or consume the payload afterwards
                    self.consume_header(header)
                    #After consuming the received data, send out any response messages,
                    #packed into a single string
                    self.flush_response()

            #Finished looping with this client:
            logging.info("Closing Client")
            client.close()
        finally:
            #finished listening entirely
            logging.info("Finishing Listening")
            self.close()

    def consume_header(self, data):
        """ Consume a header, in prep for more data possibly """
        assert(len(data) == self.headerSize);
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
            #Start an AI tick here
            self.toClientMessages = self.linked_engine.tick(self.fromClientMessages)
            #todo: then send the queued messages
            for m in self.toClientMessages:
                self.respond(self.NET_ACTION(m))
            #then send AI_COMPLETE header
            self.respond(self.NET_HEADER(UnityServer.MESSAGE_T.AI_COMPLETE))
            self.fromClientMessages = []
            self.toClientMessages = []
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
            listenAmount = min(self.blockSize, amount_to_listen_for - len(received))
            received += self.accepted_client.recv(listenAmount).decode()
        md5_from_data = self.getMD5(received)
        assert(md5_from_header == md5_from_data)

        self.fromClientMessages.append(received)
        logging.info("Payload was: {}".format(received))

    def respond(self, data):
        """ Adds data to the queue to send """
        self.toClientMessages.append(data)

    def flush_response(self):
        """ Actually sends all queued messages out as a single string """
        logging.info("Flushing Response")
        if not bool(self.toClientMessages):
            return

        packed = bytearray("|".join(self.toClientMessages) + "\n", "utf-8")
        self.accepted_client.sendall(packed)
        self.toClientMessages.clear()
        logging.info("Sent data")

    def getMD5(self, s):
        """ Craete the md5 hash of a string for error checking """
        obj = md5(s.encode())
        return obj.hexdigest().upper()


if __name__ == "__main__":
    # Setup root_logger:
    LOGLEVEL = root_logger.DEBUG
    LOG_FILE_NAME = "log.Network"
    root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

    console = root_logger.StreamHandler()
    console.setLevel(root_logger.INFO)
    root_logger.getLogger('').addHandler(console)
    logging = root_logger.getLogger(__name__)
    ##############################
    logging.info("Setting up Server")
    #TODO: find all .trie files and pass them in
    baseTries = []
    engine = TrieEngine(path=baseTries)
    sev = UnityServer(linked_engine=engine)
    try:
        sev.setup()
        sev.listen_for_headers()
    except (KeyboardInterrupt):
        logging.info("Shutting down")
        sev.close()
