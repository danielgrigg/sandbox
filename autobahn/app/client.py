import sys

from twisted.internet import reactor
from twisted.python import log

from autobahn.twisted.websocket import WebSocketClientFactory, \
                                       WebSocketClientProtocol, \
                                       connectWS


class EchoClientProtocol(WebSocketClientProtocol):

   def onConnect(self, response):
      print(response)

   def sendHello(self):
      self.sendMessage("Hello, world!".encode('utf8'))

   def onOpen(self):
      self.sendHello()

   def onMessage(self, payload, isBinary):
      if not isBinary:
         print("Text message received: {}".format(payload.decode('utf8')))
   #   reactor.callLater(1, self.sendHello)


if __name__ == '__main__':

   if len(sys.argv) < 2:
      print("Need the WebSocket server address, i.e. ws://localhost:9000")
      sys.exit(1)

   if len(sys.argv) > 2 and sys.argv[2] == 'debug':
      log.startLogging(sys.stdout)
      debug = True
   else:
      debug = False

   headers = {'Authorization': 'Bearer 583de85a-a937-408b-8752-5bd6489834c0'}

   factory = WebSocketClientFactory(sys.argv[1],
                                    headers = headers,
                                    debug = debug,
                                    debugCodePaths = debug)

   factory.protocol = EchoClientProtocol
   connectWS(factory)

   reactor.run()
