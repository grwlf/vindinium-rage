import glob
import sys

from thrift.transport import TSocket
from thrift.transport import TTransport
from thrift.protocol import TBinaryProtocol
from thrift.server import TServer

from tf import TF


class TF_Server:
  def __init__(self):
    pass

  def step(self, arg):
    print('step')
    return 0.0


def run():
  handler = TF_Server()
  processor = TF.Processor(handler)
  transport = TSocket.TServerSocket(host='127.0.0.1', port=9090)
  tfactory = TTransport.TBufferedTransportFactory()
  pfactory = TBinaryProtocol.TBinaryProtocolFactory()

  server = TServer.TSimpleServer(processor, transport, tfactory, pfactory)
  print ('Running the server')
  server.serve()


if __name__ == '__main__':
  run()
