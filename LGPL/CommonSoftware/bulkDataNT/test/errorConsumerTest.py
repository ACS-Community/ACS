#!/usr/bin/env python
from time import sleep
from sys  import argv
from Acspy.Clients.SimpleClient import PySimpleClient
from Acspy.Nc.Consumer          import Consumer
import bulkdata

def dataHandler(someParam):
    print "----------------------------"
    print someParam.flow
    print someParam.status
    print someParam.timestamp
    print "----------------------------"

    return
#------------------------------------------------------------------------------
if __name__ == "__main__":

    g = Consumer(bulkdata.CHANNELNAME_ERR_PROP)
    g.addSubscription(bulkdata.errorStatusBlock, handler_function=dataHandler)
    g.consumerReady()
    #After five events have been received, disconnect from the channel
    print "Waiting for events . . ."
    doit = True
    while(doit):
        try:
            sleep(0.1)
        except KeyboardInterrupt:
            doit = False
    g.disconnect()
