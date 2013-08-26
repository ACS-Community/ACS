#!/usr/bin/env python

from Acspy.Nc.CommonNC        import CommonNC
from Acspy.Nc.Supplier        import Supplier
import datacapEx
from datacapEx import ExecBlockProcessedEvent, DataCapturerId, ExecBlockStartedEvent, ScanStartedEvent
import asdmEX

s = Supplier('pyTest-NC')

name = 'DATACAP1'
s.publishEvent(name)

sessionId = asdmEX.IDLEntityRef('SessionId','X1','SID','1.0')
sb = asdmEX.IDLEntityRef('SB1','X1','SB1','1.0')
dcId = DataCapturerId (name, 'arrayId', sessionId, sb)

execBlockId = asdmEX.IDLEntityRef('ExecBlockId','X1','SB1','1.0')
d = ExecBlockProcessedEvent( dcId, 'statu', execBlockId, 0)
s.publishEvent(d)

execId = asdmEX.IDLEntityRef('4','3','2', '1')
execBlockId = asdmEX.IDLEntityRef('1','2','3','4')
sse = ScanStartedEvent(execId, "something", 4, [datacapEx.LAST, datacapEx.LAST],0)
s.publishEvent(sse)

execId = "23"
execBlockEntityRef = asdmEX.IDLEntityRef(execId,"X00000000","0","0")
sbId   = asdmEX.IDLEntityRef(execId,"X00000000","0","0")
arrayId = "1"
time = 100
startExecBlock = datacapEx.ExecBlockStartedEvent(execBlockEntityRef,sbId,sessionId,arrayId,time)
s.publishEvent(startExecBlock)
    
endExecBlock   = datacapEx.ExecBlockEndedEvent(execBlockEntityRef,sbId,sessionId,arrayId,datacapEx.SUCCESS,time+10)
s.publishEvent(endExecBlock)

print "All structures successfully sent!!"
s.destroyNotificationChannel()






