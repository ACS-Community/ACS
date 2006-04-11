#--REGULAR IMPORTS-------------------------------------------------------------
from random  import choice
#--CORBA STUBS-----------------------------------------------------------------
import ACS
reload(ACS)
#--ACS Imports-----------------------------------------------------------------
from ACSImpl.DevIO import DevIO
#--Globals---------------------------------------------------------------------
almaStates =    [ACS.SUBSYSSTATE_AVAILABLE,
                 ACS.SUBSYSSTATE_OFFLINE,
                 ACS.SUBSYSSTATE_ONLINE,
                 ACS.SUBSYSSTATE_OPERATIONAL,
                 ACS.SUBSYSSTATE_ERROR]

offlineBeginStates = [ACS.SUBSYSSTATE_SHUTDOWN,
                      ACS.SUBSYSSTATE_INITIALIZING_PASS1,
                      ACS.SUBSYSSTATE_PREINITIALIZED,
                      ACS.SUBSYSSTATE_INITIALIZING_PASS2,
                      ACS.SUBSYSSTATE_REINITIALIZING]

offlineFinishStates = [ACS.SUBSYSSTATE_SHUTTINGDOWN_PASS1,
                       ACS.SUBSYSSTATE_PRESHUTDOWN,
                       ACS.SUBSYSSTATE_SHUTTINGDOWN_PASS2]

offlineStates = offlineBeginStates + offlineFinishStates

modeStates =    [ACS.SUBSYSMODE_SIMULATION,
                 ACS.SUBSYSMODE_STANDALONE,
                 ACS.SUBSYSMODE_DEGRADED]

#------------------------------------------------------------------------------
class MasterCompDevIO(DevIO):
    '''
    '''
    #--------------------------------------------------------------------------
    def __init__(self, initVal=[ACS.SUBSYSSTATE_OFFLINE, ACS.SUBSYSSTATE_SHUTDOWN]):
        '''
        '''
        DevIO.__init__(self, initVal)
        return
    #--------------------------------------------------------------------------
    def read(self):
        '''
        '''
        retVal = []
        retVal.append(choice(almaStates))
        
        if retVal[0]==ACS.SUBSYSSTATE_OFFLINE:
            #special case
            retVal.append(choice(offlineStates))

        retVal.append(ACS.SUBSYSMODE_SIMULATION)
        
        return retVal
    #--------------------------------------------------------------------------
if __name__=="__main__":
    joe = MasterCompDevIO()
    for i in range(0,20):
        print joe.read()
