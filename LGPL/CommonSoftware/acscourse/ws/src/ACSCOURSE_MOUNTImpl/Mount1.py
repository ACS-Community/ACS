#--CORBA STUBS-----------------------------------------------------------------
import ACSCOURSE_MOUNT__POA
#--ACS Imports-----------------------------------------------------------------
from Acspy.Servants.ContainerServices  import ContainerServices
from Acspy.Servants.ComponentLifecycle import ComponentLifecycle
from Acspy.Servants.ACSComponent       import ACSComponent
#--ACS Error System Imports----------------------------------------------------
import ACSErrTypeACSCourseImpl

class Mount1(ACSCOURSE_MOUNT__POA.Mount1,  #CORBA stubs for IDL interface
             ACSComponent,  #Base IDL interface
             ContainerServices,  #Developer niceties
             ComponentLifecycle):  #HLA stuff
    '''
    Simple component implementation provided as a reference for developers.
    '''
    def __init__(self):
        '''
        Just call superclass constructors here.
        '''
        ACSComponent.__init__(self)
        ContainerServices.__init__(self)
        return
    #------------------------------------------------------------------------------
    #--Implementation of IDL methods-----------------------------------------------
    #------------------------------------------------------------------------------
    def objfix(self, az, el):
        '''
        Python implementation of IDL method.
        '''
	if el<=90:
            self.getLogger().logInfo("objfix called with az="+str(az)+" and el="+str(el))
	else:
            self.getLogger().logCritical("Wrong value for el "+str(el))
            raise ACSErrTypeACSCourseImpl.TargetOutOfRangeExImpl()
            
#------------------------------------------------------------------------------
#--Main defined only for generic testing---------------------------------------
#------------------------------------------------------------------------------
if __name__ == "__main__":
    import ACSErrTypeACSCourse
    print "Creating an object"
    g = Mount1()
    try:
        g.objfix(10,90)
    except ACSErrTypeACSCourse.TargetOutOfRangeEx, e:
        h = ACSErrTypeACSCourseImpl.TargetOutOfRangeExImpl(exception=e, create=0)
        h.Print()
    print "Done..."

