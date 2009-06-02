package alma.alarmsystem.corbaservice;

import org.omg.CORBA.Any;
import org.omg.CORBA.ORB;

import alma.acs.container.AdvancedContainerServices;
import alma.acs.logging.AcsLogger;

import si.ijs.maci.AdministratorOperations;

import alma.JavaContainerError.wrappers.AcsJContainerEx;

public class AlarmSystemAdvancedContainerServices implements AdvancedContainerServices {
	
	/**
	 * The logger
	 */
	private final AcsLogger logger;
	
	private final AlarmSystemContainerServices alSysContSvcs;
	
	public AlarmSystemAdvancedContainerServices(AlarmSystemContainerServices alSysContSvcs) {
		if (alSysContSvcs==null) {
			throw new IllegalArgumentException("The alarm system container services can't be null");
		}
		this.alSysContSvcs=alSysContSvcs;
		logger=alSysContSvcs.getLogger();
		if (logger==null) {
			throw new IllegalStateException("The logger can't be null");
		}
	}
	
	@Override
	public org.omg.CORBA.Object corbaObjectFromString(String strObjRef) {
		ORB orb = alSysContSvcs.getOrb();
		org.omg.CORBA.Object objRef = orb.string_to_object(strObjRef);
		logger.finer("converted corba object reference string " + strObjRef + 
						" back to a corba object reference.");
		return objRef;
	}
	
	@Override
	public String corbaObjectToString(org.omg.CORBA.Object objRef)
	{
		ORB orb = getORB(); 
		String str = orb.object_to_string(objRef);
		logger.finer("converted corba object reference of type " + objRef.getClass().getName() +
						" to the string " + str);
		return str;
	}

	@Override
	public Any getAny() {
		ORB orb = getORB();
    	org.omg.CORBA.Any any = orb.create_any();
    	if (any == null) {
    		// should never happen, but we check just in case, 
    		// since there is a difficult to verify NPE when Any is created by MC for sending alarms.
    		String msg = "Failed to create org.omg.CORBA.Any";
    		logger.warning(msg);
    		throw new NullPointerException(msg);
    	}
    	return any;
	}

	@Override
	public ORB getORB() {
		return alSysContSvcs.getOrb();
	}
	
	@Override
	public void disconnectManagerAdmin(AdministratorOperations adminOp) {
	}
	
	public void connectManagerAdmin(AdministratorOperations adminOp, boolean retryConnectOnFailure) throws AcsJContainerEx {
	}
}
