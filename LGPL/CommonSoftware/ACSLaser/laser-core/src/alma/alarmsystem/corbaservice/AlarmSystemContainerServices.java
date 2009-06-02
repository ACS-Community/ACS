package alma.alarmsystem.corbaservice;

import java.util.concurrent.ThreadFactory;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.omg.CORBA.ORB;
import org.omg.PortableServer.Servant;

import com.cosylab.CDB.DAL;
import com.cosylab.CDB.DALHelper;

import alma.ACS.OffShoot;
import alma.ACSErrTypeCommon.wrappers.AcsJUnexpectedExceptionEx;
import alma.JavaContainerError.wrappers.AcsJContainerEx;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.container.AdvancedContainerServices;
import alma.acs.container.ContainerServicesBase;
import alma.acs.logging.AcsLogger;

public class AlarmSystemContainerServices implements ContainerServicesBase {
	
	/**
	 * The CORBA ORB
	 */
	private final ORB orb;
	
	/**
	 * The logger
	 */
	private final AcsLogger logger;
	
	/**
	 * The CORBA server fro the alarm system
	 */
	private AlarmSystemCorbaServer alSysCorbaServer;
	
	/**
	 * The name returned by <code>getName()</code>.
	 * 
	 */
	private static final String name = "AlarmService";
	
	/**
	 * The implementation of the {@link AdvancedContainerServices}
	 */
	private final AlarmSystemAdvancedContainerServices advancedContainerServices;
	
	/**
	 * Constructor 
	 * 
	 * @param theOrb The ORB
	 * @param theLogger The logger
	 */
	public AlarmSystemContainerServices(AlarmSystemCorbaServer alSysCorbaServer, AcsLogger theLogger) {
		if (alSysCorbaServer==null) {
			throw new IllegalArgumentException("The AlarmSystemCorbaServer can't be null");
		}
		if (theLogger==null) {
			throw new IllegalArgumentException("The logger can't be null");
		}
		this.alSysCorbaServer=alSysCorbaServer;
		this.orb=alSysCorbaServer.getORB();
		logger=theLogger;
		advancedContainerServices= new AlarmSystemAdvancedContainerServices(this);
	}
	
	public void deactivateOffShoot(Servant cbServant)
	throws AcsJContainerServicesEx 	{
		try {
			alSysCorbaServer.deactivateOffShoot(cbServant);
		} catch (AcsJContainerEx ex) {
			throw new AcsJContainerServicesEx(ex);
		}
	}

	@Override
	public OffShoot activateOffShoot(Servant cbServant)
			throws AcsJContainerServicesEx {
		try {
			return alSysCorbaServer.activateOffShoot(cbServant);
		} catch (AcsJContainerEx ex) {
			throw new AcsJContainerServicesEx(ex);
		} catch (AcsJUnexpectedExceptionEx uex) {
			throw new AcsJContainerServicesEx(uex);
		}
	}

	@Override
	public AdvancedContainerServices getAdvancedContainerServices() {
		return advancedContainerServices;
	}

	@Override
	public AcsLogger getLogger() {
		return logger;
	}

	@Override
	public String getName() {
		return name;
	}

	@Override
	public ThreadFactory getThreadFactory() {
		// TODO Auto-generated method stub
		return null;
	}
	
	@Override
	public DAL getCDB() throws AcsJContainerServicesEx {
		try {
			org.omg.CORBA.Object dalObj = alSysCorbaServer.getServiceFromNameServer("CDB");
			DAL dal = DALHelper.narrow(dalObj);
			return dal;
		} catch (Throwable thr) {
			String msg = "Unexpectedly failed to get the CDB reference!";
			logger.log(Level.FINE, msg, thr);
			AcsJContainerServicesEx ex = new AcsJContainerServicesEx(thr);
			ex.setContextInfo(msg);
			throw ex;
        }
	}

	/**
	 * 
	 * @return The Orb
	 */
	public ORB getOrb() {
		return orb;
	}

}
