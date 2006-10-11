package alma.lasersource.test;

import java.io.File;
import java.io.FileWriter;
import java.util.logging.Logger;

import org.omg.CORBA.ORB;

import si.ijs.maci.Manager;
import si.ijs.maci.ManagerHelper;
import com.cosylab.CDB.JDAL;
import com.cosylab.CDB.JDALHelper;

import alma.acs.logging.ClientLogManager;

/**
 * A class containing a set of useful methods needed by the tests
 * 
 * NOTE:
 * In this test I am interested in cheking if the alarms are sent to the log.
 * CurrentlyI have the following message in the stdout (I think I'll mask that
 * in the TestList.grep):
 * failed to flush logging queue because remote logging service has not been made available.
 * 
 * This message is here because I have not initialized correctly the log and the remote logging
 * is disabled.
 * To fix that I should define this class as a Client, log into the manager and initialize the log.
 * This would require a lot of time because I don't have the SimpleClient available at this point:
 * everything should be done by hand.
 * Ok... I don't care about thi error because if the message is in the stdout it means that 
 * everything is working.
 * 
 * @author acaproni
 *
 */
public class TestUtil {
	

	// A reference to the ORB
	private static ORB orb = null;
	
	// A reference to the Manager
	private static Manager manager=null;
	
	// A reference to the logger
	private static Logger logger = null;
	
	/**
	 * Return the logger
	 * 
	 * @return The logger
	 */
	public static Logger getLogger(String name) {
		if (logger!=null) {
			return logger;
		}
		// The logger should be initialized in order to send logs remotely:
		// read the NOTE in header of the class
		logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(name,true);
		return logger;
	}
	
	/**
	 * 
	 * @return The ORB
	 */
	public static ORB getORB() {
		if (orb!=null) {
			return orb;
		}
		String args[] = {};
		orb=ORB.init(args, null);
		return orb;
	}
	
	/**
	 * Get a reference to the Manager
	 */
	public static Manager getManager() {
		if (manager!=null) {
			return manager;
		}
		String managerLoc = System.getProperty("ACS.manager");
        if (managerLoc == null) {
                System.out.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
                System.exit(-1);
        }
        
		org.omg.CORBA.Object managerObj = getORB().string_to_object(managerLoc);
		if (managerObj==null) {
			return null;
		} 
		manager = ManagerHelper.narrow(managerObj);
        return manager;
	}
	
	/**
	 * Get a reference to the DAL
	 * 
	 * @param manager A reference to the Manager
	 * @return The reference to the DAL or NULL
	 */
	public static JDAL getDAL(Manager manager) {
		JDAL dal;
		try {
	        org.omg.CORBA.Object cdbObj = manager.get_service(0, "CDB", false);
	        if (cdbObj==null) {
				System.out.println("Error getting the CDB from the manager");
				return null;
			} 
	        dal = JDALHelper.narrow(cdbObj);
	        if (dal==null) {
	        	System.out.println("Error narrowing the DAL");
	        	return null;
	        }
		} catch (Exception e) {
			System.out.println("Error getting DAL: ");
			e.printStackTrace();
			return null;
		}
		return dal;
	}

}
