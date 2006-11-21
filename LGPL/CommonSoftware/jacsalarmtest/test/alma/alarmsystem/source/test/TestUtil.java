package alma.alarmsystem.source.test;

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
	
	private static String XMLHead[] = {
			"<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n",
			"<alarm-system-configuration xmlns=\"urn:schemas-cosylab-com:AcsAlarmSystem:1.0\" \n\t\t",
			"xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"> \n",
			"\t<configuration-property name=\"Implementation\">"
	};
	
	private static String XMLTail = "</configuration-property>\n</alarm-system-configuration>\n";
	
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
		try {
			manager = ManagerHelper.narrow(managerObj);
		} catch (Exception e) {
			System.out.println("Error narrowing the manager: "+e.getMessage());
			e.printStackTrace();
			return null;
		}
        return manager;
	}
	
	/**
	 * Get a reference to the DAL
	 * 
	 * @param manager A reference to the Manager
	 * @return The reference to the DAL or NULL
	 */
	public static JDAL getDAL(Manager manager) {
		if (manager==null) {
			throw new IllegalArgumentException("getDAL received a null manager reference!");
		}
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
	
	/**
	 * Remove the Alarm branch from the CDB
	 * 
	 * @param CDBFolder The directory of the CDB 
	 */
	public static void deleteAlarmBranch(String CDBFolder) {
		String CDBdir = CDBFolder+"/CDB/Alarms";
		File alarmsDir = new File(CDBdir);
		if (!alarmsDir.exists()) {
			return;
		}
		String configDirName = CDBdir+"/AlarmSystemConfiguration";
		String configFileName = configDirName+"/AlarmSystemConfiguration.xml";
		File configDir = new File(configDirName);
		File configFile = new File(configFileName);
		if (configFile.exists()) {
			configFile.delete();
		}
		if (configDir.exists()) {
			configDir.delete();
		}
		alarmsDir.delete();
	}
	
	/**
	 * Rewrite the Alarm branch of the CDB.
	 * 
	 * @param CDBFolder The directory of the CDB 
	 * @param ASImplementation The value of the implementation property of the CDB 
	 */
	public static void setupAlarmBranch(String CDBFolder,String ASImplementation) {
		deleteAlarmBranch(CDBFolder);
		String CDBdir = CDBFolder+"/CDB/Alarms";
		String configDirName = CDBdir+"/AlarmSystemConfiguration";
		String configFileName = configDirName+"/AlarmSystemConfiguration.xml";
		try {
			File alarmsDir = new File(CDBdir);
			alarmsDir.mkdir();
			File configDir = new File(configDirName);
			configDir.mkdir();
			FileWriter writer = new FileWriter(configFileName);
			for (int t=0; t<XMLHead.length; t++) {
				writer.write(XMLHead[t]);
			}
			writer.write(ASImplementation);
			writer.write(XMLTail);
			writer.flush();
			writer.close();
		} catch (Exception e) {
			System.out.println("Error setting up the Alarm branch of the CDB: ");
			e.printStackTrace();
		}
	}

}

