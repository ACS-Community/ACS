package alma.alarmsystem.source;

import cern.laser.source.alarmsysteminterface.ASIException;
import cern.laser.source.alarmsysteminterface.AlarmSystemInterfaceFactory;
import cern.laser.source.alarmsysteminterface.AlarmSystemInterface;
import cern.laser.source.alarmsysteminterface.impl.AlarmSystemInterfaceProxy;

import alma.acs.util.ACSPorts;

import si.ijs.maci.Manager;
import si.ijs.maci.ManagerHelper;
import com.cosylab.CDB.DAL;
import com.cosylab.CDB.DALHelper;

import org.omg.CORBA.ORB;
import org.omg.CORBA.Object;
import org.omg.CORBA.IntHolder;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.w3c.dom.Node;
import org.w3c.dom.NamedNodeMap;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import java.io.StringReader;

/**
 * ACSAlarmSystemInterfaceFactory extends the CERN AlarmSystemInterfaceFactory
 * to create sources with different implementations depending on the actual
 * configuration in the CDB.
 * 
 * The type of implementation is in AlarmSystemConfiguration.xml: a property named
 * Implementation.
 * CERN implementation is used only if the property is CERN. 
 * If the property is not found, is ACS or the CDB record does not exist the 
 * ACS implementation for sources is used.
 * 
 * @author acaproni
 *
 */
public class ACSAlarmSystemInterfaceFactory extends AlarmSystemInterfaceFactory {
	// It is true if ACS implementation for sources must be used and
	// null if it has not yet been initialized
	// false means CERN implementation
	private static Boolean useACSImplementation = null;
	
	private static boolean useACSImplementation() {
		if (useACSImplementation!=null) {
			return useACSImplementation;
		}
		
        DAL dal = getDAL();
        if (dal==null) {
        	useACSImplementation=true;
        } else {
        	useACSImplementation = retrieveImplementationType(dal);
        }
        if (useACSImplementation) {
        	System.out.println("Using ACS implementation");
        } else {
        	System.out.println("Using CERN implementation");
        }
        return useACSImplementation;
	}
	
	/**
	 * Read the Implementation property from the Alarm System Configuration
	 * 
	 * @param dal The DAL
	 * 
	 * @return false if the Implementation property is CERN 
	 *         true otherwise
	 */
	private static boolean retrieveImplementationType(DAL dal) {
		if (dal==null) {
			return true;
		}
		String dao;
		try {
			dao = dal.get_DAO("Alarms/AlarmSystemConfiguration");
		} catch (Exception e) {
			System.out.println("Alarms/AlarmSystemConfiguration not found");
			return true;
		}
		String implementation = getProperty(dao,"Implementation");
		return implementation==null || implementation.equals("ACS");
	}
	
	/**
	 * Get the value of a property from the DAO.
	 * 
	 * @param dao The dao (XML string)
	 * @param propName The name of the property
	 * @return The value of the property with the given name
	 *         null if the property doesn't exist
	 */
	private static String getProperty(String dao, String propName) {
		if (dao==null || propName==null) {
			return null;
		}
		DocumentBuilder builder = null;
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		
		try {
			builder = factory.newDocumentBuilder();
		} catch (Exception e) {
			System.out.println("Error instantiating the document builder");
			System.out.println(e.getMessage());
			e.printStackTrace();
			return null;
		}
		Document doc;
		try {
			doc = builder.parse(new InputSource(new StringReader(dao)));
		} catch (Exception e) {
			System.out.println("Error parsing the DAO: ["+dao+"]");
			System.out.println(e.getMessage());
			e.printStackTrace();
			return null;
		}
		NodeList propNodeList = doc.getElementsByTagName("configuration-property");
		String val = null; // The value of the property to return
		for (int t=0; t<propNodeList.getLength(); t++) {
			Node node = propNodeList.item(t);
			// Get the attributes
			NamedNodeMap attributes = node.getAttributes();
			if (attributes==null) {
				continue;
			}
			// Get the attribute "name"
			Node attrNodeName=attributes.getNamedItem("name");
			if (attrNodeName==null) {
				continue;
			}
			String nameNode = attrNodeName.getNodeValue();
			if (nameNode==null) {
				continue;
			}
			if (nameNode.equals(propName)) {
				// We have found the property!
				Node child = node.getFirstChild();
				val = child.getNodeValue();
				break;
			}
			
		}
		System.out.println("Returning <"+propName+","+val+">");
		return val;
	}
	
	/**
	 * Get the DAL
	 * 
	 * @return A reference to the DAL
	 */
	private static DAL getDAL() {
		String managerLoc = System.getProperty("ACS.manager");
        if (managerLoc == null) {
                System.out.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
                System.exit(-1);
        }
        String args[] = {};
		ORB orb=ORB.init(args, null);
		if (orb==null) {
			return null;
		}
		org.omg.CORBA.Object managerObj = orb.string_to_object(managerLoc);
		if (managerObj==null) {
			return null;
		} 
		Manager manager = ManagerHelper.narrow(managerObj);
        if (manager==null) {
			return null;
		} 
        IntHolder status = new IntHolder();
        org.omg.CORBA.Object cdbObj = manager.get_service(0, "CDB", false, status);
        if (cdbObj==null) {
			return null;
		} 
        DAL dal = DALHelper.narrow(cdbObj);
        
        return dal;
	}
	
	/**
	   * Create a new instance of an alarm system interface.
	   * @param sourceName the source name.
	   * @return the interface instance.
	  * @throws ASIException if the AlarmSystemInterface instance can not be created.
	   */
	  public static AlarmSystemInterface createSource(String sourceName) throws ASIException {
		  System.out.println("ACS implementation: "+useACSImplementation());
		  return new AlarmSystemInterfaceProxy(sourceName);
	  }

	  /**
	   * Create a new instance of an alarm system interface without binding it to any source.
	   * @return the interface instance.
	  * @throws ASIException if the AlarmSystemInterface instance can not be created.
	   */
	  public static AlarmSystemInterface createSource() throws ASIException {
		  System.out.println("ACS implementation: "+useACSImplementation());
		  return new AlarmSystemInterfaceProxy("UNDEFINED");
	  }
}
