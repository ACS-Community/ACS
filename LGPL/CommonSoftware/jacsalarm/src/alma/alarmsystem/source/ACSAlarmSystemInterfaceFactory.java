package alma.alarmsystem.source;

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

import java.lang.reflect.Constructor;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.w3c.dom.Node;
import org.w3c.dom.NamedNodeMap;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import alma.acsErrTypeAlarmSourceFactory.acsErrTypeAlarmSourceFactoryEx;
import alma.acsErrTypeAlarmSourceFactory.ACSASFactoryNotInitedEx;
import alma.acsErrTypeAlarmSourceFactory.InavalidManagerEx;
import alma.acsErrTypeAlarmSourceFactory.ErrorGettingDALEx;
import alma.acsErrTypeAlarmSourceFactory.SourceCreationErrorEx;
import alma.acsErrTypeAlarmSourceFactory.FaultStateCreationErrorEx;
import alma.acsErrTypeAlarmSourceFactory.wrappers.AcsJACSASFactoryNotInitedEx;
import alma.acsErrTypeAlarmSourceFactory.wrappers.AcsJInavalidManagerEx;
import alma.acsErrTypeAlarmSourceFactory.wrappers.AcsJErrorGettingDALEx;
import alma.acsErrTypeAlarmSourceFactory.wrappers.AcsJSourceCreationErrorEx;
import alma.acsErrTypeAlarmSourceFactory.wrappers.AcsJFaultStateCreationErrorEx;

import java.util.logging.Logger;

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
public class ACSAlarmSystemInterfaceFactory {
	// It is true if ACS implementation for sources must be used and
	// null if it has not yet been initialized
	// false means CERN implementation
	private static Boolean useACSAlarmSystem = null;
	
	// The ORB
	private static ORB orb=null;
	
	// The Manager
	private static Manager manager=null;
	
	// The manager handle
	private static int managerHandle=0;
	
	// The logger
	private static Logger logger=null;
	
	/**
	 * Init the static variables of the class
	 * This method has to be called outside of the class before executing any other
	 * methods. In this implementation it is called the first time a static method is
	 * executed (in this case infact useACSAlarmSystem is null)
	 * 
	 * @param theORB The ORB 
	 * @param manager A reference to the manager 
	 * @param logger The logger
	 * @param mgrHandle The manager handle
	 */
	public static void init(ORB theORB, Manager manager, int mgrHandle, Logger logger) throws InavalidManagerEx, ErrorGettingDALEx {
		ACSAlarmSystemInterfaceFactory.orb=theORB;
		ACSAlarmSystemInterfaceFactory.manager=manager;
		ACSAlarmSystemInterfaceFactory.managerHandle=mgrHandle;
		ACSAlarmSystemInterfaceFactory.logger = logger;
		if (manager==null) {
			manager = getManager();
		}
		if (manager==null) {
			NullPointerException e = new NullPointerException("Error getting the manager!");
			AcsJInavalidManagerEx acsE = new AcsJInavalidManagerEx(e);
			throw acsE.toInavalidManagerEx();
		}
		
		DAL dal=getDAL(manager,managerHandle);
        useACSAlarmSystem = retrieveImplementationType(dal);
	}
	
	/**
	 * Cleanup the class.
	 * This method has to be called outside of the class and performs all the necessary
	 * clean up
	 *
	 */
	public static void done() {
		useACSAlarmSystem=null;
		manager=null;
		orb=null;
	}
	
	/**
	 * Get a reference to the DAL
	 * 
	 * @param manager A reference to the Manager
	 * @param handle The manager handle
	 */
	private static DAL getDAL(Manager manager, int handle) throws ErrorGettingDALEx {
		IntHolder status = new IntHolder();
		DAL dal;
		try {
	        org.omg.CORBA.Object cdbObj = manager.get_service(handle, "CDB", true, status);
	        if (cdbObj==null) {
				throw new NullPointerException("Error getting the CDB from the manager");
			} 
	        dal = DALHelper.narrow(cdbObj);
	        if (dal==null) {
	        	throw new NullPointerException("Error narrowing the DAL");
	        }
		} catch (Exception e) {
			AcsJErrorGettingDALEx dalEx = new AcsJErrorGettingDALEx(e);
			throw dalEx.toErrorGettingDALEx();
		}
		return dal;
	}
	
	/**
	 * Return the orb
	 * If it is null, a new ORB is created and inited
	 * 
	 * @return The ORB
	 */
	private static ORB getORB() {
		if (orb!=null) {
			return orb;
		}
		String args[] = {};
		orb = ORB.init(args, null);
		return orb;
	}
	
	/**
	 * Get a reference to the Manager
	 */
	private static Manager getManager() {
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
			return true;
		}
		String implementation = getProperty(dao,"Implementation");
		return implementation==null || !implementation.equals("CERN");
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
		return val;
	}
	
	/**
	 * Create a new instance of an alarm system interface.
	 * @param sourceName the source name.
	 * @return the interface instance.
	 * @throws ASIException if the AlarmSystemInterface instance can not be created.
	 */
	  public synchronized static ACSAlarmSystemInterface createSource(String sourceName) throws ACSASFactoryNotInitedEx, SourceCreationErrorEx  {
		  if (useACSAlarmSystem==null) {
			  Exception e = new IllegalStateException("Factory not initialised");
			  throw new AcsJACSASFactoryNotInitedEx(e).toACSASFactoryNotInitedEx();
		  }
		  if (useACSAlarmSystem) {
			  return new ACSAlarmSystemInterfaceProxy(sourceName,logger);
		  } else {
			  try {
				  Thread t = Thread.currentThread();
				  ClassLoader loader = t.getContextClassLoader();
				  Class cl =loader.loadClass("alma.acs.alarmsystem.binding.ACSLaserSource");
				  Class[] classes = {Class.forName("java.lang.String")};
				  Constructor constructor = cl.getConstructor(classes);
				  return (ACSAlarmSystemInterface)constructor.newInstance(sourceName);
			  } catch (Exception e) {
				  System.out.println("ERROR: "+e.getMessage());
				  e.printStackTrace();
				  throw new AcsJSourceCreationErrorEx(e).toSourceCreationErrorEx();
			  }
		  }
	  }

	  /**
	   * Create a new instance of an alarm system interface without binding it to any source.
	   * @return the interface instance.
	  * @throws ASIException if the AlarmSystemInterface instance can not be created.
	   */
	  public static ACSAlarmSystemInterface createSource() throws ACSASFactoryNotInitedEx, SourceCreationErrorEx {
		  return createSource("UNDEFINED");
	  }
	  
	  /** 
	   * Factory method for creating ACSFaultState instances.
	   * @return a new ACSFaultState instance.
	   *
	   */
	  public synchronized static ACSFaultState createFaultState() throws ACSASFactoryNotInitedEx, FaultStateCreationErrorEx{
		  if (useACSAlarmSystem==null) {
			  Exception e = new IllegalStateException("Factory not initialised");
			  throw new AcsJACSASFactoryNotInitedEx(e).toACSASFactoryNotInitedEx();
		  }
		  if (useACSAlarmSystem) {
			  return new ACSFaultStateImpl();
		  } else {
			  try {
				  Thread t = Thread.currentThread();
				  ClassLoader loader = t.getContextClassLoader();
				  Class cl =loader.loadClass("alma.acs.alarmsystem.binding.ACSLaserFaultStateImpl");
				  Class[] classes = {};
				  Constructor constructor = cl.getConstructor(classes);
				  return (ACSFaultState)constructor.newInstance();
			  } catch (Exception e) {
				  throw new AcsJFaultStateCreationErrorEx(e).toFaultStateCreationErrorEx();
			  }
		  }
	  }

	  /** 
	   * Factory method for creating ACSFaultState instances.
	   * @return a new ACSFaultState instance.
	   * @param family the fault family.
	   * @param member the fault member.
	   * @param code the fault code.
	   */
	  public static ACSFaultState createFaultState(String family, String member, int code) throws ACSASFactoryNotInitedEx, FaultStateCreationErrorEx {
		  ACSFaultState state = createFaultState();
		  state.setFamily(family);
		  state.setMember(member);
		  state.setCode(code);
		  return state;
	  }
	  
	  /**
	   * Return the type of AS used
	   * 
	   * @return True if ACS AS is used, false otherwise
	   */
	  public static boolean usingACSAlarmSystem() throws ACSASFactoryNotInitedEx {
		  if (useACSAlarmSystem==null) {
			  Exception e = new IllegalStateException("Factory not initialised");
			  throw new AcsJACSASFactoryNotInitedEx(e).toACSASFactoryNotInitedEx();
		  }
		  return ACSAlarmSystemInterfaceFactory.useACSAlarmSystem;
	  }
}
