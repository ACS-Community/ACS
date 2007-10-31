package alma.alarmsystem.source;

import com.cosylab.CDB.DAL;

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

import alma.acs.logging.AcsLogLevel;
import alma.acsErrTypeAlarmSourceFactory.acsErrTypeAlarmSourceFactoryEx;
import alma.acsErrTypeAlarmSourceFactory.ACSASFactoryNotInitedEx;
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
	
	private static final String CONFIGURATION_PATH="Alarms/Administrative/AlarmSystemConfiguration";
	// It is true if ACS implementation for sources must be used and
	// null if it has not yet been initialized
	// false means CERN implementation
	private static Boolean useACSAlarmSystem = null;
	
	// The logger
	private static Logger logger=null;
	
	/**
	 * Init the static variables of the class
	 * This method has to be called before executing any other
	 * method. 
	 * 
	 * @param logger The logger
	 * @param dal The DAL to init the AS with CERN or ACS implementation
	 */
	public static void init(Logger logger, DAL dal) throws ErrorGettingDALEx {
		if (logger==null) {
			throw new IllegalArgumentException("Invalid null logger");
		}
		ACSAlarmSystemInterfaceFactory.logger = logger;
		
		useACSAlarmSystem = retrieveImplementationType(dal);
		if (logger!=null) {
			if (useACSAlarmSystem) {
				logger.log(AcsLogLevel.DEBUG,"Alarm system type: ACS");
			} else {
				logger.log(AcsLogLevel.DEBUG,"Alarm system type: CERN");
			}
		}
	}
	
	/**
	 * Cleanup the class.
	 * This method has to be called outside of the class and performs all the necessary
	 * clean up
	 *
	 */
	public static void done() {
		useACSAlarmSystem=null;
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
			dao = dal.get_DAO(CONFIGURATION_PATH);
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
				  Class[] classes = {String.class , Logger.class };
				  Constructor constructor = cl.getConstructor(classes);
				  return (ACSAlarmSystemInterface)constructor.newInstance(sourceName,logger);
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
