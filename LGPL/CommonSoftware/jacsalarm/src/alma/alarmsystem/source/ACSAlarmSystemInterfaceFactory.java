/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package alma.alarmsystem.source;

import java.io.StringReader;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.util.logging.Logger;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

import com.cosylab.CDB.DAL;

import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.container.ContainerServicesBase;
import alma.acs.logging.AcsLogLevel;
import alma.acsErrTypeAlarmSourceFactory.ACSASFactoryNotInitedEx;
import alma.acsErrTypeAlarmSourceFactory.FaultStateCreationErrorEx;
import alma.acsErrTypeAlarmSourceFactory.SourceCreationErrorEx;
import alma.acsErrTypeAlarmSourceFactory.wrappers.AcsJACSASFactoryNotInitedEx;
import alma.acsErrTypeAlarmSourceFactory.wrappers.AcsJFaultStateCreationErrorEx;
import alma.acsErrTypeAlarmSourceFactory.wrappers.AcsJSourceCreationErrorEx;

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
	
	/**
	 * The path in the CDB of the AS configuraton
	 */
	private static final String CONFIGURATION_PATH="Alarms/Administrative/AlarmSystemConfiguration";
	
	/**
	 *  It is <code>true</code> if ACS implementation for sources must be used;
	 *  <code>false</code> means CERN implementation.
	 *  <P>
	 *  It is <code>null</code> if it has not yet been initialized.
	 */
	private static Boolean useACSAlarmSystem = null;
	
	/**
	 * At the present, while using the CERN implementation, 
	 * we use the same source for sending all the alarms
	 * so we can use a singleton and return the same object 
	 * to all the clients.
	 *  
	 *  @see AlarmSystemInterfaceProxy
	 */
	private static ACSAlarmSystemInterface source=null;
	
	/**
	 * The logger
	 */
	private static Logger logger=null;
	
	/**
	 * Container services
	 */
	private static ContainerServicesBase containerServices;
	
	/**
	 * Init the static variables of the class
	 * This method has to be called before executing any other
	 * method. 
	 * 
	 * @param logger The logger
	 * @param dal The DAL to init the AS with CERN or ACS implementation
	 * @throws AcsJContainerServicesEx 
	 */
	public static void init(ContainerServicesBase containerServices) throws AcsJContainerServicesEx {
		if (containerServices==null) {
			throw new AcsJContainerServicesEx(new Exception("Invalid null ContainerServicesBase"));
		}
		ACSAlarmSystemInterfaceFactory.containerServices=containerServices;
		ACSAlarmSystemInterfaceFactory.logger = containerServices.getLogger();
		DAL dal = containerServices.getCDB();
		if (logger==null || dal==null) {
			throw new IllegalArgumentException("Invalid DAL or Logger from ContainerServicesBase");
		}
		
		useACSAlarmSystem = retrieveImplementationType(dal);
		if (logger!=null) {
			if (useACSAlarmSystem) {
				logger.log(AcsLogLevel.DEBUG,"Alarm system type: ACS");
			} else {
				logger.log(AcsLogLevel.DEBUG,"Alarm system type: CERN");
				try {
					initCmwMom();
				} catch (Throwable t) {
					throw new AcsJContainerServicesEx(new Exception("Error initing cmw-mom",t));
				}
			}
		}
	}
	
	/**
	 * Initialize cmw-mom (that in turn inits acs-jms) by setting the container services.
	 * 
	 * The container services must be set in a cmw-mom static variable in order the
	 * messages are published into the NC.
	 * The container services will be passed from this class down to the acs-jms classes.
	 * 
	 * @see cern.cmw.mom.pubsub.impl.ACSJMSTopicConnectionImpl
	 */
	private static void initCmwMom() throws Exception {
		if (containerServices==null) {
			throw new IllegalStateException("Trying to init cmw-mom with null ContainerServicesBase");
		}
		try {
			Thread t = Thread.currentThread();
			ClassLoader loader = t.getContextClassLoader();
			Class cl =loader.loadClass("cern.cmw.mom.pubsub.impl.ACSJMSTopicConnectionImpl");
			Field var = cl.getField("containerServices");
			var.set(null, containerServices);
			logger.log(AcsLogLevel.DEBUG,"cmw-mom/acs-jms initialized");
		} catch (Throwable t) {
			throw new Exception("Error setting ContainerServices into cmw-mom",t);
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
			  if (source!=null) {
				  return source;
			  }
			  try {
				  Thread t = Thread.currentThread();
				  ClassLoader loader = t.getContextClassLoader();
				  Class cl =loader.loadClass("alma.acs.alarmsystem.binding.ACSLaserSource");
				  Class[] classes = {String.class , Logger.class };
				  Constructor constructor = cl.getConstructor(classes);

				  // TODO: take ContainerServicesBase object received in the init method, 
				  // and set it on ACSJMSTopicConnectionImpl.containerServices
				  // Of course in after the ACS 7.0 release rush, this static field communication 
				  // must be replaced with with real calls, or at least the absence of such an object must be detected early.
				  source=(ACSAlarmSystemInterface)constructor.newInstance(sourceName,logger);
				  return source;
			  } catch (Throwable t) {
				  System.out.println("ERROR: "+t.getMessage());
				  t.printStackTrace();
				  throw new AcsJSourceCreationErrorEx(t).toSourceCreationErrorEx();
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
	   * @throws IllegalStateException If the factory has not been initialized yet
	   */
	  public static boolean usingACSAlarmSystem() throws ACSASFactoryNotInitedEx {
		  if (useACSAlarmSystem==null) {
			  Exception e = new IllegalStateException("Factory not initialised");
			  throw new AcsJACSASFactoryNotInitedEx(e).toACSASFactoryNotInitedEx();
		  }
		  return ACSAlarmSystemInterfaceFactory.useACSAlarmSystem;
	  }
}
