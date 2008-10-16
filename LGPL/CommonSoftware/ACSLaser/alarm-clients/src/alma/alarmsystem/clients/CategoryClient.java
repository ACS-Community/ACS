/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2007
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */
package alma.alarmsystem.clients;

import java.util.Set;
import java.util.logging.Logger;

import alma.acs.container.ContainerServices;
import alma.acs.logging.AcsLogLevel;
import alma.alarmsystem.AlarmService;
import alma.alarmsystem.AlarmServiceHelper;
import alma.alarmsystem.Category;
import alma.alarmsystem.clients.alarm.AlarmClientException;
import alma.maciErrType.wrappers.AcsJCannotGetComponentEx;

import cern.laser.client.data.Alarm;
import cern.laser.client.services.selection.AlarmSelectionHandler;
import cern.laser.client.services.selection.AlarmSelectionListener;
import cern.laser.client.services.selection.CategorySelection;
import cern.laser.client.services.selection.Selection;
import cern.laser.console.Configuration;
import cern.laser.console.User;
import cern.laser.console.impl.UserHandlerImpl;

/**
 * A client that listen to alarms from all the categories.
 * 
 * It is a wrapper to CERN classes in order to simplify
 * the usage from ACS without dealing with low level
 * details of the alarm system.
 * 
 * The class connects to the alarm system as a CERN client,
 * logging in a generic user.
 * The listener receives alarms and errors from the alarm system by means 
 * of a callback.
 * The alarm system sends all the already active alarms when the user logs is.
 * 
 * The close() method has to be called in order to free all the resources.
 * 
 * @author acaproni
 *
 */
public class CategoryClient {
	
	// The user handler
	private UserHandlerImpl userHandler;
	
	// The user to log in (test for instance)
	private User testUser;
	
	// The user's default configuration
	private Configuration defaultConf;
	
	// The alarm selection handler
	private AlarmSelectionHandler jms_selectionHandler;
	
	// ACS ContainerServices
	private ContainerServices contSvc;
	
	// The logger
	private Logger logger;
	
	// The alarm service component and its IDL
	private final String alarmServiceIDL = "*/AlarmService:*";
	private AlarmService alarm;
	
	// To avoid to release the resources twice
	private volatile boolean closed=false;
	
	/**
	 * Constructor
	 * 
	 * @param contServices The containerServices
	 * @throws AlarmClientException
	 */
	public CategoryClient(ContainerServices contServices) throws AlarmClientException {
		if (contServices==null) {
			throw new IllegalArgumentException("ContainerServices can't be null");
		}
		contSvc=contServices;
		
		logger=contSvc.getLogger();
		if (logger==null) {
			throw new IllegalStateException("Got a null logger from the container services!");
		}
		
		
		
		System.out.println("AlarmSystemClient built");
		
	}
	
	/**
	 * Connect the AlarmSrevice component
	 */
	private void getAlarmServiceComponent() throws Exception {
		String[] names  = contSvc.findComponents("*",alarmServiceIDL);
		if (names==null || names.length==0) {
			// Nothing has been found
			throw new Exception("No available alarm component");
		}
		if (names.length>1) {
			contSvc.getLogger().log(AcsLogLevel.WARNING,"More then one AlarmService component found");
			
		}
		contSvc.getLogger().log(AcsLogLevel.DEBUG,"Getting "+names[0]);
		// Get a reference to the first component
		alarm=AlarmServiceHelper.narrow(contSvc.getComponent(names[0]));
		contSvc.getLogger().log(AcsLogLevel.DEBUG,names[0]+" connected");
	}
	
	/**
	 * Add the categories to the configuration i.e. add the categories 
	 * the client wants to listen to
	 * 
	 * @param config The Configuration
	 * @param categories The categories to listen to
	 *                   If it is null, it adds all the categories returned
	 *                   by the alarm system component
	 * @throws Exception 
	 */
	private void addCategories(Configuration config,Category[] categories) throws Exception {
		if (categories==null) {
			categories = alarm.getCategories();
		}
		if (categories.length==0) {
			logger.log(AcsLogLevel.WARNING,"No categories to connect to");
			return;
		}
		Selection selection = config.getSelection();
		CategorySelection catSel = selection.createCategorySelection();
		for (Category cat: categories) {
			cern.laser.business.data.CategoryImpl businessCategory = new cern.laser.business.data.CategoryImpl(
					cat.categoryId,
					cat.name,
					cat.description,
					cat.path,
					cat.leaf);
			cern.laser.client.impl.data.CategoryImpl cImpl=new cern.laser.client.impl.data.CategoryImpl(businessCategory);
			
			catSel.add(cImpl);
		}
		selection.setCategorySelection(catSel);
	}
	
	/**
	 * Connects to all the categories of the alarm system.
	 * 
	 * It is equivalent to <code>connect(listener,null)</code>. 
	 *  
	 * @param listener The lister to notify of the alarms received from the categories
	 * @throws AlarmClientException In case of failure connecting the client
	 * @throws AcsJCannotGetComponentEx If the alarm service component is not available
	 * 
	 * @see {@link CategoryClient.connect(AlarmSelectionListener listener, Category[] categories)}
	 */
	public void connect(AlarmSelectionListener listener) throws AlarmClientException, AcsJCannotGetComponentEx {
		connect(listener,null);
	}
	
	/**
	 * Connects to the passed categories of the alarm system
	 * 
	 * @param listener The lister to notify of the alarms received from the categories
	 * @param categories The categories to connect to
	 * @throws AcsJCannotGetComponentEx In case the AlarmService is not available
	 * @throws AlarmClientException In case of failure connecting the client
	 */
	public void connect(AlarmSelectionListener listener, Category[] categories) throws AlarmClientException, AcsJCannotGetComponentEx {
		if (listener==null) {
			throw new IllegalArgumentException("The listener can't be null");
		}
		if (closed) {
			throw new IllegalStateException("SourceClient is closed!");
		}
		try {
			getAlarmServiceComponent();
		} catch (Throwable t) {
			AcsJCannotGetComponentEx ex = new AcsJCannotGetComponentEx(t);
			ex.setReason("Alarm service component unavailable");
			throw ex;
		}
		try {
			userHandler=new UserHandlerImpl();
			logger.log(AcsLogLevel.DEBUG,"UserHandler succesfully built");
			
			testUser = userHandler.getUser("test");
			logger.log(AcsLogLevel.DEBUG,"User generated");
			
			defaultConf=testUser.getDefaultConfiguration();
			logger.log(AcsLogLevel.DEBUG,"Getting the selection handler");
			jms_selectionHandler = AlarmSelectionHandler.get();
			addCategories(defaultConf,categories);
			
			// Get the active alarms (they are received by the listener)
			java.util.Map<String, Alarm> alreadyActive=jms_selectionHandler.select(defaultConf.getSelection(),listener);
			if (alreadyActive!=null && alreadyActive.size()>0) {
				Set<String> keys = alreadyActive.keySet();
				for (String key: keys) {
					listener.onAlarm(alreadyActive.get(key));
				}
			}
		} catch (Throwable t) {
			throw new AlarmClientException("Exception connecting the category client",t);
		}
	}
	
	/**
	 * Release all the resource,
	 * 
	 * @throws AlarmClientException 
	 */
	public void close() throws AlarmClientException {
		closed=true;
		try {
			if (jms_selectionHandler!=null) {
				jms_selectionHandler.close();
			}
			if (contSvc!=null && alarm!=null) {
				contSvc.releaseComponent(alarm.name());
			}
		} catch (Exception e) {
			throw new AlarmClientException("Exception closing: ",e);
		}
	}
	
	/**
	 * Ensure that the resources have been released before destroying the object
	 */
	public void finalize() throws Throwable {
		if (!closed) {
			close();
		}
		super.finalize();
	}
	
	/**
	 * Get the parents of a reduced alarm.
	 * <P>
	 * The method asks the alarm service component for the parents of a reduced
	 * alarm.
	 * 
	 * @param id The id of the alarm 
	 * @param node <code>true</code> if the alarm is a Node alarm;
	 * 			   <code>false</code> if the alarm of the passed ID is a multiplicity.
	 * @return The array of the alarm parent of the alarm with the passed ID
	 * @throws AlarmClientException In case of error getting the alarms from the component 
	 */
	public  alma.alarmsystem.Alarm[] getParents(String id, boolean node) throws AlarmClientException {
		if (id==null || id.isEmpty()) {
			throw new IllegalArgumentException("Invalid alarm ID");
		}
		alma.alarmsystem.Alarm[] alarms = null;
		try {
			if (node) {
				alarms=alarm.getNodeParents(id);
			} else {
				alarms=alarm.getMultiplicityParents(id);
			}
		} catch (Throwable t) {
			throw new AlarmClientException("Error getting parents from component",t);
		}
		return alarms;
	}
	
	/**
	 * Get the parents of a reduced alarm.
	 * <P>
	 * The method asks the alarm service component for the children of a reduced
	 * alarm.
	 * 
	 * @param id The id of the alarm 
	 * @param node <code>true</code> if the alarm is a Node alarm;
	 * 			   <code>false</code> if the alarm of the passed ID is a multiplicity.
	 * @return The array of the alarm parent of the alarm with the passed ID
	 * @throws AlarmClientException In case of error getting the alarms from the component 
	 */
	public  alma.alarmsystem.Alarm[] getChildren(String id, boolean node) throws AlarmClientException {
		if (id==null || id.isEmpty()) {
			throw new IllegalArgumentException("Invalid alarm ID");
		}
		alma.alarmsystem.Alarm[] alarms = null;
		try {
			if (node) {
				alarms=alarm.getNodeChildren(id);
			} else {
				alarms=alarm.getMultiplicityChildren(id);
			}
		} catch (Throwable t) {
			throw new AlarmClientException("Error getting children from component",t);
		}
		return alarms;
	}
	
	/**
	 * Return the threshold for the multiplicity node whose
	 * ID is passed as parameter.
	 * 
	 * @param id The id of the multiplicity node
	 * @return The multiplicity threshold of the node with the given id
	 * @throws AlarmClientException If an error happens getting the threshold from the component 
	 */
	public int  getMultiplicityThreshold(String id) throws AlarmClientException {
		if (id==null || id.isEmpty()) {
			throw new IllegalArgumentException("Invalid alarm ID");
		}
		try {
			return alarm.getMultiplicityThreshold(id);
		} catch (Throwable t) {
			throw new AlarmClientException("Error getting threshold from component",t);
		}
	}
	
	/**
	 * Get the active children of a reduced alarm.
	 * <P>
	 * The method asks the alarm service component for the active children of a reduced
	 * alarm.
	 * 
	 * @param id The id of the alarm 
	 * @param node <code>true</code> if the alarm is a Node alarm;
	 * 			   <code>false</code> if the alarm of the passed ID is a multiplicity.
	 * @return The array of the alarm parent of the alarm with the passed ID
	 * @throws AlarmClientException In case of error getting the alarms from the component 
	 */
	public  alma.alarmsystem.Alarm[] getActiveChildren(String id, boolean node) throws AlarmClientException {
		if (id==null || id.isEmpty()) {
			throw new IllegalArgumentException("Invalid alarm ID");
		}
		alma.alarmsystem.Alarm[] alarms = null;
		try {
			if (node) {
				alarms=alarm.getActiveNodeChildren(id);
			} else {
				alarms=alarm.getActiveMultiplicityChildren(id);
			}
		} catch (Throwable t) {
			throw new AlarmClientException("Error getting children from component",t);
		}
		return alarms;
	}
	
//	/**
//	 * Convert a CORBA alarm into a client alarm.
//	 * 
//	 * @param alarm The CORBA alarm
//	 * @return the client alarm
//	 */
//	private Alarm convertAlarmType(alma.alarmsystem.Alarm alarm) {
//		SourceDefinition sourceDef = new SourceDefinition(
//				alarm.alarmSource.
//				); 
//		Source businessSource = new Source();
//		
//		cern.laser.business.data.Alarm businessAlarm = new AlarmImpl(
//				alarm.alarmId,
//				alarm.systemName,
//				alarm.identifier,
//				alarm.problemDescription,
//				alarm.priority,
//				alarm.cause,
//				alarm.action,
//				alarm.consequence,
//				alarm.piquetGSM,
//				alarm.piquetEmail,
//				alarm.helpURL,
//				alarm.instant,
//				alarm.alarmSource,
//				alarm.alarmLocation,
//				alarm.alarmResponsiblePerson,
//				alarm.categories,
//				alarm.alarmStatus,
//				alarm.alarmTriplet,
//				alarm.nodeParent,
//				alarm.multiplicityParent,
//				alarm.nodeChild,
//				alarm.multiplicityChild
//				);
//	}
}
