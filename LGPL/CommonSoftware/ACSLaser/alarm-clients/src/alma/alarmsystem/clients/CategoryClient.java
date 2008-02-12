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

import java.util.logging.Logger;

import alma.acs.container.ContainerServices;
import alma.acs.logging.AcsLogLevel;
import alma.alarmsystem.AlarmService;
import alma.alarmsystem.AlarmServiceHelper;
import alma.alarmsystem.Category;
import alma.alarmsystem.clients.alarm.AlarmClientException;

import cern.laser.client.services.selection.AlarmSelectionHandler;
import cern.laser.client.services.selection.AlarmSelectionListener;
import cern.laser.client.services.selection.CategorySelection;
import cern.laser.client.services.selection.Selection;
import cern.laser.console.Configuration;
import cern.laser.console.User;
import cern.laser.console.impl.UserHandlerImpl;
import cern.laser.guiplatform.alarms.AlarmSelectionHandlerFactory;

/**
 * A client that listen to alarms from all the categories.
 * 
 * It is a wrapper to CERN classes in order to simplify
 * the usage from ACS without dealing with low level
 * details of the alarm system.
 * 
 * The class connects to the alarm system as a CERN client,
 * logging in a generic user.
 * The listern receives alarms and errors from the alarm system by means 
 * of a callback.
 * The alarm system sends all the already active alarms when the user logs is.
 * 
 * The close() method has to be called in order to free all the resources.
 * 
 * @author acaproni
 *
 */
public class CategoryClient {
	
	// The user handle
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
		System.out.println("addCategories");
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
	 * Connects to all the categories of the alarm system
	 *  
	 * @param listener The lister to notify of the alarms received from the categories
	 * @throws AlarmClientException
	 */
	public void connect(AlarmSelectionListener listener) throws AlarmClientException {
		connect(listener,null);
	}
	
	/**
	 * Connects to the passed categories of the alarm system
	 * 
	 * @param listener The lister to notify of the alarms received from the categories
	 * @param categories The categories to connect to
	 * @throws AlarmClientException
	 */
	public void connect(AlarmSelectionListener listener, Category[] categories) throws AlarmClientException {
		if (listener==null) {
			throw new IllegalArgumentException("The listener can't be null");
		}
		try {
			getAlarmServiceComponent();
			userHandler=new UserHandlerImpl();
			logger.log(AcsLogLevel.DEBUG,"UserHandler succesfully built");
			
			testUser = userHandler.getUser("test");
			logger.log(AcsLogLevel.DEBUG,"User generated");
			
			defaultConf=testUser.getDefaultConfiguration();
			logger.log(AcsLogLevel.DEBUG,"Getting the selection handler");
			jms_selectionHandler = AlarmSelectionHandlerFactory.getHandler();
			System.out.println("Adding categories");
			addCategories(defaultConf,categories);
			
			// Get the active alarms (they are received by the listener)
			jms_selectionHandler.select(defaultConf.getSelection(),listener);
			
		} catch (Throwable t) {
			throw new AlarmClientException("Exception in ctor: ",t);
		}
	}
	
	/**
	 * Release all the resource,
	 * 
	 * @throws AlarmClientException 
	 */
	public void close() throws AlarmClientException {
		try {
			jms_selectionHandler.close();
			contSvc.releaseComponent(alarm.name());
		} catch (Exception e) {
			throw new AlarmClientException("Exception closing: ",e);
		}
	}
}
