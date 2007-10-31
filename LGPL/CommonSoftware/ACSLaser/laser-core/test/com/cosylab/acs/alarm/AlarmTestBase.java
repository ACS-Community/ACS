/*
 * Copyright (c) 2004 by Cosylab d.o.o.
 *
 * The full license specifying the redistribution, modification, usage and other
 * rights and obligations is included with the distribution of this project in
 * the file license.html. If the license is not included you may find a copy at
 * http://www.cosylab.com/legal/abeans_license.htm or may write to Cosylab, d.o.o.
 *
 * THIS SOFTWARE IS PROVIDED AS-IS WITHOUT WARRANTY OF ANY KIND, NOT EVEN THE
 * IMPLIED WARRANTY OF MERCHANTABILITY. THE AUTHOR OF THIS SOFTWARE, ASSUMES
 * _NO_ RESPONSIBILITY FOR ANY CONSEQUENCE RESULTING FROM THE USE, MODIFICATION,
 * OR REDISTRIBUTION OF THIS SOFTWARE.
 */

package com.cosylab.acs.alarm;

import java.util.logging.Logger;

import junit.framework.TestCase;
import alma.acs.component.client.ComponentClient;
import alma.alarmsystem.AlarmService;
import cern.cmw.mom.pubsub.impl.ACSJMSTopicConnectionImpl;
import cern.laser.client.LaserException;
import cern.laser.client.impl.common.AlarmServiceSingleton;
import cern.laser.definition.AdminUser;
import cern.laser.definition.AdminUserHandler;
import cern.laser.definition.LaserDefinitionException;


/**
 * DOCUMENT ME!
 *
 * @author kzagar To change the template for this generated type comment go to
 *         Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and
 *         Comments
 */
public class AlarmTestBase extends TestCase
{
	protected AlarmService alarmService;
	protected ComponentClient client;
	public static final String ADMIN_USERNAME = "test_admin";
	public static final String ADMIN_PASSWORD = "password";
	
	protected Logger log;
	
	/**
	 * Creates a new AlarmTestBase object.
	 */
	public AlarmTestBase()
	{
		super();
		init();
	}

	/**
	 * Constructor for AlarmTestBase.
	 *
	 * @param arg0
	 */
	public AlarmTestBase(String arg0)
	{
		super(arg0);
		init();
	}

	private void init()
	{
		// By default, LASER host is the local machine.
		final String LASER_HOSTS = "laser.hosts";

		if (System.getProperty(LASER_HOSTS) == null) {
			System.setProperty(LASER_HOSTS, "localhost");
		}
		
		String managerLoc = System.getProperty("ACS.manager");
		if (managerLoc == null) { 
			System.out.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
			System.exit(-1);
		}

		try {
			alarmService = AlarmServiceSingleton.getInstance();
			client = AlarmServiceSingleton.getComponentClientInstance();
			ACSJMSTopicConnectionImpl.containerServices = client.getContainerServices();
			log=client.getContainerServices().getLogger();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * Retrieve the default admin user used in testing. If the user does not
	 * yet exist, it is created.
	 * 
	 * @return The admin user.
	 * 
	 * @throws LaserDefinitionException Error creating admin user.
	 * @throws LaserException Error connecting to alarm server.
	 */
	protected AdminUser getAdmin() throws LaserDefinitionException, LaserException
	{
		if(this.admin == null) {
			AdminUserHandler handler = AdminUserHandler.get();
			try {
				this.admin = handler.loginUser(ADMIN_USERNAME, ADMIN_PASSWORD);
			} catch(LaserDefinitionException e) {
				this.admin = handler.createUser(ADMIN_USERNAME, ADMIN_PASSWORD);
			}
		}
		return this.admin;
	}
	
	private AdminUser admin = null;
}

/* __oOo__ */
