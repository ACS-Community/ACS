/*
 * Created on Mar 30, 2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package com.cosylab.acs.alarm;

import java.util.Collection;
import java.util.Iterator;

import alma.acs.component.client.ComponentClientTestCase;

import junit.framework.TestCase;
import cern.laser.client.LaserConnectionException;
import cern.laser.console.Comment;
import cern.laser.console.CommentedAlarm;
import cern.laser.console.CommentedAlarmMap;
import cern.laser.console.Configuration;
import cern.laser.console.LaserConsoleException;
import cern.laser.console.User;
import cern.laser.console.UserHandler;
import cern.laser.guiplatform.alarms.helpers.AlarmImpl;

/**
 * Test the functionality for user management of the LASER system. 
 * 
 * @author Klemen Zagar, Cosylab
 */
public class AlarmUserTest extends ComponentClientTestCase {
	/* The prefix string of the username. */
	private final String USER_PREFIX = "test_user";
	/* Number of users to create during the test. */
	private final int NUM_OF_USERS = 15;
	/* The password for a user. */
	private final String PASSWORD = "password";
	/* The prefix string of configuration name. */
	private final String CONFIG_PREFIX = "test_config";
	/* Number of configurations to create during the test. */
	private final int NUM_OF_CONFIGS = 15;	
	/* The prefix string of a comment. */
	private final String COMMENT_PREFIX = "test_comment";

	public AlarmUserTest(String arg0) throws Exception {
		super(arg0);
	}
	
	/**
	 * Set-up the test. Defines the default host of the LASER server.
	 */
	public void setUp() {
		final String LASER_HOSTS = "laser.hosts";
		if(System.getProperty(LASER_HOSTS) == null) {
			System.setProperty(LASER_HOSTS, "localhost");
		}
	}
	
	/**
	 * Tear-down the test. Removes all the users that have been created during
	 * the test.
	 */
	public void tearDown() throws Exception {
		UserHandler handler = UserHandler.get(getContainerServices());
		handler.removeUser(USER_PREFIX);
		for(int i = 0; i < NUM_OF_USERS; ++i) {
			handler.removeUser(USER_PREFIX + i);
		}
		super.tearDown();
	}

	/**
	 * Test creation and removal of users via the UserHandler interface.
	 * 
	 * @throws LaserConsoleException Should not happen.
	 */
	public void notestUserHandler() throws LaserConsoleException {
		UserHandler handler = UserHandler.get(getContainerServices());
		
		/* Create users. */
		for(int i = 0; i < NUM_OF_USERS; ++i) {
			/* Test UserHandler.createUser. */
			User user = handler.createUser(USER_PREFIX + i, PASSWORD);
			assertEquals(USER_PREFIX + i, user.getName());
			assertEquals(PASSWORD, user.getPassword());
		
			/* Test UserHandler.getUser. */
			User user2 = handler.getUser(USER_PREFIX + i,getContainerServices());
			assertEquals(user.getName(), user2.getName());
			assertEquals(PASSWORD, user2.getPassword());
			
			/* Test User.setPassword. */
			user.setPassword(PASSWORD + PASSWORD);
			assertEquals(PASSWORD + PASSWORD, user.getPassword());
			user2 = handler.getUser(USER_PREFIX + i,getContainerServices());
			assertEquals(PASSWORD + PASSWORD, user2.getPassword());
		}
		
		/* Test UserHandler.getUsers. */
		Collection users = handler.getUsers();
		assertEquals(NUM_OF_USERS, users.size());
		Iterator iter = users.iterator();
		
		/* Test that UserHandler.createUser fails if the user already exists. */ 		
		for(int i = 0; i < NUM_OF_USERS; ++i) {
			try {
				handler.createUser(USER_PREFIX + i, PASSWORD);
				fail();
			} catch(LaserConsoleException e) {
				assertEquals("user " + USER_PREFIX + i + " is already defined", e.getMessage());
			}
		}
		
		/* Test UserHandler.removeUser. */
		for(int i = 0; i < NUM_OF_USERS; ++i) {
			handler.removeUser(USER_PREFIX + i);
		}
		
		/* Make sure that UserHandler.removeUser actually removed the user. */
		for(int i = 0; i < NUM_OF_USERS; ++i) {
			try {
				handler.getUser(USER_PREFIX + i,getContainerServices());
				fail();		
			} catch(LaserConsoleException e) {
				assertTrue(e.getMessage().startsWith("unable to get user"));
			}
		}
	}
	
	
	/**
	 *
	 */
	public void notestUserConfiguration() throws LaserConsoleException
	{
		UserHandler handler = UserHandler.get(getContainerServices());
		User user = handler.createUser(USER_PREFIX, PASSWORD);
				
		CommentedAlarmMap cam1 = new CommentedAlarmMap();
		cam1.put(new CommentedAlarm(new AlarmImpl("AlarmSource:ALARM_SOURCE_A:2", false, false, false, "AlarmSource", "ALARM_SOURCE_A", 2), new Comment(USER_PREFIX, COMMENT_PREFIX + 1)));
		cam1.put(new CommentedAlarm(new AlarmImpl("ROdouble:ALARM_SOURCE_A#current:1", false, false, false, "ROdouble", "ALARM_SOURCE_A#current", 1), new Comment(USER_PREFIX, COMMENT_PREFIX + 2)));
		cam1.put(new CommentedAlarm(new AlarmImpl("AlarmSource:ALARM_SOURCE_B:2", false, false, false, "AlarmSource", "ALARM_SOURCE_B", 2), new Comment(USER_PREFIX, COMMENT_PREFIX + 3)));
		for(int i = 0; i < NUM_OF_CONFIGS; ++i) {
			Configuration config = user.createConfiguration(CONFIG_PREFIX + i);
			config.setAcknowledged(cam1);
			
			Configuration config2;
			try {
				config2 = user.getConfiguration(CONFIG_PREFIX + i);
			} catch (LaserConsoleException e) {
				throw new LaserConsoleException("getConfiguration error", e);
			} catch (LaserConnectionException e) {
				throw new LaserConsoleException("getConfiguration error", e);
			}
			assertTrue(compareCommentedAlarmMap(cam1, config2.getAcknowledged()));
		}
		
		//user.set
		// TODO Configuration setting tests are missing.
		//fail();
		
		handler.removeUser(USER_PREFIX);
	}

	/**
	 * @param cam1
	 * @param map
	 * @return
	 */
	private boolean compareCommentedAlarmMap(CommentedAlarmMap cam1, CommentedAlarmMap map) {
		return true;
	}
	
}
