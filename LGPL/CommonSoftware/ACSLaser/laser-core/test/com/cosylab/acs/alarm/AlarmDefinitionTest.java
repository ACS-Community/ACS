/*
 * Created on Mar 30, 2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package com.cosylab.acs.alarm;

import java.util.GregorianCalendar;

import cern.laser.business.definition.data.AlarmDefinition;
import cern.laser.business.definition.data.CategoryDefinition;
import cern.laser.business.definition.data.CategoryLink;
import cern.laser.business.definition.data.MaintenanceMask;
import cern.laser.business.definition.data.ModeMask;
import cern.laser.business.definition.data.ReductionLink;
import cern.laser.business.definition.data.SourceDefinition;
import cern.laser.client.LaserException;
import cern.laser.definition.AdminUser;
import cern.laser.definition.AdminUserHandler;
import cern.laser.definition.AlarmDefinitionHandler;
import cern.laser.definition.CategoryDefinitionHandler;
import cern.laser.definition.CategoryLinkDefinitionHandler;
import cern.laser.definition.LaserDefinitionException;
import cern.laser.definition.ReductionMaskDefinitionHandler;
import cern.laser.definition.SourceDefinitionHandler;

/**
 * @author kzagar
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class AlarmDefinitionTest extends AlarmTestBase {	
	private static final String DESCRIPTION =
		/* 0 */ "0123456789ABCDEF" +
		/* 1 */ "0123456789ABCDEF" +
		/* 2 */ "0123456789ABCDEF" +
		/* 3 */ "0123456789ABCDEF" +
		/* 4 */ "0123456789ABCDEF" +
		/* 5 */ "0123456789ABCDEF" +
		/* 6 */ "0123456789ABCDEF" +
		/* 7 */ "0123456789ABCDEF" +
		/* 8 */ "0123456789ABCDEF" +
		/* 9 */ "0123456789ABCDEF" +
		/* A */ "0123456789ABCDEF" +
		/* B */ "0123456789ABCDEF" +
		/* C */ "0123456789ABCDEF" +
		/* D */ "0123456789ABCDEF" +
		/* E */ "0123456789ABCDEF" +
		/* F */ "0123456789ABCDEF";
	
	/**
	 * Test the AdminUserHandler implementation. The implementation should be
	 * capable of creating, logging-in and removing of admin users.
	 * 
	 * @throws LaserDefinitionException
	 * @throws LaserException
	 */
	public void notestAdminUserHandler() throws LaserDefinitionException, LaserException {
		final String ADMIN_USERNAME = "test_admin_1";
		final String ADMIN_PASSWORD = "drowssap";
		AdminUserHandler handler = AdminUserHandler.get();
		AdminUser user = handler.createUser(ADMIN_USERNAME, ADMIN_PASSWORD);
		assertNotNull(user);
		
		AdminUser user2 = handler.loginUser(ADMIN_USERNAME, ADMIN_PASSWORD);
		assertNotNull(user2);
		
		try {
			handler.createUser(ADMIN_USERNAME, ADMIN_PASSWORD);
			fail();
		} catch(LaserDefinitionException e) {
			assertEquals("unable to create user : " + ADMIN_USERNAME , e.getMessage());
		}
		
		// This test can not be performed because admin user removal does not
		// work. Most probably the EJB container is not configured properly,
		// yielding exception 'Error saving state: No value specified for
		// parameter 2'.
		
		handler.removeUser(ADMIN_USERNAME);
		
		try {
			handler.loginUser(ADMIN_USERNAME, ADMIN_PASSWORD);
			fail();		
		} catch(LaserDefinitionException e) {
			assertTrue(e.getMessage().startsWith("unable to get user"));
		}
	}
	
	/**
	 * Test the CategoryDefinitionHandler implementation. 
	 * @throws LaserDefinitionException
	 */
	public void notestCategoryDefinitionHandler() throws LaserDefinitionException, LaserException
	{
		final String CATEGORY_NAME="CERN.TEST_CATEGORY";
		
		CategoryDefinitionHandler handler = getAdmin().getCategoryDefinitionHandler();

		CategoryDefinition category = new CategoryDefinition(CATEGORY_NAME, DESCRIPTION);
		handler.createCategory(category);

		// TODO: check whether user categories are as they should be
		//Collection c = getAdmin().getUserCategories();
		//assertEquals(NUM_OF_CATEGORIES, c.size());
				
		//category = new CategoryDefinition(CATEGORY_NAME);
		//handler.removeCategory(category);
	}

	public void notestAlarmDefinitionHandler() throws LaserDefinitionException, LaserException
	{
		final String FAULT_FAMILY = "TEST_FAMILY";
		final String FAULT_MEMBER = "TEST_MEMBER";
		final String SYSTEM_NAME = "System Name";
		final String IDENTIFIER = "ID";
		final String CAUSE = "Cause";
		final String ACTION = "Action";
		final String CONSEQUENCE = "Consequence";
		final String HELP_URL = "http://tempuri.org/help";
		final String SOURCE_NAME = "TEST_SOURCE";
		final String BUILDING = "Building";
		final String FLOOR = "Floor";
		final String ROOM = "Room";
		final String MNEMONIC = "Mnemonic";
		final String POSITION = "Position";
		final String GSM = "01234567";
		final String EMAIL = "someone@somewhere.com"; 
		
		final Integer FAULT_CODE = new Integer(1234);
		final Integer PRIORITY = new Integer(5678);
		final Integer RESPONSIBLE_PERSON_ID = null;
		
		final Boolean INSTANT = new Boolean(true);

		SourceDefinitionHandler sdh = getAdmin().getSourceDefinitionHandler();

		SourceDefinition sd = new SourceDefinition(SOURCE_NAME, DESCRIPTION, "hostName", new Integer(1000), new Integer(1));
		sdh.createSource(sd);

		// TODO: check whether user sources are as they should be
		//Collection sources = getAdmin().getUserSources();
		//assertEquals(NUM_OF_SOURCES, sources.size());
		
		AlarmDefinitionHandler adh = getAdmin().getAlarmDefinitionHandler();
		
		AlarmDefinition ad = new AlarmDefinition(
			FAULT_FAMILY, 
			FAULT_MEMBER, 
			FAULT_CODE,
			SYSTEM_NAME,
			IDENTIFIER,
			DESCRIPTION,
			PRIORITY,
			CAUSE,
			ACTION,
			CONSEQUENCE,
			INSTANT,
			HELP_URL,
			SOURCE_NAME,
			BUILDING,
			FLOOR, 
			ROOM,
			MNEMONIC,
			POSITION,
			RESPONSIBLE_PERSON_ID,
			GSM,
			EMAIL);
		adh.createAlarm(ad);
		
		// TODO: check whether user sources are as they should be
		//Collection sources = getAdmin().getUserSources();
		//assertEquals(NUM_OF_SOURCES, sources.size());

		//ad = new AlarmDefinition(FAULT_FAMILY, FAULT_MEMBER, FAULT_CODE);
		//adh.removeAlarm(ad);

		//sd = new SourceDefinition(SOURCE_NAME);
		//sdh.removeSource(sd);
	}

	public void notestCategoryLinkDefinitionHandler() throws LaserDefinitionException, LaserException
	{
		final String FAULT_FAMILY = "TEST_FAMILY_1";
		final String FAULT_MEMBER = "TEST_MEMBER_1";
		final String SOURCE_NAME = "TEST_SOURCE";

		final int FAULT_CODE = 1235;

		final String CATEGORY_NAME = "CERN.TEST_CATEGORY_1";
				
		AlarmDefinitionHandler adh = getAdmin().getAlarmDefinitionHandler();
		CategoryDefinitionHandler cdh = getAdmin().getCategoryDefinitionHandler();
		CategoryLinkDefinitionHandler clh = getAdmin().getCategoryLinkDefinitionHandler();
		
		AlarmDefinition ad = createAlarm(FAULT_FAMILY, FAULT_MEMBER, FAULT_CODE, SOURCE_NAME);
		adh.createAlarm(ad);
		
		CategoryDefinition cd = new CategoryDefinition(CATEGORY_NAME);
		cdh.createCategory(cd);
		
		CategoryLink cl = new CategoryLink(cd, ad);
		clh.createCategoryLink(cl);
		
		// TODO: check whether user sources are as they should be
		//Collection sources = getAdmin().getUserSources();
		//assertEquals(NUM_OF_SOURCES, sources.size());

		//adh.removeAlarm(ad);
		//cdh.removeCategory(cd);
	}

	public void notestReductionMaskHandler() throws LaserDefinitionException, LaserException
	{
		final String SOURCE_NAME = "TEST_SOURCE";

		AlarmDefinition ad1 = createAlarm("TEST_ALARM_A", "MemberA", 1300, SOURCE_NAME);
		AlarmDefinition ad2 = createAlarm("TEST_ALARM_B", "MemberB", 1301, SOURCE_NAME);
		AlarmDefinition ad3 = createAlarm("TEST_ALARM_C", "MemberC", 1302, SOURCE_NAME);
		
		AlarmDefinitionHandler adh = getAdmin().getAlarmDefinitionHandler();
		adh.createAlarm(ad1);
		adh.createAlarm(ad2);
		adh.createAlarm(ad3);
		
		ReductionMaskDefinitionHandler rmdh = getAdmin().getReductionMaskDefinitionHandler();
		ReductionLink rl1 = new ReductionLink(ad1, ad2);
		ReductionLink rl2 = new ReductionLink(ad1, ad3);
		rmdh.createNodeLink(rl1);
		rmdh.createMultiplicityLink(rl2);
		
		rmdh.addMaintenanceMask(ad2, new MaintenanceMask(
			new GregorianCalendar(2004, 06, 10).getTime(),
			new GregorianCalendar(2004, 07, 10).getTime()
		));
		
		rmdh.addModeMask(ad3, new ModeMask("MODE_A"));
	}
	
	private AlarmDefinition createAlarm(String faultFamily, String faultMember, int faultCode, String sourceName)
	{
		final String SYSTEM_NAME = "System Name";
		final String IDENTIFIER = "ID";
		final String CAUSE = "Cause";
		final String ACTION = "Action";
		final String CONSEQUENCE = "Consequence";
		final String HELP_URL = "http://tempuri.org/help";
		final String BUILDING = "Building";
		final String FLOOR = "Floor";
		final String ROOM = "Room";
		final String MNEMONIC = "Mnemonic";
		final String POSITION = "Position";
		final String GSM = "01234567";
		final String EMAIL = "someone@somewhere.com"; 
		
		final Integer PRIORITY = new Integer(5678);
		final Integer RESPONSIBLE_PERSON_ID = null;
		
		final Boolean INSTANT = new Boolean(true);
		
		return new AlarmDefinition(
					faultFamily, 
					faultMember, 
					new Integer(faultCode),
					SYSTEM_NAME,
					IDENTIFIER,
					DESCRIPTION,
					PRIORITY,
					CAUSE,
					ACTION,
					CONSEQUENCE,
					INSTANT,
					HELP_URL,
					sourceName,
					BUILDING,
					FLOOR, 
					ROOM,
					MNEMONIC,
					POSITION,
					RESPONSIBLE_PERSON_ID,
					GSM,
					EMAIL);
	}
}
