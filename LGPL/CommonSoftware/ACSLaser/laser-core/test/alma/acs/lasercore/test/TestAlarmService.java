/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 *    MA 02111-1307  USA
 */
package alma.acs.lasercore.test;

import junit.framework.TestCase;
import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.container.ContainerServices;
import alma.alarmsystem.CERNAlarmService;
import alma.alarmsystem.Category;
import alma.alarmsystem.Source;
import alma.alarmsystem.corbaservice.CernAlarmServiceUtils;
import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;

/**
 * Test the alarm service component by calling its IDL methods
 * 
 * @author acaproni
 *
 */
public class TestAlarmService extends ComponentClientTestCase {

	/**
	 * The AS component
	 */
	private CERNAlarmService alarmService;
	
	/**
	 * Container services
	 */
	private ContainerServices contSvcs;
	
	/**
	 * An helper class
	 */
	private CernAlarmServiceUtils utils;
	
	/**
	 * Constructor 
	 * 
	 * @throws Exception
	 */
	public TestAlarmService() throws Exception {
		super("TestAlarmService");
	}
	
	/**
	 * @see TestCase
	 */
	public void setUp() throws Exception {
		super.setUp();
		
		contSvcs = getContainerServices();
		assertNotNull(contSvcs);
		
		utils = new CernAlarmServiceUtils(contSvcs);
		assertNotNull(utils);
		
		// Get the AS 
		alarmService =utils.getCernAlarmService();
		assertNotNull(alarmService);
	}
	
	/**
	 * @see TestCase
	 */
	public void tearDown() throws Exception {
		
		alarmService=null;
		
		super.tearDown();
	}
	
	/**
	 * Test AlarmSerrvice's getCategories
	 * 
	 * @throws Exception
	 */
	public void testGetCategories() throws Exception {
		
		Category[] categories = alarmService.getCategories();
		assertNotNull(categories);
		
		// CAT1, CAT2 and ROOT
		assertEquals(3,categories.length);
		
		for (Category cat: categories) {
			if (!cat.name.equals("ROOT") && !cat.name.equals("CATEGORY1") && !cat.name.equals("CATEGORY2")) {
				throw new Exception("Inavlid name of category: "+cat.name);
			}
			assertEquals(cat.name, cat.path);
		}
	}
	
	/**
	 * Check if the category root is OK
	 * 
	 * @throws Exception
	 */
	public void testGetCategoryRoot() throws Exception {
		Category root=alarmService.getCategoryTreeRoot();
		assertNotNull(root);
		
		assertEquals("ROOT", root.name);
		assertEquals("ROOT", root.path);
		assertFalse("ROOT is a leaf!", root.leaf);
	}
	
	/**
	 * Test if the sources returned by the component are as expected
	 * 
	 */
	public void testSources() throws Exception {
		Source[] sources = alarmService.getSources();
		
		assertNotNull(sources);
		
		assertEquals(1, sources.length);
		Source src =sources[0];
		assertEquals("ALARM_SYSTEM_SOURCES", src.sourceId);
	}
}
