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
package alma.alarmsystem.clients.test;

import cern.laser.client.data.Alarm;
import cern.laser.client.services.selection.AlarmSelectionListener;
import cern.laser.client.services.selection.LaserSelectionException;
import alma.acs.component.client.ComponentClientTestCase;
import alma.alarmsystem.clients.CategoryClient;

/**
 * Test the getThreshold method of the Category client.
 * 
 * Each method of the Category client needs to be tested in a separate class due to a bug in the NC 
 * (COMP-2153)
 * 
 * @author acaproni
 *
 */
public class CategoryClientThreshold extends ComponentClientTestCase implements AlarmSelectionListener {
	
	/**
	 *  The categoryClient to test
	 */
	private CategoryClient categoryClient;

	/**
	 * Constructor
	 */
	public CategoryClientThreshold() throws Exception {
		super(CategoryClientThreshold.class.getName());
	}
	
	/**
	 * @see extends ComponentClientTestCase
	 */
	public void setUp() throws Exception {
		super.setUp();
		
		categoryClient = new CategoryClient(getContainerServices());
		assertNotNull(categoryClient);
	}
	
	/**
	 * @see extends ComponentClientTestCase
	 */
	public void tearDown() throws Exception {
		categoryClient.close();
		super.tearDown();
	}
	
	public void testGetThreashold() throws Exception {
		categoryClient.connect(this);
		int threshold = categoryClient.getMultiplicityThreshold("TEST:MCAUSE:1");
		assertEquals(3, threshold);
	}

	/**
	 * @see cern.laser.client.services.selection.AlarmSelectionListener#onAlarm(cern.laser.client.data.Alarm)
	 */
	@Override
	public void onAlarm(Alarm alarm) {	}

	/**
	 * @see cern.laser.client.services.selection.AlarmSelectionListener#onException(cern.laser.client.services.selection.LaserSelectionException)
	 */
	@Override
	public void onException(LaserSelectionException e) {}
}
