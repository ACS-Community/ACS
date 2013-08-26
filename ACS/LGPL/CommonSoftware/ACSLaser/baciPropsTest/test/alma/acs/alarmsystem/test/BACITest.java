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
package alma.acs.alarmsystem.test;

import java.util.logging.Level;

import org.omg.CORBA.Object;

import cern.laser.client.data.Alarm;
import cern.laser.client.services.selection.AlarmSelectionListener;
import cern.laser.client.services.selection.LaserSelectionException;
import cern.laser.source.alarmsysteminterface.FaultState;

import alma.acs.component.client.ComponentClientTestCase;
import alma.alarmsystem.clients.CategoryClient;
import alma.alarmsystem.clients.SourceClient;
import alma.alarmsystem.clients.source.SourceListener;
import alma.alarmsystemPropTest.BaciPropTest;
import alma.alarmsystemPropTest.BaciPropTestHelper;

/**
 * Base class for testing property alarms sent by BACI
 * 
 * @author acaproni
 *
 */
public class BACITest extends ComponentClientTestCase implements SourceListener, AlarmSelectionListener {
	// The category client
	private CategoryClient categoryClient;
	
	// The source client
	private SourceClient sourceClient;
	
	/**
	 * The component that triggers BACI to send alarms
	 */
	protected BaciPropTest testComponent;
	private static final String COMPONENT_NAME = "TEST_COMPONENT";
	
	/**
	 * Constructor
	 */
	public BACITest(String title) throws Exception {
		super("BACITest:"+title);
	}

	/**
	 * @see alma.acs.component.client.ComponentClientTestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		// Check the container services... just in case ;-)
		assertNotNull(getContainerServices());
		// Instantiate and connect the source client
		sourceClient = new SourceClient(getContainerServices());
		assertNotNull(sourceClient);
		sourceClient.addAlarmListener(this);
		sourceClient.connect();
		// Instantiate and connect the category client
		categoryClient = new CategoryClient(getContainerServices());
		assertNotNull(categoryClient);
		categoryClient.connect(this);
		// Get the component
		getTestComponent();
	}

	/**
	 * @see alma.acs.component.client.ComponentClientTestCase#tearDown()
	 */
	@SuppressWarnings("deprecation")
	@Override
	protected void tearDown() throws Exception {
		// Close the source client
		if (sourceClient!=null) {
			sourceClient.close();
			sourceClient=null;
		}
		// Close the category client
		if (categoryClient!=null) {
			categoryClient.close();
			categoryClient=null;
		}
		// release the component
		if (testComponent!=null) {		
			getContainerServices().releaseComponent(COMPONENT_NAME);
			testComponent=null;
		}
		super.tearDown();
	}
	
	/**
	 * Get the test component
	 */
	private void getTestComponent() throws Exception {
		Object obj = this.getContainerServices().getComponent(COMPONENT_NAME);
		assertNotNull(obj);
		testComponent=BaciPropTestHelper.narrow(obj);
		assertNotNull(testComponent);
	}
	
	/**
	 * Print the alarms received by the sources
	 * 
	 * @see alma.alarmsystem.clients.source.SourceListener#faultStateReceived(cern.laser.source.alarmsysteminterface.FaultState)
	 */
	@Override
	public void faultStateReceived(FaultState faultState) {
		m_logger.info("Source alarm received: <"+faultState.getFamily()+", "+faultState.getMember()+", "+faultState.getCode()+"> "+faultState.getDescriptor());
	}

	/**
	 * Print a message for each alarm received by the ASC
	 * 
	 * @see cern.laser.client.services.selection.AlarmSelectionListener#onAlarm(cern.laser.client.data.Alarm)
	 */
	@Override
	public void onAlarm(Alarm alarm) {
		m_logger.info("Alarm from categories: <"+alarm.getAlarmId()+"> ACTIVE="+alarm.getStatus().isActive());
	}

	/**
	 * Print a message for each error
	 * 
	 * @see cern.laser.client.services.selection.AlarmSelectionListener#onException(cern.laser.client.services.selection.LaserSelectionException)
	 */
	@Override
	public void onException(LaserSelectionException e) {
		m_logger.log(Level.WARNING, "LaserSelectionException: ", e);
	}

	/**
	 * @see alma.alarmsystem.clients.source.SourceListener#sourceXMLMsgReceived(java.lang.String)
	 */
	@Override
	public void sourceXMLMsgReceived(String asiMessage) {}
}
