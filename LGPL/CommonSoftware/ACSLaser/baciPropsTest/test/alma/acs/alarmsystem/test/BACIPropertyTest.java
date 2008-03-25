package alma.acs.alarmsystem.test;

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
 * A class to test if baci sends BACI property alarms.
 * 
 * The test is performed by getting TEST_COMPONENT component and executing
 * CORBA calls to change the value of its property. 
 * All the alarms received are written in the stdout and then checked by tat
 * 
 * @author acaproni
 *
 */
public class BACIPropertyTest extends ComponentClientTestCase implements SourceListener, AlarmSelectionListener {

	// The category client
	private CategoryClient categoryClient;
	
	// The source client
	private SourceClient sourceClient;
	
	/**
	 * The component that triggers BACI to send alarms
	 */
	private BaciPropTest testComponent;
	private static final String COMPONENT_NAME = "TEST_COMPONENT";
	
	/**
	 * Constructor
	 */
	public BACIPropertyTest() throws Exception {
		super("BACIPropertyTest");
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
	 * Test the sending of alarms by setting the value of a RODouble property
	 * The limits are defined in the CDB.
	 * 
	 * @throws Exception
	 */
	public void testRODouble() throws Exception {
		// No alarms
		testComponent.setDoubleVar((float)0.0);
		try {
			Thread.sleep(5000);
		} catch (Exception e) {}
		// HIGH ON
		testComponent.setDoubleVar((float)400.0);
		try {
			Thread.sleep(5000);
		} catch (Exception e) {}
		// HIGH OFF
		testComponent.setDoubleVar((float)340.0);
		try {
			Thread.sleep(5000);
		} catch (Exception e) {}
		// HIGH ON
		testComponent.setDoubleVar((float)400.0);
		try {
			Thread.sleep(5000);
		} catch (Exception e) {}
		// HIGH OFF
		testComponent.setDoubleVar((float)0.0);
		try {
			Thread.sleep(5000);
		} catch (Exception e) {}
		// LOW ON
		testComponent.setDoubleVar((float)-200.0);
		try {
			Thread.sleep(5000);
		} catch (Exception e) {}
		// LOW OFF
		testComponent.setDoubleVar((float)-90.0);
		try {
			Thread.sleep(5000);
		} catch (Exception e) {}
		// LOW ON
		testComponent.setDoubleVar((float)-200.0);
		try {
			Thread.sleep(5000);
		} catch (Exception e) {}
		// LOW OFF
		testComponent.setDoubleVar((float)0.0);
		try {
			Thread.sleep(20000);
		} catch (Exception e) {}
	}
	
	/**
	 * Print the alarms received by the sources
	 * 
	 * @see alma.alarmsystem.clients.source.SourceListener#faultStateReceived(cern.laser.source.alarmsysteminterface.FaultState)
	 */
	@Override
	public void faultStateReceived(FaultState faultState) {
		System.out.println("Source alarm received: <"+faultState.getFamily()+", "+faultState.getMember()+", "+faultState.getCode()+"> "+faultState.getDescriptor());
	}

	/**
	 * Print a message for each alarm received by the ASC
	 * 
	 * @see cern.laser.client.services.selection.AlarmSelectionListener#onAlarm(cern.laser.client.data.Alarm)
	 */
	@Override
	public void onAlarm(Alarm alarm) {
		System.out.println("Alarm from categories: <"+alarm.getAlarmId()+"> ACTIVE="+alarm.getStatus().isActive());
	}

	/**
	 * Print a message for each error
	 * 
	 * @see cern.laser.client.services.selection.AlarmSelectionListener#onException(cern.laser.client.services.selection.LaserSelectionException)
	 */
	@Override
	public void onException(LaserSelectionException e) {
		System.out.println("LaserSelectionException: "+e.getMessage());
		e.printStackTrace();
	}

}
