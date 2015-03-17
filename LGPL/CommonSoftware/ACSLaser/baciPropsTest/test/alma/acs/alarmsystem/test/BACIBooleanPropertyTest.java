package alma.acs.alarmsystem.test;

import java.util.List;

public class BACIBooleanPropertyTest extends BACITest {

	static final int SLEEP_TIME = 2000;
	
	/**
	 * 
	 * @param title
	 * @throws Exception
	 */
	public BACIBooleanPropertyTest() throws Exception {
		super("BACIBooleanPropertyTest");
	}

	public void testROboolean() throws Exception {
		setBooleanVarAndWait(true); // Alarm
		setBooleanVarAndWait(false); // No alarm
		setBooleanVarAndWait(true); // Alarm
		setBooleanVarAndWait(true);
		setBooleanVarAndWait(false); // No alarm
		setBooleanVarAndWait(false);
		setBooleanVarAndWait(true); // Alarm
		setBooleanVarAndWait(false); // No alarm
		
		setAnotherBooleanVarAndWait(false); // Alarm
		setAnotherBooleanVarAndWait(true); // No alarm
		setAnotherBooleanVarAndWait(false); // Alarm
		setAnotherBooleanVarAndWait(false); 
		setAnotherBooleanVarAndWait(true); // No alarm
		setAnotherBooleanVarAndWait(true);
		setAnotherBooleanVarAndWait(false); // Alarm
		setAnotherBooleanVarAndWait(true); // No alarm
		
		setBooleanNoAlarmsVarAndWait(true); // Alarm
		setBooleanNoAlarmsVarAndWait(false); // No alarm
		setBooleanNoAlarmsVarAndWait(true); // Alarm
		setBooleanNoAlarmsVarAndWait(true); 
		setBooleanNoAlarmsVarAndWait(false); // No alarm
		setBooleanNoAlarmsVarAndWait(false); 
		setBooleanNoAlarmsVarAndWait(true); // Alarm
		setBooleanNoAlarmsVarAndWait(false); // No alarm
	}
	
	protected void setBooleanVarAndWait(boolean value) throws Exception {
		testComponent.setBooleanVar(value);
		try {
			Thread.sleep(SLEEP_TIME);
		} catch(Exception e) {}
	}
	
	protected void setAnotherBooleanVarAndWait(boolean value) throws Exception {
		testComponent.setAnotherBooleanVar(value);
		try {
			Thread.sleep(SLEEP_TIME);
		} catch(Exception e) {}
	}
	
	protected void setBooleanNoAlarmsVarAndWait(boolean value) throws Exception {
		testComponent.setBooleanNoAlarmsVar(value);
		try {
			Thread.sleep(SLEEP_TIME);
		} catch(Exception e) {}
	}
}
