package alma.acs.alarmsystem.test;

import java.util.List;

public class BACIBooleanSeqPropertyTest extends BACITest {

	static final int SLEEP_TIME = 2000;
	
	/**
	 * 
	 * @param title
	 * @throws Exception
	 */
	public BACIBooleanSeqPropertyTest() throws Exception {
		super("BACIBooleanSeqPropertyTest");
	}

	public void testROBooleanSeq() throws Exception {
		setBooleanSeqVarAndWait(new boolean [] {true, true}); // 2 alarms
		setBooleanSeqVarAndWait(new boolean [] {true, false}); // 1 no alarm
		setBooleanSeqVarAndWait(new boolean [] {false, false}); // 1 no alarm
		setBooleanSeqVarAndWait(new boolean [] {true, false}); // 1 alarm
		setBooleanSeqVarAndWait(new boolean [] {true, true}); // 1 alarm
	}
		
	protected void setBooleanSeqVarAndWait(boolean[] value) throws Exception {
		testComponent.setBooleanSeqVar(value);
		try {
			Thread.sleep(SLEEP_TIME);
		} catch(Exception e) {}
	}
}
