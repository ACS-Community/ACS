package cl.utfsm.samplingSystemUITestCase;

import junit.framework.TestCase;
import cl.utfsm.samplingSystemUI.core.SampDetail;

public class SampDetailTest extends TestCase {

	public void testEquals() throws Exception {
		SampDetail sd1 = new SampDetail("SAMP1","brightness",100,1);
		SampDetail sd2 = new SampDetail("SAMP1","brightness",100,1);
		SampDetail sd3 = new SampDetail("SAMP1","brightness",200,1);
		assertEquals(sd1,sd2);
		assertTrue(sd1.hashCode()==sd2.hashCode());
		assertNotSame(sd2,sd3);
		assertTrue(sd1.getComponent().equals("SAMP1"));
		assertTrue(sd1.getProperty().equals("brightness"));
		assertTrue(sd1.getFrequency()==100);
		assertTrue(sd1.getReportRate()==1);


	}



}
