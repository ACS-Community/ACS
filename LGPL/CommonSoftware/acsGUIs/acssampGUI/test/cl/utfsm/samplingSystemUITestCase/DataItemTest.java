package cl.utfsm.samplingSystemUITestCase;

import junit.framework.TestCase;
import cl.utfsm.samplingSystemUI.core.DataItem;

public class DataItemTest extends TestCase {

	public void testEquals() throws Exception {
		DataItem sd1 = new DataItem(100,3.14);
		DataItem sd2 = new DataItem(100,3.14);
		DataItem sd3 = new DataItem(117,2.70);
		assertEquals(sd1,sd2);
		assertTrue(sd1.hashCode()==sd2.hashCode());
		assertNotSame(sd2,sd3);
		assertTrue(sd1.getTime()==100);
		assertTrue(sd1.getValue()==3.14);
	}



}
