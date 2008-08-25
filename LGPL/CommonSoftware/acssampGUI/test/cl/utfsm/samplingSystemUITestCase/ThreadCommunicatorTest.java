package cl.utfsm.samplingSystemUITestCase;

import junit.framework.TestCase;
import cl.utfsm.samplingSystemUI.core.ThreadCommunicator;
import java.util.concurrent.LinkedBlockingQueue;

public class ThreadCommunicatorTest extends TestCase {


	public void testSingleton() throws Exception {
		ThreadCommunicator tc1=null;
		ThreadCommunicator tc2=null;
		tc1 = ThreadCommunicator.getInstance();
		tc2 = ThreadCommunicator.getInstance();
		assertNotNull(tc1);
		assertNotNull(tc2);
		assertEquals(tc1,tc2);
	}

	public void testConsistency() throws Exception {
		ThreadCommunicator tc1= ThreadCommunicator.getInstance();
		LinkedBlockingQueue tvn = tc1.createChannel("TVN");
		LinkedBlockingQueue chv = tc1.createChannel("CHV");
		assertEquals(tvn,tc1.getChannel("TVN"));
		assertEquals(chv,tc1.getChannel("CHV"));
		assertNotSame(tvn,chv);
	}

	/* public void testDuplicate(){
		ThreadCommunicator tc1= ThreadCommunicator.getInstance();
		tc1.removeChannel("TVN");
               	LinkedBlockingQueue tvn = tc1.createChannel("TVN");
		try {
			LinkedBlockingQueue chv = tc1.createChannel("TVN");
			fail("Duplicate channel must be avoided");

		}
		catch(IllegalArgumentException e){
			assertTrue("Duplicate channel launched exception",true);
			return;
		}
	}*/

	public void testDeletion() throws Exception {
		ThreadCommunicator tc1= ThreadCommunicator.getInstance();
		tc1.removeChannel("TVN");
		tc1.createChannel("TVN");
		tc1.removeChannel("TVN");
		assertNull(tc1.getChannel("TVN"));
	}

}
