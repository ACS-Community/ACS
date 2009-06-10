package cl.utfsm.samplingSystemUITestCase;

import junit.framework.TestCase;
import cl.utfsm.samplingSystemUI.core.SamplingManager;
import cl.utfsm.samplingSystemUI.core.SampDetail;
import cl.utfsm.samplingSystemUI.core.AcsInformation;
import cl.utfsm.samplingSystemUI.core.SamplingManagerException;
import alma.ACSErrTypeCommon.CouldntAccessComponentEx;
import alma.ACSErrTypeCommon.TypeNotSupportedEx;
import alma.acssamp.Samp;
import alma.acssamp.SampObj;

public class SamplingManagerTest extends TestCase {


	protected void setUp() throws Exception {
		AcsInformation.getInstance("SamplingManagerTest");
	}

	public void testSingleton() throws Exception {
		SamplingManager  man1=null;
		SamplingManager  man2=null;
		man1 = SamplingManager.getInstance("SAMP1");
		man2 = SamplingManager.getInstance("SAMP1");
		assertNotNull(man1);
		assertNotNull(man2);
		assertEquals(man1,man2);
		
		man2 = SamplingManager.getInstance("SAMP2");
		assertNotNull(man2);
		assertNotSame(man1, man2);
		man1 = SamplingManager.getInstance("SAMP2");
		assertNotNull(man1);
		assertEquals(man1, man2);
		
		man1 = SamplingManager.getInstance();
		assertNotNull(man1);
		assertEquals(man1, man2);
		
		boolean exception = false;
		try {
			man1 = SamplingManager.getInstance("SAMP3");
		} catch (SamplingManagerException e) {
			exception = true;
		}
		assertTrue(exception);
	}

	public void testGetSamplingObj() throws Exception {
		SamplingManager man1 = null;
		SamplingManager man2 = null;
		Samp sampManager1 = null;
		Samp sampManager2 = null;
		
		man1 = SamplingManager.getInstance("SAMP1");
		assertNotNull(man1);
		sampManager1 = man1.getSampReference();
		assertNotNull(sampManager1);
		sampManager2 = man1.getSampReference();
		assertNotNull(sampManager2);
		assertEquals(sampManager1, sampManager2);
		
		man2 = SamplingManager.getInstance("SAMP2");
		assertNotNull(man2);
		sampManager1 = man2.getSampReference();
		assertNotNull(sampManager1);
		sampManager2 = man2.getSampReference();
		assertNotNull(sampManager2);
		assertEquals(sampManager1, sampManager2);
		
		sampManager1 = man1.getSampReference();
		sampManager2 = man2.getSampReference();
		assertNotSame(sampManager1, sampManager2);
	}

	public void testGetSampObj() throws Exception {
		SamplingManager  man1=null;
		SampDetail sDetail1 = new SampDetail("LAMP1","brightness",100,1);
		SampDetail sDetail2 = new SampDetail("LAMP1","brightness",200,1);
		SampObj temp;
		man1 = SamplingManager.getInstance("SAMP1");
		temp = man1.getSamplingObj(sDetail1);
		assertNotNull(temp);
		assertNotNull(man1.getSamplingObj(sDetail2));
		assertEquals(man1.getSamplingObj(sDetail1),temp);
		assertEquals(man1.getSamplingObj(sDetail1),man1.getSamplingObj(sDetail1));
		assertNotSame(man1.getSamplingObj(sDetail2),temp);
	}
	
	public void testGetSampObjExceptions() throws Exception {
		
		SamplingManager man1 = SamplingManager.getInstance("SAMP1");
		SampObj temp = null;
		boolean exceptionCaught;
		
		exceptionCaught = false;
		try {
			temp = man1.getSamplingObj(new SampDetail("SAMPLED4","my_ROdouble",100,1));
		} catch (CouldntAccessComponentEx e) {
			exceptionCaught = true;
		}
		assertTrue(exceptionCaught);
		
		exceptionCaught = false;
		try {
			temp = man1.getSamplingObj(new SampDetail("SAMPLED1","my_ROstring",100,1));
		} catch(TypeNotSupportedEx e) {
			exceptionCaught = true;
		}
		assertTrue(exceptionCaught);
		
		exceptionCaught = false;
		try {
			temp = man1.getSamplingObj(new SampDetail("SAMPLED2","my_RWdouble",100,1));
		} catch(Exception e) {
			exceptionCaught = true;
		}
		assertTrue(!exceptionCaught);
		assertNotNull(temp);
		
	}

}
