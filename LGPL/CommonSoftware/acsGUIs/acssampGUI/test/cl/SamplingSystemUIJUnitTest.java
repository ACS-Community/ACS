package cl;
import junit.framework.Test;
import junit.framework.TestSuite;
import cl.utfsm.samplingSystemUITestCase.*;


public class SamplingSystemUIJUnitTest 
{
	public static Test suite() 
	{
		TestSuite suite = new TestSuite("Test for ACS Sampling System UI");
		suite.addTestSuite(AcsInformationTest.class);
		suite.addTestSuite(ComponentsManagerTest.class);
		suite.addTestSuite(SampDetailTest.class);
		suite.addTestSuite(SamplingManagerTest.class);
		suite.addTestSuite(ThreadCommunicatorTest.class);
		suite.addTestSuite(DataItemTest.class);
		return suite;
	}
}
