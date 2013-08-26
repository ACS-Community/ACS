package cl.utfsm.acs.ebe;
import junit.framework.Test;
import junit.framework.TestSuite;


public class EbeSuite
{
	public static Test suite() {
		TestSuite suite = new TestSuite("Test for Error Browser and Editor: EBE classes");
		suite.addTestSuite(ErrorSchemaTEST.class);
		suite.addTestSuite(EbeDocumentTEST.class);
		suite.addTestSuite(EbeDocumentManagerTEST.class);
		return suite;
	}
}
