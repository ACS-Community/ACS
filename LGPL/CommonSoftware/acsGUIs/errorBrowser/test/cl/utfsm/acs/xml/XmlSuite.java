package cl.utfsm.acs.xml;
import junit.framework.Test;
import junit.framework.TestSuite;
import cl.utfsm.acs.xml.*;


public class XmlSuite
{
        public static Test suite()
        {
                TestSuite suite = new TestSuite("Test for Error Browser and Editor: XML classes");
                suite.addTestSuite(XmlSeekerTEST.class);
                suite.addTestSuite(CommonSchemaTEST.class);
                return suite;
        }
}

