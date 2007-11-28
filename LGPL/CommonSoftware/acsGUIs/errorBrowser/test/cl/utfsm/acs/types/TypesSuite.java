package cl.utfsm.acs.types;
import junit.framework.Test;
import junit.framework.TestSuite;
import cl.utfsm.acs.types.*;


public class TypesSuite
{
        public static Test suite()
        {
                TestSuite suite = new TestSuite("Test for Error Browser and Editor: Types Suite");
                suite.addTestSuite(AcsSimpleTypeTEST.class);
                suite.addTestSuite(AcsAttributeTEST.class);
                suite.addTestSuite(AcsComplexTypeTEST.class);
                suite.addTestSuite(SimpleObjectTEST.class);
                suite.addTestSuite(ComplexObjectTEST.class);
                return suite;
        }
}

