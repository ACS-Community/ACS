package alma.acs.tmcdb;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllPojoTests {

	public static Test suite() {

		TestSuite suite = new TestSuite("Test for auto-generated Java pojo files");
		suite.addTestSuite( TestPojosInitialization.class );
		suite.addTestSuite( TestPojosPublishingMechanism.class );
		suite.addTestSuite( TestPojosPersistence.class );
		suite.addTestSuite( TestPojosCascading.class );
		suite.addTestSuite( TestSnmpTables.class );
		return suite;
	}
}