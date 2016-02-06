/*
 * Created by mschilli on Dec 20, 2004
 */
package alma.acs.commandcenter.util;

import junit.framework.Test;
import junit.framework.TestSuite;

/**
 * A TestSuite holding 
 * <ul>
 * <li>various UnitTests 
 * <li>other TestSuites
 * <li>Utilities for those tests
 * </ul>
 * 
 * @author mschilli
 */
public class _Tests {


	public static Test suite() {
		TestSuite ret = new TestSuite("Test for " + _Tests.class.getPackage().getName());

		ret.addTestSuite(UtilsTest.class);
		
		return ret;
	}

}