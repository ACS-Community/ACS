package alma.acs.commandcenter.app;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import alma.acs.commandcenter.AccTests;

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

	/* $Id: _Tests.java,v 1.1 2005/10/27 13:14:05 mschilli Exp $ */

	public static Test suite() {
		TestSuite ret = new TestSuite("Test for " + _Tests.class.getPackage().getName());

		if (AccTests.hasNativeAcs()) {
			ret.addTestSuite(CommandCenterLogicTest.class);
		}
		
		
		return ret;
	}

	
	
	// ===================================================================
	// =======================  Test Utilities  ==========================
	// ===================================================================
	

	static protected void enter (TestCase x) {
		String testclass = x.getClass().getName();
		testclass = testclass.substring(testclass.lastIndexOf('.')+1);
		String testmethod = x.getName();
		System.err.println("\n=== " + testmethod + " ("+testclass+") ===");
	}


}






