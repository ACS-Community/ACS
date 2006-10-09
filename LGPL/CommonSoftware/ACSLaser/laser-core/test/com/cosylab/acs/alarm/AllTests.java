/*
 * Created on Mar 30, 2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package com.cosylab.acs.alarm;

import junit.framework.Test;
import junit.framework.TestSuite;

/**
 * @author kzagar
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for com.cosylab.acs.alarm");
		//$JUnit-BEGIN$
		//suite.addTest(new TestSuite(AlarmSourceTest.class));
		suite.addTest(new TestSuite(ACSCategoryDAOTest.class));
		suite.addTest(new TestSuite(AlarmListener.class));
		//$JUnit-END$
//		suite.addTest(new TestSuite(AlarmUserTest.class));
//		suite.addTest(new TestSuite(AlarmDefinitionTest.class));
		return suite;
	}
}
