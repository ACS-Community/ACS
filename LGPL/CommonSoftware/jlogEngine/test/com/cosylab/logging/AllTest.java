/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package com.cosylab.logging;

import junit.framework.*;
/**
 * AllTests extens a suite which by definition can automatically 
 * integrate and execute multiple tests.
 * Creation date: (10/24/2001 11:30:52 AM)
 * @author: 
 */

public class AllTest extends junit.framework.TestSuite {
/**
 * AllTest constructor comment.
 */
public AllTest() {
	super();
}
/**
 * AllTest constructor comment.
 * @param theClass java.lang.Class
 */
public AllTest(Class theClass) {
	super(theClass);
}
/**
 * AllTest constructor comment.
 * @param name java.lang.String
 */
public AllTest(String name) {
	super(name);
}


public static void main(String args[]) {
	int count = args.length;
	if (count!= 0 && args[0] == "gui")
        junit.swingui.TestRunner.run(AllTest.class);
    else
    	 junit.textui.TestRunner.run(suite());
    System.exit(0);
    }

public static TestSuite suite() {
	TestSuite suite = new TestSuite();
	suite.addTest(new TestSuite(LogEntryTest.class));
	suite.addTest(new TestSuite(CacheTest.class));
	return suite;
}
}
