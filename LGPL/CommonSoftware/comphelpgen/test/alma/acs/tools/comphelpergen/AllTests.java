/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
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
package alma.acs.tools.comphelpergen;

import junit.framework.Test;
import junit.framework.TestSuite;
/**
 * @author rgeorgie
 * AllTests extends a suite which by definition can automatically 
 * integrate and execute multiple tests.
 */

public class AllTests extends junit.framework.TestSuite
{
	public AllTests()
	{
		super();
	}

	public AllTests(Class theClass)
	{
		super(theClass);
	}

	public AllTests(String name)
	{
		super(name);
	}

	public static Test suite()
	{
		TestSuite suite = new TestSuite();
		
		suite.addTest(new TestSuite(CompHelperGeneratorTest.class));
	
		return suite;
	}
}

