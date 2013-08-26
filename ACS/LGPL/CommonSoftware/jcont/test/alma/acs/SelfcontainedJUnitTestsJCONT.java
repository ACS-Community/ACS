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
package alma.acs;

import junit.framework.Test;
import junit.framework.TestSuite;

import alma.acs.container.CleaningThreadFactoryTest;
import alma.acs.container.ComponentMapTest;
import alma.acs.container.archive.UIDLibraryTest;
import alma.acs.container.corba.AcsCorbaTest;
import alma.acs.container.corba.CorbaNullFinderTest;
import alma.acs.entityutil.EntityRefFinderTest;
import alma.acs.entityutil.EntitySerializerTest;

/**
 * JUnit test suite for all JUnit tests in this module which do not require 
 * any setup outside of JUnit (such as a running ACS), and which do not rely on an external
 * tool like TAT to analyze the command line output. 
 *  
 * @author hsommer
 * created Nov 11, 2003 1:43:55 PM
 */
public class SelfcontainedJUnitTestsJCONT
{

	public static Test suite()
	{
		TestSuite suite = new TestSuite("Tests for ACS module jcont");
		//$JUnit-BEGIN$
        suite.addTestSuite(CleaningThreadFactoryTest.class);
        suite.addTestSuite(ComponentMapTest.class);
		suite.addTestSuite(EntityRefFinderTest.class);
		suite.addTestSuite(EntitySerializerTest.class);
		suite.addTestSuite(AcsCorbaTest.class);
		suite.addTestSuite(UIDLibraryTest.class);
		suite.addTestSuite(CorbaNullFinderTest.class);
		//$JUnit-END$
		return suite;
	}
}
