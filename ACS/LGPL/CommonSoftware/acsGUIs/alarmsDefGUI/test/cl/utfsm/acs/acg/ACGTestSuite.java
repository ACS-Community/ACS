/*
 *    ALMA - Atacama Large Millimeter Array
 *    (c) Universidad Tecnica Federico Santa Maria, 2009
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
package cl.utfsm.acs.acg;

import junit.framework.Test;
import junit.framework.TestSuite;
import cl.utfsm.acs.acg.core.*;
import cl.utfsm.acs.acg.dao.*;

public class ACGTestSuite {

	public static Test suite() 
	{
		TestSuite suite = new TestSuite("Test for Alarm Configuration GUI core components");
		suite.addTestSuite(AcsInformationTest.class);
		suite.addTestSuite(DAOManagerTest.class);
		suite.addTestSuite(UserAuthenticatorTest.class);
		suite.addTestSuite(ReductionRuleTest.class);
		suite.addTestSuite(AlarmSystemManagerTest.class);
		suite.addTestSuite(CategoryManagerTest.class);
		suite.addTestSuite(AlarmManagerTest.class);
		suite.addTestSuite(ReductionManagerTest.class);
		suite.addTestSuite(SourceManagerTest.class);
		suite.addTestSuite(ACSAlarmDAOImplTest.class);
		suite.addTestSuite(ACSCategoryDAOImplTest.class);
		suite.addTestSuite(ACSAlarmSystemDAOImplTest.class);
		return suite;
	}
}
