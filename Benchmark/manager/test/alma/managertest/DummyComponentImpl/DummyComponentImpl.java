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
package alma.managertest.DummyComponentImpl;

import alma.acs.component.ComponentImplBase;
import alma.managertest.DummyComponentOperations;

public class DummyComponentImpl extends ComponentImplBase  implements DummyComponentOperations
{

	public DummyComponentImpl(){}	

	public void cleanUp() throws alma.maciErrType.wrappers.AcsJComponentCleanUpEx {
		m_logger.info("cleanUp called.");
		super.cleanUp();
	}
/*
	public double returnAttributeValue(double a)
	{
		m_logger.info("returnAttributeValue called");
		return a;
	}
*/	
	public void doNothing()
	{
		m_logger.info("doNothing called");
	}

}
