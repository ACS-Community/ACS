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
package alma.ACSCOURSE_MOUNT.Mount1Impl;

import alma.ACSCOURSE_MOUNT.Mount1Operations;
import alma.acs.component.ComponentImplBase;

/**
 * @author hsommer
 * created Mar 18, 2004 4:27:29 PM
 */
public class Mount1Impl extends ComponentImplBase implements Mount1Operations
{

	/**
	 * @see alma.ACSCOURSE_MOUNT.Mount1Operations#objfix(double, double)
	 */
	public void objfix(double az, double elev) {
		String msg = "Stupid implementation of Mount1 does nothing in method objfix. Parameters are az='" +
					az + "' and elev='" + elev + "'";
		m_logger.info(msg);

	}

}

