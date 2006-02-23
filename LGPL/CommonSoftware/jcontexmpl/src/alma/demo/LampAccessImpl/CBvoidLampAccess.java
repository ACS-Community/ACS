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
package alma.demo.LampAccessImpl;

import java.util.logging.Logger;

import alma.ACS.CBDescOut;
import alma.ACS.CBvoidPOA;
import alma.ACSErr.Completion;

/** 
 * @author radi
 * 
 * Implements the callback for the LampAccess component. 
 */

public class CBvoidLampAccess extends CBvoidPOA 
{
	private Logger m_logger;

	/**
	 * Passes a logger to the callback object.
	 * @param logger
	 */
	public CBvoidLampAccess(Logger logger)
	{
		m_logger = logger;		
	}

	/**
	 * Used to notify of a state of incompletion.
	 * @param completion error handing structure.
	 * @param desc callback descriptor passed from server to client.
	 * @see alma.ACS.CBvoidOperations#working(alma.ACSErr.Completion, alma.ACS.CBDescOut)
	 */
	public void working(Completion completion, CBDescOut desc)
	{
		m_logger.fine("called working()...");
	}
	/**
	 * Used to notify of a state of completion.
	 * @param completion error handing structure.
	 * @param desc callback descriptor passed from server to client.
	 * @see alma.ACS.CBvoidOperations#done(alma.ACSErr.Completion, alma.ACS.CBDescOut)
	 */
	public void done(Completion completion, CBDescOut desc)
	{
		m_logger.fine("called done()...");
	}
	/**
	 * Used to negotiate timeouts between client and sever.
	 * @param myLong timeout between client and sever.
	 * @param desc callback descriptor passed from server to client.
	 * @return boolean
	 * @see alma.ACS.CallbackOperations#negotiate(long, alma.ACS.CBDescOut)
	 */
	public boolean negotiate(long myLong, CBDescOut desc)
	{
		m_logger.fine("called negotiate()...");
		return true;
	}
}
