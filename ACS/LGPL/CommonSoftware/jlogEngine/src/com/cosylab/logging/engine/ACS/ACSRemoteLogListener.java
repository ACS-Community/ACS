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
package com.cosylab.logging.engine.ACS;

import com.cosylab.logging.engine.log.ILogEntry;

/**
 * This class defines the interface for classes (LCEngine) 
 * that support custom callbacks for receiving log entries 
 * submitted to the logging mechanism and for processing them.
 */
public interface ACSRemoteLogListener {
	
	/**
	 * The method is executed when a new log arrives from the NC
	 * 
	 * @param logEntry The new log just read from the NC
	 */
	public void logEntryReceived(ILogEntry logEntry);
	

}
