/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2005
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

package alma.acs.logging.adapters;

import java.util.logging.Filter;
import java.util.logging.Level;
import java.util.logging.LogRecord;



/**
 * Filters out JacORB log messages that we don't want to see.
 * Also corrects the log level of a few known messages when it seems inappropriate;
 * this is a bit dirty because the logging filters are not expected to modify the log records,
 * but it saves us from re-examining the same log record in a subsequent home-brew modifier stage.  
 */
public class JacORBFilter implements Filter {

	protected int logLevel = Level.ALL.intValue(); 
	
	/**
	 * Discards less useful or misleading Jacorb logs based on the message.
	 * <p>
	 * To improve performance, we could instead match file and line, but then would have to update 
	 * that information with the next jacorb code change.
	 */
	public boolean isLoggable(LogRecord record) {
		String message = record.getMessage();
		boolean isLoggable = true;
		if (record.getLevel().intValue() == Level.FINE.intValue()) {
			// from FINE to discard
			isLoggable = !(
					message.endsWith("_invoke: queuing request") ||
					message.indexOf("is queued (queue size:") > 0 ||
					message.endsWith("trying to get a RequestProcessor") ||
					message.endsWith("waiting for queue") ||
					message.endsWith("with request processing") || // for "starts...",  "ends..."			
					message.endsWith("invokeOperation on servant (stream based)") ||
					message.endsWith("reset a previous completion call") ||
					message.startsWith("Delegate.getReference") || 
					message.startsWith("ClientConnectionManager: releasing ClientGIOPConnection") ||
					message.startsWith("Delegate released!") ||
					message.endsWith(": streamClosed()")								
			);
		}
		else if (record.getLevel().intValue() == Level.INFO.intValue()) {
			// map from INFO to FINE
			if (message.indexOf("(C) The JacORB project") > 0 || 
				message.equals("ORB run") ||
				message.startsWith("Received CloseConnection on ClientGIOPConnection") ||
				message.startsWith("prepare ORB for shutdown") ||
				message.startsWith("Client-side TCP transport to") ||
				message.startsWith("ORB going down...") ||
				(message.startsWith("POA ") && message.endsWith("destroyed")) ||
				message.startsWith("Opened new server-side TCP/IP" )) {
				record.setLevel(Level.FINE);
			}
			// map from INFO to FINER
			else if (message.startsWith("Connected to ") ||
					message.startsWith("Closed server-side transport to") ||
					message.startsWith("ClientConnectionManager: created new ClientGIOPConnection") ||
					message.startsWith("ClientConnectionManager: found ClientGIOPConnection") ||
					message.equals("Listener exited") ) {
				record.setLevel(Level.FINER);
			}
			// from INFO to discard
			else isLoggable = !(
					message.startsWith("oid: ") ||
					message.startsWith("InterceptorManager started with") ||
					message.startsWith("Using server ID (")
			);
		}
		
		// TODO: filter out error message "no adapter activator exists for ComponentPOA"
		
		// filter by possibly modified log level
		if (isLoggable && record.getLevel().intValue() < this.logLevel) {
			isLoggable = false;
		}

//		if (!isLoggable) {
//			System.out.println("dropping JacORB message " + message + " with Level " + record.getLevel().getName());
//		}
		
		return isLoggable;
	}

	
	/**
	 * Sets the log level that allows this filter to discard log records.
	 * This is necessary because our extended filter functionality may downgrade log levels.
	 * Log records that passed the level check before may then become invalid. 
	 * The JDK does not foresee this, that's why we must discard them here based on the level. 
	 */
	public void setLogLevel(Level level) {
		logLevel = level.intValue();
	}
}
