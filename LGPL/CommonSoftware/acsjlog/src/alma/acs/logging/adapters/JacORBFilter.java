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

import java.util.Date;
import java.util.logging.Filter;
import java.util.logging.Level;
import java.util.logging.LogRecord;

import alma.acs.util.IsoDateFormat;


/**
 * Filters out JacORB log messages that we don't want to see.
 * Also corrects the log level of a few known messages when it seems inappropriate (and blanks non-printable chars in the message);
 * this is a bit dirty because the JDK logging filters are not expected to modify the log records,
 * but it saves us from re-examining the same log record in a subsequent home-brew modifier stage.
 * <p>
 * See http://jira.alma.cl/browse/COMP-8302 about statistics of real-world jacorb logs.
 */
public class JacORBFilter implements Filter {

	protected int logLevel = Level.ALL.intValue(); 
	
	/**
	 * Discards less useful or misleading Jacorb logs based on the message.
	 * <p>
	 * TODO-: to improve performance, we could instead match file and line, but then would have to update 
	 * that information with the next jacorb code change.
	 * The implementation already minimizes string comparison by checking only those messages that can occur at the given log level.
	 * <p>
	 * TODO: Add repeat guard based on the message, e.g. using MultipleRepeatGuard from jacsutil.
	 */
	public boolean isLoggable(LogRecord record) {
		String message = record.getMessage();
		boolean isLoggable = true;
		if (record.getLevel().intValue() == Level.FINE.intValue()) {
			// map from FINE to FINEST
			if (message.startsWith("POA ") && (
						message.endsWith("shutdown is in progress") ||
						message.endsWith("destruction is apparent") ||
						message.endsWith("clear up the queue...") ||
						message.endsWith("... done") ||
						message.endsWith("stop the request controller") ||
						message.endsWith("etherialize all servants ...")
					) ) {
				record.setLevel(Level.FINEST);
			}
			else if (
					message.startsWith("get_policy_overrides returns") ||
					message.startsWith("read GIOP message of size") ||
					message.startsWith("wrote GIOP message of size")) {
				// From ACS unit tests it seems that this message is totally harmless, 
				// caused by a Corba stub calling org.jacorb.orb.Delegate#getRelativeRoundtripTimeout()
				// and asking for the RELATIVE_RT_TIMEOUT_POLICY_TYPE policy.
				// We do not fully discard the log though because it may have other causes at the AOS.
				record.setLevel(Level.FINEST);
//				// Enable the following 2 lines to investigate for http://jira.alma.cl/browse/COMP-8302, to see where all these logs come from
//				String stackTrace = org.apache.commons.lang.exception.ExceptionUtils.getStackTrace(new Throwable());
//				System.out.println("Hack for COMP-8302 debugging: 'get_policy_overrides returns' message logged with trace " + stackTrace);
			} 
			// from FINE to discard
			else isLoggable = !(
					message.endsWith("_invoke: queuing request") ||
					message.indexOf("is queued (queue size:") > 0 ||
					message.endsWith("trying to get a RequestProcessor") ||
					message.endsWith("waiting for queue") ||
					message.endsWith("with request processing") || // for "starts...",  "ends..."
					message.endsWith("invokeOperation on servant (stream based)") ||
					message.endsWith("reset a previous completion call") ||
					message.startsWith("Delegate.getReference") || 
					message.startsWith("Received CodeSetContext. Using") || 
					message.startsWith("ClientConnectionManager: releasing ClientGIOPConnection") ||
					message.startsWith("Delegate released!") ||
					message.endsWith(": streamClosed()")		// findPOA: impl_name mismatch
			);
		}
		
		else if (record.getLevel().intValue() == Level.INFO.intValue()) {
			// from INFO to CONFIG
			if (message.indexOf("(C) The JacORB project") > 0 || 
				message.startsWith("Received CloseConnection on ClientGIOPConnection") ||
				message.startsWith("prepare ORB for shutdown") ||
				message.startsWith("Client-side TCP transport to") ||
				message.startsWith("ORB going down...") ||
				message.startsWith("ORB run") || // also "ORB run, exit"
				message.equals("ORB shutdown complete") || 
				( message.startsWith("POA ") && (
						message.endsWith("destroyed") 
				)) ||
				message.startsWith("Opened new server-side TCP/IP" )) {
				record.setLevel(Level.CONFIG);
			}
			// from INFO to FINEST
			else if (message.startsWith("Connected to ") ||
					message.startsWith("Closed server-side transport to") ||
			        ( message.startsWith("POA ") && 
                       ( message.endsWith("up the queue ...") ||
						 message.endsWith("... done")
                       ) 
                     ) ||
					message.startsWith("Retrying to connect to") ||
					message.startsWith("Negotiated char codeset of") ||
					message.startsWith("ClientConnectionManager: created new ClientGIOPConnection") ||
					message.startsWith("ClientConnectionManager: found ClientGIOPConnection") ||
					message.startsWith("Initialising ORB with ID") ||
					message.startsWith("Set default native char codeset to") ||
					message.startsWith("Set default native wchar codeset to") ||
					message.equals("Listener exiting") ||
					message.equals("ConsumerTie exited") ) {
				record.setLevel(Level.FINEST);
			}
			// from INFO to stdout shortcut
			else if (( message.startsWith("ClientGIOPConnection to ") || 
					   message.startsWith("ServerGIOPConnection to ") ) && 
					 message.contains(" BufferDump:\n" ) ||
					message.startsWith("sendMessages(): ")) {
				// The jacorb dumps from org.jacorb.orb.etf.StreamConnectionBase.flush()
				// and org.jacorb.orb.giop.GIOPConnection.getMessage() are very special.
				// They get enabled via properties 'jacorb.debug.dump_outgoing_messages' 
				// and 'jacorb.debug.dump_incoming_messages' and logged as INFO.
				// If they are enabled, we still want to see them only in the local logs 
				// (otherwise there may be even "positive feedback" leading to log explosion).
				// A cleaner concept would be to only flag this LogRecord as "log only locally", 
				// return "isLoggable = true" and leave the rest to the local and remote log handlers.
				// The fact that we are dealing with a generic LogRecord (not AcsLogRecord) makes this
				// option also ugly though (LogParameterUtil tricks), and thus we just print and drop 
				// the log right away here.
				String timeStamp = IsoDateFormat.formatDate(new Date(record.getMillis()));
				System.out.println(timeStamp + " " + message);
				isLoggable = false;
			}
			// from INFO to discard
			else isLoggable = !(
					message.startsWith("oid: ") ||
					message.startsWith("InterceptorManager started with") ||
					message.startsWith("Using server ID (")
			);
		}
		
		else if (record.getLevel().intValue() == Level.WARNING.intValue()) {
			// from WARNING to CONFIG
//			if (message.indexOf("") > 0) { 
//				record.setLevel(Level.CONFIG);
//			}
//			// from WARNING to FINEST
//			else if (message.startsWith("")) {
//				record.setLevel(Level.FINEST);
//			}
			// from WARNING to discard
//			else 
			isLoggable = !(
					message.startsWith("no adapter activator exists for") // client tries to find remote poa locally and then complains if not there... 
			);
		}
		else if (record.getLevel().intValue() == Level.SEVERE.intValue()) {
			// HSO 07-02-19: thought this adapter activator crap was logged as warning, but now it showed up in jcont test as ACS-level "Emergency", which is JDK-SEVERE  
			isLoggable = !(
					message.startsWith("no adapter activator exists for") // client tries to find remote poa locally and then complains if not there... 
			);
		}
				
		// filter by possibly modified log level
		if (isLoggable && record.getLevel().intValue() < this.logLevel) {
			isLoggable = false;
		}

//		if (!isLoggable) {
//			System.out.println("dropping JacORB message " + message + " with Level " + record.getLevel().getName());
//		}
		
		// Remove non-ascii chars from the log message, which seems to occur only in logs coming from jacorb, 
		// see http://jira.alma.cl/browse/COMP-3243
		// For simplicity we blank all non-ascii-7-printable chars except newline and tab, 
		// at the low risk of erroneously blanking more sophisticated (Umlaut etc) chars that jacorb may send us.
		// If that turns out to be the case, and the encoding turns out as unicode, then see http://en.wikipedia.org/wiki/C0_and_C1_control_codes
		if (isLoggable && message != null) {
			String message2 = null;
			int mlen = message.length();
			for (int i = 0; i < mlen; i++) {
				char ch = message.charAt(i);
				if ((ch < 32 || ch >= 127) && ch != '\n' && ch != '\t') {
					// got a bad char
					if (message2 == null) {
						// copy the leading good chars only if needed, to improve performance
						message2 = message.substring(0, i);
					}
					// blank the bad char
					message2 += '#';
				}
				else if (message2 != null) {
					message2 += ch;
				}
			}
			if (message2 != null) {
				record.setMessage(message2);
			}
		}
		
		return isLoggable;
	}

	
	/**
	 * Sets the log level that allows this filter to discard log records.
	 * This is necessary because our extended filter functionality may downgrade log levels.
	 * Log records that passed the level check before may then become invalid. 
	 * The JDK does not foresee this, that's why we must discard them here based on the level. 
	 */
	public void setLogLevel(Level level) {
		if (level != null) {
			logLevel = level.intValue();
		}
//		else {
//			System.out.println("&&&& JacORB filter got null level");
//		}
	}
}
