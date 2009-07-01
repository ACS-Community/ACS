/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2004
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
package alma.acs.logging;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.logging.LogRecord;

import org.omg.CORBA.Any;
import org.omg.CORBA.ORB;
import org.omg.CORBA.UserException;
import org.omg.DsLogAdmin.Log;
import org.omg.DsLogAdmin.LogOperations;

import alma.Logging.AcsLogServiceOperations;
import alma.Logging.XmlLogRecord;
import alma.acs.logging.formatters.AcsLogFormatter;
import alma.acs.logging.formatters.AcsXMLLogFormatter;

/**
 * Sends log records to the remote CORBA log service.
 * No caching or threading is done, so this class should be used at the delivering end of a smarter queue.
 * 
 * @author hsommer
 * created Apr 19, 2005 4:15:19 PM
 */
class RemoteLogDispatcher {

	/**
	 * Name of the property which will enable using the ACS extensions to the 
	 * telecom logging service ("Log"), mainly to avoid wrapping all log records with a Corba Any.
	 */
	public static final String USE_ACS_LOGSERVICE_EXTENSIONS_PROPERTYNAME = "alma.acs.logging.useAcsLogServiceExtensions";

	public final boolean useAcsLogServiceExtensions = Boolean.getBoolean(USE_ACS_LOGSERVICE_EXTENSIONS_PROPERTYNAME);
	
	/** 
	 * Maximum number of CORBA Anys (with 1 log record each) sent to remote log service at a time.
	 * This buffer is is configured in the CDB through the attribute <code>dispatchPacketSize</code>.
	 */
	private int bufferSize = 30;

	private final ORB orb;
	private final AcsLogServiceOperations logService;

	private final AcsLogFormatter logFormatter;

	private LogRecordComparator timestampLogRecordComparator;

	private final boolean DEBUG = Boolean.getBoolean("alma.acs.logging.verbose");

    
    /**
     * 
     * @param orb  used for creation of Anys
     * @param logService  remote log service to which logs are sent. 
     *                    Since the reference is persistent across failures and restarts, 
     *                    we can keep working with this object the whole time.
     *                    May be <code>null</code> for unit tests, in which case {@link #sendLogRecords(LogRecord[])} 
     *                    will fail and return the all log records inside the <code>FailedLogRecords</code> structure.
     *                    With ACS 7.0.1 we allow the super type {@link LogOperations} instead of the usually
     *                    expected {@link Log}, to allow unit tests with a mock Log service.
     * @param logFormatter  used to format LogRecords to Any representation. 
     *                    No direct assumption on XML is made, so technically any valid Any returned by the formatter will do 
     *                    (as far as this class is concerned).  
     */
    RemoteLogDispatcher(ORB orb, AcsLogServiceOperations logService, AcsLogFormatter logFormatter) {
        this.orb = orb;
        this.logService = logService;
        this.logFormatter = logFormatter;
        timestampLogRecordComparator = new LogRecordComparator(true);
        
        if (useAcsLogServiceExtensions && !(logFormatter instanceof AcsXMLLogFormatter)) {
        	throw new RuntimeException("Only XML-based remote logging is supported with the " + USE_ACS_LOGSERVICE_EXTENSIONS_PROPERTYNAME + " option.");
        }
    }
 
    public int getBufferSize() {
        return bufferSize;
    }

    /**
     * Sets the size of the log record buffer, which determines the maximum number of log records 
     * that can be sent to the remote log service in one call. 
     * <p>
     * Corresponds to <code>dispatchPacketSize</code> in the CDB.
     * 
     * @param newBuffSize value >=1, otherwise ignored
     */
    public void setBufferSize(int newBuffSize) {
        if (newBuffSize >= 1) {
            bufferSize = newBuffSize;
        }
        else {
            System.err.println("RemoteLogDispatcher#setBufferSize: ignoring illegal value " + newBuffSize);
        }
    }

    /**
     * Attempts to send <code>logRecords</code> over to the remote logging service.
     * To not lose any log records in case of failure, they can be obtained from the returned 
     * <code>FailedLogRecords</code> object, and should be fed back to the log record queue in order to try again later.
     * <p>
     * Should not be called concurrently (which can't happen since we use a single threaded executor 
     * in <code>DispatchingLogQueue</code>).
     * <p>
     * Sorts all log records by timestamp before converting them for remote transmission.
     * 
     * @param logRecords
     * @return those LogRecords that failed to be sent, either because they could not be converted to Any, 
     * or because the remote logger failed.
     */
    FailedLogRecords sendLogRecords(LogRecord[] logRecords) {
        // sort this set of records by timestamp (queue delivers them by level/timestamp)
        Arrays.sort(logRecords, timestampLogRecordComparator);
        
        FailedLogRecords failures = new FailedLogRecords();
        
        // used for feeding back these LogRecords if the sending fails
        List<LogRecord> candidateLogRecords = new ArrayList<LogRecord>(); 

		if (useAcsLogServiceExtensions) {
			// create CORBA XmlLogRecord containing XML representations of log records and the log level as a separate field
			List<XmlLogRecord> remoteLogRecords = new ArrayList<XmlLogRecord>();
			for (int i = 0; i < logRecords.length; i++) {
				if (i < getBufferSize()) {
					try {
						String xml = ((AcsXMLLogFormatter) logFormatter).format(logRecords[i]);
						int level = AcsLogLevel.getNativeLevel(logRecords[i].getLevel()).getAcsLevel().value;
						XmlLogRecord remoteLogRecord = new XmlLogRecord(xml, (short) level);
						remoteLogRecords.add(remoteLogRecord);
						candidateLogRecords.add(logRecords[i]);
					} 
					catch (RuntimeException e) {
						failures.addSerializationFailure(logRecords[i]);
					}
				} 
				else {
					// records that don't fit into one remote call must be sent later
					// this should never happen except for during concurrently changing buffer size.
					failures.addSendFailure(logRecords[i]);
				}
			}
			// send the log records over CORBA
			if (!remoteLogRecords.isEmpty()) {
				XmlLogRecord[] remoteLogRecordsArray = remoteLogRecords.toArray(new XmlLogRecord[remoteLogRecords.size()]);
				writeRecords(remoteLogRecordsArray);
			}
		}
        else { // default is Corba telecom Log interface that requires records inside Anys
	        List<Any> anyLogRecords = new ArrayList<Any>();
	        for (int i = 0; i < logRecords.length; i++) {
	            if (i < getBufferSize()) {
	                try {
	                    Any anyLogRecord = orb.create_any();
	                    anyLogRecord = logFormatter.formatAny(anyLogRecord, logRecords[i]);
	                    anyLogRecords.add(anyLogRecord);
	                    candidateLogRecords.add(logRecords[i]);
	                } catch (RuntimeException e) {
	                    failures.addSerializationFailure(logRecords[i]);
	                }
	            }
	            else {
	                // records that don't fit into one remote call must be sent later
	                failures.addSendFailure(logRecords[i]);
	            }
	        }
	        
	        // send the log records over CORBA 
	        if (!anyLogRecords.isEmpty()) {
	            Any[] anyLogRecordsArray = anyLogRecords.toArray(new Any[anyLogRecords.size()]);
	            try {
	                writeRecords(anyLogRecordsArray);
	            } 
	//            catch (LogFull ex1) {
	//            	// Telecom Log Service spec v. 1.1.2, chapter 1.2.5.1:
	//            	// "if availability status is log_full and LogFullAction is "halt", then a LogFull exception is raised 
	//            	// and the number of log records written will be returned in the exception."
	//            	// @TODO: Can we actually assume that this number n refers to the first n log records from the Any[] so that we only feed back the later records?
	//            	int recordsWritten = ex1.n_records_written;
	//            	if (recordsWritten > 0 && recordsWritten < candidateLogRecords.size()) {
	//            		for (int i = recordsWritten; i < candidateLogRecords.size(); i++) {
	//						failures.addSendFailure(candidateLogRecords.get(i));
	//					}
	//            	}
	//            	else {
	//            		failures.addSendFailures(candidateLogRecords);
	//            	}
	//            }
	//            // @todo: slow down future sending attempts if we get a 
	//            // LogDisabled - operational state "disabled" due to some runtime problem in the Log
	//            // LogLocked - hopefully temporary admin setting on the Log
	//            // LogOffDuty - other exotic reasons to be not "on duty", such as log duration time or scheduling time
	            catch (Throwable thr) {
	                // feed back these records to we can try them again later
	                failures.addSendFailures(candidateLogRecords);
	                // Currently ACS does not make use of the semantics of the various exceptions specified here by CORBA, i.e.
	                // LogDisabled, LogOffDuty, LogLocked, LogFull (see above)
	                // There can also be java.net.ConnectException thrown.. @TODO perhaps we should print that to stdout, with repeatGuard.
	            }
	        }
        }
        return failures;
    }


	/**
	 * The CORBA call to {@link Log#write_records(Any[])}. May be faked by test subclasses.
	 * 
	 * @param anyLogRecordsArray
	 * @throws UserException
	 */
	protected void writeRecords(Any[] anyLogRecordsArray) throws UserException {
		logService.write_records(anyLogRecordsArray);
		if (DEBUG) {
			System.out.println("sent the following Any log records to the log service:");
			for (int i = 0; i < anyLogRecordsArray.length; i++) {
				// TODO: CARLI, change that here assumes that is string, it can be a binary log!!!
				System.out.println("" + i + ") " + anyLogRecordsArray[i].extract_string());
			}
		}
	}

	/**
	 * Alternative to {@link #writeRecords(Any[]) used when property {@code alma.acs.logging.useAcsLogServiceExtensions} is set.
	 * @param remoteLogRecords
	 */
	protected void writeRecords(XmlLogRecord[] remoteLogRecords) {
		logService.writeRecords(remoteLogRecords);
	}



	static class FailedLogRecords
	{
		private List<LogRecord> serializeFailures;
		private List<LogRecord> sendFailures;

		void addSerializationFailure(LogRecord logRecord) {
			if (serializeFailures == null) {
				serializeFailures = new ArrayList<LogRecord>();
			}
			serializeFailures.add(logRecord);
		}

		void addSendFailure(LogRecord logRecord) {
			if (sendFailures == null) {
				sendFailures = new ArrayList<LogRecord>();
			}
			sendFailures.add(logRecord);
		}

		void addSendFailures(List<LogRecord> logRecords) {
			if (sendFailures == null) {
				sendFailures = new ArrayList<LogRecord>();
			}
			sendFailures.addAll(logRecords);
		}

		boolean hasSerializationFailures() {
			return (serializeFailures != null);
		}

		List<LogRecord> getSerializationFailures() {
			if (serializeFailures == null) {
				return new ArrayList<LogRecord>(0);
			} else {
				return serializeFailures;
			}
		}

		boolean hasSendFailures() {
			return (sendFailures != null);
		}

		List<LogRecord> getSendFailures() {
			if (sendFailures == null) {
				return new ArrayList<LogRecord>(0);
			} else {
				return sendFailures;
			}
		}

	}
}
