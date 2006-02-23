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
import java.util.logging.Formatter;
import java.util.logging.LogRecord;

import org.omg.CORBA.Any;
import org.omg.CORBA.ORB;
import org.omg.CORBA.UserException;
import org.omg.DsLogAdmin.Log;

/**
 * Sends log records to the remote CORBA log service.
 * No caching or threading is done, so this class should be used at the delivering end of a smarter queue.
 * 
 * @author hsommer
 * created Apr 19, 2005 4:15:19 PM
 */
class RemoteLogDispatcher {

    /** 
     * Maximum number of CORBA Anys (with 1 log record each) sent to remote log service at a time.
     * This buffer is what the CDB documentation refers to as "cache" (e.g. attribute <code>cacheSize</code>).
     * We call it 'buffer' because our 'cache' is the queue that feeds this dispatcher. 
     */
    private int bufferSize = 30;

    private ORB orb;
    private Log logService;
    
    private Formatter xmlFormatter;
    private LogRecordComparator timestampLogRecordComparator;
    protected boolean DEBUG = false;
    
    
    /**
     * 
     * @param orb  used for creation of Anys
     * @param logService  remote log service to which logs are sent. 
     *                    Since the reference is persistent across failures and restarts, 
     *                    we can keep working with this object the whole time.
     *                    May be <code>null</code> for unit tests, in which case {@link #sendLogRecords(LogRecord[])} 
     *                    will fail and return the all log records inside the <code>FailedLogRecords</code> structure.
     * @param xmlLogFormatter  used to format LogRecords to XML representation. 
     *                    No direct assumption on XML is made, so technically any valid String returned by the formatter will do 
     *                    (as far as this class is concerned).  
     */
    RemoteLogDispatcher(ORB orb, Log logService, Formatter xmlLogFormatter) {
        this.orb = orb;
        this.logService = logService;
        this.xmlFormatter = xmlLogFormatter;
        timestampLogRecordComparator = new LogRecordComparator(true);
    }
 
    public int getBufferSize() {
        return bufferSize;
    }

    /**
     * Sets the size of the log record buffer, which determines the maximum number of log records 
     * that can be sent to the remote log service in one call. 
     * <p>
     * Corresponds to <code>cacheSize</code> in the CDB.
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
     * @return those LogRecords that failed to be sent, either because they could not be converted to XML, 
     * or because the remote logger failed.
     */
    FailedLogRecords sendLogRecords(LogRecord[] logRecords) {
        // sort this set of records by timestamp (queue delivers them by level/timestamp)
        Arrays.sort(logRecords, timestampLogRecordComparator);
        
        FailedLogRecords failures = new FailedLogRecords();
        
        // used for feeding back these LogRecords if the sending fails
        List candidateLogRecords = new ArrayList(); 
        
        // create CORBA Anys containing XML String representations of log records
        List anyLogRecords = new ArrayList();
        for (int i = 0; i < logRecords.length; i++) {
            if (i < getBufferSize()) {
                try {
                    String xmlLogRecord = xmlFormatter.format(logRecords[i]);
                    // TODO: check if these anys can and should be cached for performance reasons
                    Any anyLogRecord = orb.create_any();
                    anyLogRecord.insert_string(xmlLogRecord);
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
            Any[] anyLogRecordsArray = (Any[]) anyLogRecords.toArray(new Any[anyLogRecords.size()]);
            try {
                writeRecords(anyLogRecordsArray);
            } catch (Throwable thr) {
                // feed back these records to we can try them again later
                failures.addSendFailures(candidateLogRecords);
                // Note that ACS does not make use of the semantics of the various exceptions specified here by CORBA, i.e.
                // LogDisabled, LogOffDuty, LogLocked, LogFull
            }
        }
        return failures;
    }

    
    /**
     * The CORBA call. To be faked by test subclasses.
     * @param anyLogRecordsArray
     * @throws UserException
     */
    protected void writeRecords(Any[] anyLogRecordsArray) throws UserException {
        logService.write_records(anyLogRecordsArray);
        if (DEBUG) {
            System.out.println("sent the following XML log records to the log service:");
            for (int i = 0; i < anyLogRecordsArray.length; i++) {
                System.out.println("" + i + ") " + anyLogRecordsArray[i].extract_string());
            }
        }
    }
    


    static class FailedLogRecords {
        private List serializeFailures;
        private List sendFailures;
        
        void addSerializationFailure(LogRecord logRecord) { 
            if (serializeFailures == null) {
                serializeFailures = new ArrayList();
            }
            serializeFailures.add(logRecord);
        }
        void addSendFailure(LogRecord logRecord) {
            if (sendFailures == null) {
                sendFailures = new ArrayList();
            }
            sendFailures.add(logRecord);
        }
        void addSendFailures(List logRecords) {
            if (sendFailures == null) {
                sendFailures = new ArrayList();
            }
            sendFailures.addAll(logRecords);
        }
        
        boolean hasSerializationFailures() {
            return (serializeFailures != null);
        }
        LogRecord[] getSerializationFailures() {
            if (serializeFailures == null) {
                return new LogRecord[0];
            }
            else {
                return (LogRecord[]) serializeFailures.toArray(new LogRecord[serializeFailures.size()]);
            }
        }
        
        boolean hasSendFailures() {
            return (sendFailures != null);
        }
        LogRecord[] getSendFailures() {
            if (sendFailures == null) {
                return new LogRecord[0];
            }
            else {
                return (LogRecord[]) sendFailures.toArray(new LogRecord[sendFailures.size()]);
            }
        }
        
    }
}
