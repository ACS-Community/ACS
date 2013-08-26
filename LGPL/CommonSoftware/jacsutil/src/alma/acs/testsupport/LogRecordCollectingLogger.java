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
package alma.acs.testsupport;

import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.logging.ConsoleHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogManager;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

/**
 * Creates a Logger that collects all <code>LogRecord</code>s produced by calls to the various log methods.
 * This can be used to verify log messages in a pure JUnit environment, without the need to use TAT reference files. 
 * 
 * @author hsommer
 * created Apr 20, 2005 2:30:47 PM
 */
public class LogRecordCollectingLogger extends Logger {

    private List<LogRecord> logRecordList = Collections.synchronizedList(new ArrayList<LogRecord>());
    
    private boolean suppressLogs = false;
    private Logger delegate;
    
    /**
     * @see java.util.logging.Logger#getLogger(java.lang.String)
     */
    public static synchronized LogRecordCollectingLogger getCollectingLogger(String name) {
    	return getCollectingLogger(name, LogRecordCollectingLogger.class);
    }

    public static synchronized <T extends LogRecordCollectingLogger> T getCollectingLogger(String name, Class<T> loggerClass) {
        LogManager manager = LogManager.getLogManager();
        Logger logger = manager.getLogger(name);
        if (logger != null && !loggerClass.isInstance(logger)) {
        	throw new ClassCastException("Logger '" + name + "' is already used and is of wrong type " + logger.getClass().getName());
        }
        T myLogger = (T) logger;
        if (myLogger == null) {
        	try {
				Constructor<T> ctor = loggerClass.getDeclaredConstructor(String.class, String.class);
				myLogger = ctor.newInstance(name, null);
				manager.addLogger(myLogger); 
				myLogger = (T) manager.getLogger(name);
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
        }
        myLogger.setLevel(Level.FINEST);
        myLogger.setUseParentHandlers(false);
        Handler logHandler = new ConsoleHandler();
        logHandler.setLevel(Level.FINEST);
        myLogger.addHandler(logHandler);      

        return myLogger; 
    }

    protected LogRecordCollectingLogger(String name, String resourceBundleName) {
        super(name, resourceBundleName);
    }

    /**
     * Here we intercept all logging activities of the base class.
     * <p>
     * 
     * @see java.util.logging.Logger#log(java.util.logging.LogRecord)
     */
    public void log(LogRecord record) {
        // must trigger a call to LogRecord#inferCaller before the log record gets formatted in a different thread        
        record.getSourceClassName();
        logRecordList.add(record);
        if (delegate != null && !suppressLogs) {
            delegate.log(record);
        }
    }

    
    public LogRecord[] getCollectedLogRecords() {
        return logRecordList.toArray(new LogRecord[logRecordList.size()]);
    }

    public void clearLogRecords() {
        logRecordList.clear();
    }
    
    /**
     * Switches real logging off/on. 
     * Logging will only happen if a real logger has been provided.
     * @param suppressLogs true if collected LogRecords should not be sent to a real logger
     * @see #setDelegateLogger(Logger)
     */
    public void suppressLogs(boolean suppressLogs) {
        this.suppressLogs = suppressLogs;
    }

    /**
     * @param delegate a logger to receive log output from this logger, or <code>null</code> to not log any output,
     *        but only store the log records.
     * @see #suppressLogs(boolean)
     */
    public void setDelegateLogger(Logger delegate) {
        this.delegate = delegate;
        if (delegate == null) {
            suppressLogs = true;
        }
    }
    
}
