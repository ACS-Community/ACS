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
import java.util.Collections;
import java.util.List;
import java.util.logging.ConsoleHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogManager;

import alma.acs.testsupport.LogRecordCollectingLogger;

/**
 * Creates a Logger that collects all <code>LogRecord</code>s produced by calls to the various log methods.
 * This can be used to generate valid LogRecords for unit tests.
 * 
 * @author hsommer
 * created Apr 20, 2005 2:30:47 PM
 */
public class CollectingLogger extends LogRecordCollectingLogger {

    private List logRecordList = Collections.synchronizedList(new ArrayList());
    private boolean suppressLogs = false;
    
    /**
     * @see java.util.logging.Logger#getLogger(java.lang.String)
     */
    public static synchronized CollectingLogger getTestLogger(String name) {
        LogManager manager = LogManager.getLogManager();
        CollectingLogger result = (CollectingLogger) manager.getLogger(name);
        if (result == null) {
            result = new CollectingLogger(name, null);
            manager.addLogger(result);
            result = (CollectingLogger) manager.getLogger(name);
        }
        result.setLevel(Level.FINEST);
        result.setUseParentHandlers(false);
        Handler logHandler = new ConsoleHandler();
        logHandler.setLevel(Level.FINEST);
        result.addHandler(logHandler);      

        return result;
    }

    protected CollectingLogger(String name, String resourceBundleName) {
        super(name, resourceBundleName);
    }

    /**
     * Produces a sequence of LogRecords which can be obtained in {@link #getCollectedLogRecords()}.
     * Output is limited by the <code>number</code> parameter. The algorithm is: 
     * <ol>
     * <li> INFO  
     * <li> SEVERE with exception  
     * <li> FINE
     * <li> alternating INFO and FINEST
     * </ol>
     * @param number
     */
    public void produceLogs1(int number) {
        if (number >= 1) {
            info("1) Just a stupid info");
            sleep(100); // to make sure the next log record has a higher timestamp
        }
        if (number >= 2) {
            log(Level.SEVERE, "2) bad NPE", new NullPointerException("I'm a NPE"));
            sleep(10);
        }
        if (number >= 3) {
            fine("3) now something light for a change.");
            sleep(10);
        }
        for (int i = 4; i <= number; i++) {
            String msg = "" + i + ") another ";
            Level level = ((i%2)==0 ? Level.INFO : Level.FINEST );
            log(level, msg+level.getName());
            sleep(10);
        }
    }
    
    private void sleep(long millis) {
        try {
            Thread.sleep(millis);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

}
