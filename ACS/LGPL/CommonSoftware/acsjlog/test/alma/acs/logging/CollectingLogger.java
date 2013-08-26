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

import java.util.logging.Level;

import alma.acs.testsupport.LogRecordCollectingLogger;

/**
 * Creates a Logger that collects all <code>LogRecord</code>s produced by calls to the various log methods.
 * This can be used to generate valid LogRecords for unit tests.
 * 
 * @author hsommer
 * created Apr 20, 2005 2:30:47 PM
 */
public class CollectingLogger extends LogRecordCollectingLogger {

    
    /**
     * Don't use this ctor directly!
     * Instead, use {@link LogRecordCollectingLogger#getCollectingLogger(java.lang.String, java.lang.Class)},
     * passing <code>CollectingLogger.class</code> as the second argument.
     * (This is some experiment, so don't ask why...)
     * @param name
     * @param resourceBundleName
     */
    public CollectingLogger(String name, String resourceBundleName) {
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
