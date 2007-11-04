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

import java.util.logging.LogManager;

/**
 * A custom log manager to be instantiated by the JVM based on the <code>java.util.logging.manager</code> property.
 * <p>
 * Currently only differs from standard JDK LogManager in that it dodges the mean effects of the shutdown hook 
 * which calls {@link LogManager#reset()} too early. 
 * It thus allows the Java container (and the Java manager and potentially other users of acsjlog) 
 * to send remote log messages until the end. 
 * <p>
 * Since ACS 7.0 the ClientLogManager no longer delegates logger requests to this class.
 * It is just kept around until we know how to support the JVM logger introspection 
 * via {@linkplain java.util.logging.LoggingMXBean} and if we could implement that interface somewhere else.
 *  
 * @author hsommer
 * created May 10, 2005 12:47:16 PM
 */
public class AcsLogManager extends LogManager {

    private final Thread dummyHook = new DummyHook();
    

    /**
     * Normal base class impl during normal operation, but no-op implementation during shutdown.
     * This serves as a workaround to escape from resetting all loggers 
     * during a Ctrl-C shutdown, as triggered by <code>java.util.logging.LogManager.Cleaner</code>.
     * <p>
     * To actually get the reset functionality (probably never needed in ALMA/ACS), call {@link #resetReally()}.
     *  
     * @see java.util.logging.LogManager#reset()
     */
    public void reset() throws SecurityException {
        if (!isShuttingDown()) {
            super.reset();
        }
    }

    /**
     * Checks if the JVM is shutting down.
     * Exploits the fact that adding a shutdown hook causes an IllegalStateException while shutting down.
     * Better techniques are welcome!
     * @return true if the JVM is shutting down, e.g. after getting a Ctrl-C.
     */
    private boolean isShuttingDown() {
        boolean isShuttingDown = false;
        try {
            Runtime.getRuntime().addShutdownHook(dummyHook);
            Runtime.getRuntime().removeShutdownHook(dummyHook);
        } catch (IllegalStateException e) {
            isShuttingDown = true;
        } catch (Throwable thr) {
            thr.printStackTrace();
        }
        return isShuttingDown;
    }
    
    
    /**
     * @see java.util.logging.LogManager#reset()
     */
    public void resetReally() throws SecurityException {
        super.reset();
    }

    private static class DummyHook extends Thread {
        public void run() {
            // should never run
        }
    }

//    public boolean addLogger(Logger logger) {
//        System.out.println("AcsLogManager: adding logger " + logger.getName());
//        return super.addLogger(logger);
//    }
}
