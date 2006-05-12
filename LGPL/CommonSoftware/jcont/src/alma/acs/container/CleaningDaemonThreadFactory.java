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
package alma.acs.container;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import edu.emory.mathcs.backport.java.util.concurrent.ThreadFactory;
import edu.emory.mathcs.backport.java.util.concurrent.atomic.AtomicInteger;


/**
 * Thread factory that remembers all threads it creates.
 * Method <code>cleanUp</code> allows killing all threads.  
 * <p>
 * A <code>ThreadGroup</code> is used to deal with all threads at a time. Its name is that given in the constructor.
 * Even though Josh Bloch tells us in "Effective Java", Item 53, that thread groups are almost useless,
 * we like the UncaughtExceptionHandler which JDK 1.4 only offers through ThreadGroups.
 * TODO: revisit this in JDK 1.5 where Thread itself has this handler.
 * 
 * @author hsommer
 * created Mar 8, 2005 1:27:38 PM
 */
public class CleaningDaemonThreadFactory implements ThreadFactory {

    private LoggingThreadGroup group;
    
    // we keep track of our threads outside of the thread group
    private List<Thread> threadList = new ArrayList<Thread>();
    
    private final AtomicInteger threadNumber = new AtomicInteger(1);
    
    private Logger logger;
    private String name;
    private ClassLoader newThreadContextCL;
    
//    private boolean DEBUG = true;

    
    /**
     * Constructor.
     * @param name  the name of the {@link ThreadGroup} to which all threads created by this factory will belong.
     * @param logger  the logger to be used by this class
     */
    public CleaningDaemonThreadFactory(String name, Logger logger) {        
        this.logger = logger;
        this.name = name;
    }
    
    /**
     * Creates a new daemon thread that is part of the same factory thread group 
     * as all other threads created by this method.
     * The thread's name will be that of the group, with an integer value appended.
     * 
     * @see edu.emory.mathcs.backport.java.util.concurrent.ThreadFactory#newThread(java.lang.Runnable)
     */
    public Thread newThread(Runnable command) {
        if (group == null) {
            group = new LoggingThreadGroup(name, logger);
            group.setMaxPriority(Thread.currentThread().getPriority()); //Thread.NORM_PRIORITY
        }

        String threadName = group.getName() + "-" + threadNumber.getAndIncrement();
        Thread t = new Thread(group, command, threadName);
        t.setDaemon(true);
        if (newThreadContextCL != null) {
            t.setContextClassLoader(newThreadContextCL);
        }
        threadList.add(t);
        return t;
    }

    
    void setNewThreadContextClassLoader(ClassLoader cl) {
        if (cl != null) {
            newThreadContextCL = cl;
        }
    }
    
    
    /**
     * Kills running threads via {@link Thread#stop()}.
     * Should be called by tbe container or similar classes when all threads 
     * created by this factory supposedly have terminated anyway thanks to smart applications. 
     * The safety concerns which led to the deprecation of the stop method thus don't seem to apply here.
     */
    public void cleanUp() {
        if (group == null) {
            return;
        }
        group.setShuttingDown();
        Thread t = null;
        for (Iterator<Thread> iter = threadList.iterator(); iter.hasNext();) {
            try {
                t = iter.next();
                if (t.isAlive()) {
                    logger.warning("forcefully terminating surviving thread " + t.getName());
                }
                t.stop();
            } catch (RuntimeException e) {
                logger.finer("exception while stopping thread '" + t.getName() + "': " + e.toString());
            } 
        }
        threadList.clear();
        try {
            // if there are threads in the group which have never started, destroy() will fail
            if (!group.isDestroyed() && group.activeCount() == 0 && group.activeGroupCount() == 0) {
                group.destroy();
            }
        } catch (Exception e) {
            logger.finer("unexpectedly failed to destroy thread group " + group.getName() + e.toString());
        }
        // in case this factory gets used again, we should build a new thread group
        group = null;
    }

    

    private static class LoggingThreadGroup extends ThreadGroup {
        private Logger logger;
        private boolean shuttingDown = false;

        LoggingThreadGroup(String name, Logger logger) {
            super(name);
            this.logger = logger;
        }

        void setShuttingDown() {
            shuttingDown = true;
        }
        
        /**
         * Called by the JVM if any of the threads in this thread group terminates with an error.
         * Logs a warning to the logger provided in the ctor.
         * <p>
         * The error <code>ThreadDeath</code> is even logged during expected thread lifetime,
         * because user threads are not supposed to be terminated through the deprecated <code>stop()</code> method
         * during their normal operation. <br>
         * During shutdown, exceptions are not logged, because the killing of surviving user threads
         * is logged already in the <code>cleanUp</code> method. 
         * 
         * @see java.lang.ThreadGroup#uncaughtException(java.lang.Thread, java.lang.Throwable)
         */
        public void uncaughtException(Thread t, Throwable e) {
            if (!shuttingDown) {
                logger.log(Level.WARNING, "User thread '" + t.getName() + "' terminated with error ", e);
            }
            // ThreadDeath must move on to really let the thread die
            if (e instanceof ThreadDeath) {
                super.uncaughtException(t, e);
            }
        }
        
    }
}
