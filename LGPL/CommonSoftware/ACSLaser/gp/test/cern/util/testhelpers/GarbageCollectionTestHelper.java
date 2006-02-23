package cern.util.testhelpers;

import java.lang.ref.PhantomReference;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.Reference;

/**
 * A helper class to check if Garbage Collection works properly.
 * It allows you to check whether an object is freed by the Garbage
 * Collector.
 *
 * To check whether an object is garbage collected, create an instance of this
 * class and pass it the reference the object in to be surveilled for GC.
 * Then, call the method <code>waitForGC()</code> to wait until the object is
 * garbage collected.
 * Here is sample code:
 * <pre><code>
 *      Object obj = new Object();
 *      GarbageCollectionTestHelper gch = new GarbageCollectionTestHelper(obj);
 *
 *          obj = null; // enable garbage collection
 *
 *          if (gch.waitForGC(maxTimeoutMillis)) {
 *              System.out.println("garbage collection ok");
 *          } else {
 *              System.err.println("garbage collection problem");
 *          }
 * </code></pre>
 * You have to use one instance of this class per reference you want to
 * surveil.
 *
 * This implementation isbased on the java.lang.ref.* classes.
 * It keeps a PhantomReference to the reference and waits until
 * the instance has been garbage collected.
 * @version 0.9.1
 * @author Vito Baggiolini
 */
public class GarbageCollectionTestHelper {
    private Reference ref;
    private ReferenceQueue refQ;
    // to variables needed for cancelling:
    private volatile boolean cancelFlag = false;
    private volatile Thread waitingThread;
    private final static long GC_SLICE_MILLIS = 200;
    
    public GarbageCollectionTestHelper(Object refToSurveil) {
        refQ = new ReferenceQueue();
        ref = new PhantomReference(refToSurveil, refQ);
    }
    public boolean waitForGC() {
        waitingThread = Thread.currentThread();
        Reference retRef = null;
        
        System.gc();
        while(true) {
            try {
                System.err.println("refQ:waiting to remove()"); //@
                retRef = refQ.remove();
                System.err.println("refQ:removed()"); //@
                break;
            } catch (InterruptedException ex) {
                if (cancelFlag) { break; }
                // ignore and continue
                continue;
            }
        }
        waitingThread = null;
        return (retRef != null);
    }
    
    /**
     * a simple algorithm to wait for GC
     */
    public boolean waitForGC(long maxTimeout) {
        long startTime = System.currentTimeMillis();
        long timeOut = maxTimeout;
        Reference retRef = null;
        
        System.gc();
        while(true) {
            try {
                retRef = refQ.remove(timeOut);
                break;
            } catch (InterruptedException ex) {
                long delta = System.currentTimeMillis() - startTime;
                if (delta < maxTimeout) {
                    timeOut = maxTimeout - delta;
                    continue;
                }
            } // catch
        } // while
        
        return (retRef != null);
    }
    
    /**
     * a more sophisticated algorithm to wait for Property Change Events
     */
    public boolean complexWaitForGC(long maxTimeout) {
        long startTime = System.currentTimeMillis();
        long timeOut = maxTimeout;
        Reference retRef = null;
        
        int slices = (int) (maxTimeout/GC_SLICE_MILLIS);
        System.err.println("waiting for " + slices + " slices"); //@
        for (int ix=0; ix< slices; ix++) {
            System.gc();
            System.err.println("sleeping for " + GC_SLICE_MILLIS); //@
            try { Thread.currentThread().sleep(GC_SLICE_MILLIS); } catch (InterruptedException ex) { ex.printStackTrace(); }
            retRef = refQ.poll();
            if (retRef != null) { return true; }
        } // while
        
        return (retRef != null);
    }
    
    public void cancel() {
        cancelFlag = true;
        waitingThread.interrupt();
    }
}