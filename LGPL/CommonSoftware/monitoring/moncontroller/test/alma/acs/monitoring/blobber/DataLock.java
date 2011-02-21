package alma.acs.monitoring.blobber;

import java.util.logging.Logger;

/**
 * A "stack" of size one for a given data item: 
 * First the data must be stored (in method {@link #put(Object)}), 
 * then it can be alternately be popped (method {@link #take()} and pushed again. 
 * The pushing and popping get blocked until the stack is free or full respectively.
 * <p>
 * @TODO replace with ArrayBlockingQueue(1)
 */
public class DataLock<C> {

    private C myData;

    private boolean myDataAvailable;

	private final Logger logger;

	private final String name;

    /**
     * @param logger
     * @param name  Used for debug logs
     */
    public DataLock(Logger logger, String name) {
    	this.logger = logger;
    	this.name = name;
    }
    
    public synchronized void put(C inData) {
    	logger.finer("DataLock " + name + ": Got a call to put(" + inData.getClass().getSimpleName() + "); thread = " + Thread.currentThread().getName() + "; myDataAvailable = " + myDataAvailable);
        try {
            while (this.myDataAvailable) {
                wait();
            }
            this.myDataAvailable = true;
            this.myData = inData;
            notify();
        } catch (InterruptedException e) {
        	logger.finer("DataLock " + name + ": Got InterruptedException in put(" + inData.getClass().getSimpleName() + ").");
        }
    	logger.finer("DataLock " + name + ": Returning from put(" + inData.getClass().getSimpleName() + ").");
    }

    public synchronized C take() {
    	logger.finer("DataLock " + name + ": Got a call to take(); thread = " + Thread.currentThread().getName() + "; myDataAvailable = " + myDataAvailable);
        try {
            while (!this.myDataAvailable) {
                wait();
            }
            this.myDataAvailable = false;
            notify();
        } catch (InterruptedException e) {
        	logger.finer("DataLock " + name + ": Got InterruptedException in take()");
        }
    	logger.finer("DataLock " + name + ": Returning from take().");
        return this.myData;
    }
}
