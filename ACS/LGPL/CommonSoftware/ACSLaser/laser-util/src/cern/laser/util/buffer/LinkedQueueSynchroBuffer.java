/*
 * $RCSfile: LinkedQueueSynchroBuffer.java,v $
 *
 * Copyright (C) 2004 European Organization for Nuclear Research.
 *
 */

package cern.laser.util.buffer;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import org.apache.log4j.Logger;

import EDU.oswego.cs.dl.util.concurrent.LinkedQueue;

/**
 * A buffer class which uses an Unbounded linked list implementation with the
 * similar function signatures to SynchroBuffer.
 * 
 * @author Niall Stapley
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:37 $
 */
public class LinkedQueueSynchroBuffer {

  /** For logging. */
  private static final Logger LOGGER = Logger.getLogger(LinkedQueueSynchroBuffer.class);

  /** The single listener to which elements must be delivered. */
  private SynchroBufferListener listener = null;

  //	/** Starts the processing when enabled. */
  //	private final Latch enabled = new Latch();

  /** The buffer. */
  private LinkedQueue buffer = new LinkedQueue();

  /** The thread to run to delivery to the listener. */
  private Thread postman = new Thread(new Postman());

  /**
   * The constructor.
   *  
   */
  public LinkedQueueSynchroBuffer() {
    super();

  }

  /**
   * Starts a new thread to take from the tail of the list and deliver the
   * element to the listener.
   */
  private class Postman implements Runnable {
    public void run() {
			/*
			 * If we really need to check that delivery is enabled each time,
			 * then we could use a lock. But we would have to check on _each_
			 * delivery that the lock was available. We should only do this if
			 * necessary.
			 */
			PullEvent p;
			Collection c;
			while (true) {
				c = new ArrayList(1);
				/* If interrupted then null is returned, so just try again. */
				try {
					c.add(buffer.take());
				} catch (InterruptedException e) {
					continue;
				}
				
				LOGGER.debug("take() returned an element.");
				/* This is thrown by consumer for circumstances not
				   specified, but what are we supposed to do about it? */
				try { 
					listener.pull(new PullEvent(this, c));
					LOGGER.debug("Delivered element to listener.");
			    } catch (PullException pe) {
			    	LOGGER.error("Error while delivering element.", pe);
			    }
			}
		}
  } // class

  /**
   * Push an object into the buffer.
   * 
   * @param o the object to push
   */
  public void push(Object object) {

    // keep trying if thread is interrupted
    while (true) {
      try {
        buffer.put(object);
        break;
      } catch (InterruptedException e) {
        continue;
      }
    }
  }

  /**
   * Atempt to take an object from the end buffer. There is a chance this was
   * interrupted and null is returned.
   * 
   * @return o the object
   *  
   */
  public Object take() {
    Object o = null;
    while (true) {
      try {
        o = buffer.take();
        break;
      } catch (InterruptedException e) {
        continue;
      }
    }
    return o;
  }

  /**
   * Push a collection of objects into the buffer.
   * 
   * @param collection the collection of objects to push
   */
  public void push(Collection collection) {
    Iterator iterator = collection.iterator();
    while (iterator.hasNext()) {
      push(iterator.next());
    }
  }

  /**
   * Set the buffer consumer listener.
   * 
   * @param listener the listener
   */
  public void setSynchroBufferListener(SynchroBufferListener listener) {
    this.listener = listener;
  }

  /**
   * Enable the listener. The listener is disabled by default. In this class, it
   * should only be called once after the listener has been set to start
   * receiving elements.
   */
  public void enable() {
    postman.start();
  }

  /**
   * Disable the listener. Pushed object are kept in the buffer and delivered
   * when the listener is enabled. In this class, this is not implemented, if
   * starting and stopping the listener input is required, use take() instead,
   * as this gives full control to the consumer as to when it takes an element.
   */
  public void disable() {
    // not implemented but could do it with lock or postname.stop() or
    // postman.interrupt();
    LOGGER.error("disable() called, but not implemented");
  }

  /**
   * Close the input (head) end of the queue. It cannot be opened again. In this
   * class, this is not implemented currently.
   */
  public void close() {
    LOGGER.error("close() called, but it is not implemented.");
  }

}