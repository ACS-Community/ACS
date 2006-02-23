package cern.laser.util.buffer;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import org.apache.log4j.Logger;

/**
 * A buffering utility class. It is an adapter for a SynchroBuffer instance allowing to detach the buffer consumer
 * thread from the SynchroBuffer itself for slow consumers.
 * 
 * @author F.Calderini
 */
public class SynchroBufferAdapter {
  private static final Logger LOGGER = Logger.getLogger(SynchroBufferAdapter.class.getName());
  private List queue;
  private SynchroBuffer adaptee;
  private SynchroBufferListener listener;
  private Thread pullingThread;

  private Boolean enabled = Boolean.TRUE;
  private Boolean closed = Boolean.FALSE;
  private Boolean firing = Boolean.FALSE;
  private Boolean semaphore = Boolean.FALSE;

  /**
   * Constructor. The listener is disabled by default.
   * 
   * @param listener the buffer listener
   * @param buffer the SynchroBuffer instance
   */
  public SynchroBufferAdapter(SynchroBufferListener listener, SynchroBuffer buffer) {
    this.listener = listener;
    adaptee = buffer;
    adaptee.setSynchroBufferListener(createSynchroBufferListener());
    queue = Collections.synchronizedList(new ArrayList());
    pullingThread = createPullingThread();
    pullingThread.start();
  }

  /**
   * Push an object into the buffer.
   * 
   * @param object the object to push
   */
  public void push(Object object) {
    adaptee.push(object);
  }

  /**
   * Push a collection of objects into the buffer.
   * 
   * @param collection the collection of objects to push
   */
  public void push(Collection collection) {
    adaptee.push(collection);
  }

  /**
   * Enable the listener. The listener is disabled by default.
   */
  public void enable() {
    adaptee.enable();
    setEnabled(true);
    LOGGER.debug("SynchroBufferAdapter listener enabled");
  }

  /**
   * Disable the listener. Pushed object are kept in the buffer and delivered when the listener is enabled.
   */
  public void disable() {
    adaptee.disable();
    setEnabled(false);
    LOGGER.debug("SynchroBufferAdapter listener disabled");
  }

  /**
   * SynchroBufferListener method.
   */
  private SynchroBufferListener createSynchroBufferListener() {
    return new SynchroBufferListener() {
      public void pull(PullEvent event) throws PullException {
        synchronized (queue) {
          queue.add(event);
          semNotify();
        }
      }
    };
  }

  private void fire() {
    if (isEnabled()) {
      setFiring(true);
      //System.out.println("firing...");
      while ((!isEmpty()) && isEnabled()) {
        List queue_copy = new ArrayList();
        synchronized (queue) {
          for (int i = 0; i < queue.size(); i++) {
            queue_copy.add(queue.get(i));
          }
          queue.clear();
        }
        Iterator iterator = queue_copy.iterator();
        while (iterator.hasNext()) {
          PullEvent event = (PullEvent) iterator.next();
          try {
            listener.pull(event);
          } catch (PullException e) {
            e.printStackTrace();
          }
        }
      }
      //System.out.println("fired");
      setFiring(false);
    }
  }

  /**
   * Thread method.
   */
  private Thread createPullingThread() {
    return new Thread() {
      public void run() {
        while ((!isClosed()) || ((!isEmpty()) && isEnabled())) {
          semWait();
          fire();
        }
        System.out.println("SynchroBufferAdapter pulling thread exited");
      }
    };
  }

  private void semNotify() {
    //System.out.println("notifying...");
    synchronized (semaphore) {
      semaphore.notify();
    }
  }

  private void semWait() {
    //System.out.println("waiting...");
    synchronized (semaphore) {
      try {
        semaphore.wait();
      } catch (InterruptedException e) {
      }
    }
  }

  private boolean isEmpty() {
    synchronized (queue) {
      return queue.isEmpty();
    }
  }

  private boolean isEnabled() {
    synchronized (enabled) {
      return enabled.booleanValue();
    }
  }

  private void setEnabled(boolean value) {
    synchronized (enabled) {
      if (value == true) {
        enabled = Boolean.TRUE;
        semNotify();
      } else {
        enabled = Boolean.FALSE;
      }
    }
  }

  private boolean isClosed() {
    synchronized (closed) {
      return closed.booleanValue();
    }
  }

  private void setClosed(boolean value) {
    synchronized (closed) {
      closed = (value ? Boolean.TRUE : Boolean.FALSE);
    }
  }

  private void setFiring(boolean value) {
    synchronized (firing) {
      firing = (value ? Boolean.TRUE : Boolean.FALSE);
    }
  }

  private boolean isFiring() {
    synchronized (firing) {
      return firing.booleanValue();
    }
  }

  /**
   * Close the buffer and deallocate resources.
   */
  public void close() {
    adaptee.close();
    setClosed(true);
    semNotify();
    while (((!isEmpty()) && isEnabled()) || isFiring()) {
      try {
        Thread.sleep(200);
      } catch (Exception e) {
      }
    }
    LOGGER.debug("SynchroBufferAdapter closed");
  }

}