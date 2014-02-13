package cern.laser.util.buffer;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicBoolean;

import org.apache.log4j.Logger;

/** A buffering utility class.
 * @author F.Calderini
 */
public class SynchroBuffer {
  private static final Logger LOGGER = Logger.getLogger(SynchroBuffer.class.getName());
  
  private long minWindowSize;
  private long maxWindowSize;
  private int windowGrowthFactor;
  private int duplicatePolicy;

  private Thread checkingThread;
  
  private AtomicBoolean closed = new AtomicBoolean(false);
  private AtomicBoolean firing = new AtomicBoolean(false);
  private AtomicBoolean enabled = new AtomicBoolean(false);
    
  private SynchroBufferListener listener = null;
    
  /** The buffer */
  private List buffer = null;
   
  /** Allows object duplication.
   */
  public static final int DUPLICATE_OK = 1;
  /** Replaces duplicated objects.
   */
  public static final int DUPLICATE_REPLACE = 2;
  /** Discards duplicated objects.
   */
  public static final int DUPLICATE_DISCARD = 3;
  
  /** Constructor.
   * @param minWindowSize the buffer window min size (msec)
   * @param maxWindowSize the buffer window max size (msec)
   * @param windowGrowthFactor the buffer window growth factor (size = minWindowSize + msg/sec x windowGrowthFactor)
   * @param duplicatePolicy the buffer object duplication policy
   */
  public SynchroBuffer(long minWindowSize, long maxWindowSize, int windowGrowthFactor, int duplicatePolicy) {
    init(minWindowSize, maxWindowSize, windowGrowthFactor, duplicatePolicy);
  }
  
  /** Default constructor. Initialisation is made via properties.
   * It reads the configuration from the resource config file specified via the system 
   * property <code>syncrobuffer.properties</code>. If not defined, it looks for the default config file 
   * <code>synchrobuffer-config.properties</code>. System properties override the configuration loaded from
   * the properties file. Configuration properties are :
   * <UL>
   * <LI>synchrobuffer.minwindowsize</LI> (msec, default 500)
   * <LI>synchrobuffer.maxwindowsize</LI> (msec, default 5000)
   * <LI>synchrobuffer.windowgrowthfactor</LI> (windowSize = minWindowSize + msg/sec x windowGrowthFactor, default 100)
   * <LI>synchrobuffer.duplicatepolicy</LI> (default SynchroBuffer.DUPLICATES_OK)
   * </UL>
   */
  public SynchroBuffer() {
    Properties properties = SynchroBufferConfig.getProperties(this.getClass().getClassLoader());
    long min_window_size = Long.parseLong(properties.getProperty(SynchroBufferConfig.MIN_WINDOW_SIZE_PROPERTY));
    long max_window_size = Long.parseLong(properties.getProperty(SynchroBufferConfig.MAX_WINDOW_SIZE_PROPERTY));
    int window_growth_factor = Integer.parseInt(properties.getProperty(SynchroBufferConfig.WINDOW_GROWTH_FACTOR_PROPERTY));
    int duplicate_policy = Integer.parseInt(properties.getProperty(SynchroBufferConfig.DUPLICATE_POLICY_PROPERTY));
    init(min_window_size, max_window_size, window_growth_factor, duplicate_policy);
  }
    
  private void init(long minSize, long maxSize, int growthFactor, int policy) {
    System.out.println("SynchroBuffer[minWindowSize=" + minSize + ",maxWindowSize=" + maxSize + ",windowGrowthFactor=" + growthFactor + ",duplicatePolicy=" + (policy == SynchroBuffer.DUPLICATE_DISCARD ? "DUPLICATE_DISCARD" : (policy == SynchroBuffer.DUPLICATE_REPLACE ? "DUPLICATE_REPLACE" : "DUPLICATE_OK")) + "]");
    if ( (minSize <= 0) || (maxSize <= 0) || (growthFactor <= 0) ) {
      throw(new IllegalArgumentException("arguments must be greater than zero"));
    } else if ( maxSize <= minSize ) {
      throw(new IllegalArgumentException("maximum window size must be greater than minimum window size"));
    } else {
      this.minWindowSize = minSize;
      this.maxWindowSize = maxSize;
      this.windowGrowthFactor = growthFactor;
      this.duplicatePolicy = policy;
      buffer = new ArrayList();
      checkingThread = createCheckingThread();
      checkingThread.start();
    }
  }

  private Thread createCheckingThread() {
    return new Thread() {
      public void run() {
        float objects_per_sec;
        long calculated_window_size;
        long firing_time = 0;
        long wait_time = minWindowSize;
        while ( (!closed.get()) || (!isEmpty() && enabled.get()) ) {
          if (enabled.get()) {
            objects_per_sec = (1000 * buffer.size()) / (wait_time + firing_time);
            calculated_window_size = minWindowSize + ((long)(windowGrowthFactor * objects_per_sec));
            wait_time = ( (calculated_window_size < maxWindowSize) ? calculated_window_size : maxWindowSize );
            firing_time = fire();
            try {
              Thread.sleep(wait_time);
            } catch (InterruptedException ie) {}
          } else {
            try {
              Thread.sleep(maxWindowSize);
            } catch (InterruptedException ie) {}
          }
        }
      }
    };
  }
    
  private long fire() {
    firing.set(true);
    Collection pulled = null;
    synchronized(buffer) {
      pulled = (Collection) ((ArrayList)buffer).clone();
      buffer.clear();
    }
    long time_before = System.currentTimeMillis();
    if (listener != null) {
      if (pulled.size() > 0) {
        try {
          listener.pull(new PullEvent(this, pulled));
        } catch (PullException pe) {
          pe.printStackTrace();
        }
      }
    }
    long time_after = System.currentTimeMillis();
    long time_elapsed = time_after-time_before;
    firing.set(false);
    
    return time_elapsed;
  }
    
  /** Push an object into the buffer. If the duplicate policy is DUPLICATE_DISCARD the object
   * is discarded if the buffer already contains it. If the duplicate policy is DUPLICATE_REPLACE
   * the object replaces any previously pushed duplicated instance. The object is appended otherwise.
   * Equals method is used to determine duplications.
   * @param o the object to push
   */
  public void push(Object object) {
    if (closed.get()) {
      throw new IllegalArgumentException("buffer closed");
    }
    synchronized(buffer) {
      switch (duplicatePolicy) {
        case SynchroBuffer.DUPLICATE_DISCARD :
          if (!buffer.contains(object)) {
            buffer.add(object);
          }
          break;
        case SynchroBuffer.DUPLICATE_REPLACE :
          int index = buffer.indexOf(object);
          if (index == -1) {
            buffer.add(object);
          } else {
            buffer.set(index, object);
          }
          break;
        default :
          buffer.add(object);
      };
    }
  }
    
  /** Push a collection of objects into the buffer.
   * @param collection the collection of objects to push
   */
  public void push(Collection collection) {
    if (closed.get()) {
      throw new IllegalArgumentException("buffer closed");
    }
    if ( (collection != null) && (collection.size() != 0) ) {
      synchronized(buffer) {
        if ( (duplicatePolicy != SynchroBuffer.DUPLICATE_DISCARD) && (duplicatePolicy != SynchroBuffer.DUPLICATE_REPLACE) ) {
          buffer.addAll(collection);
        } else {
          Iterator iterator = collection.iterator();
          while (iterator.hasNext()) {
            push(iterator.next());
          }
        }
      }
    }
  }
    
  /** Set the buffer consumer listener.
   * @param listener the listener
   */
  public void setSynchroBufferListener(SynchroBufferListener listener) {
    this.listener = listener;
  }
    
  /** Enable the listener. The listener is disabled by default.
   */
  public void enable() {
    enabled.set(true);
  }

  /** Disable the listener. Pushed object are kept in the buffer and delivered when the listener is enabled.
   */
  public void disable() {
    enabled.set(false);
  }

  private boolean isEmpty() {
    synchronized(buffer) {
      return buffer.isEmpty();
    }
  }
    
  /** Close the buffer and deallocate resources.
   */
  public void close() {
	  boolean wasClosed=closed.getAndSet(true);
	  if (wasClosed) {
		  return;
	  }
    while ((!isEmpty() && enabled.get()) || firing.get()) {
      try {
        Thread.sleep(minWindowSize);
      } catch (Exception e) {}
    }
  }
    
}

