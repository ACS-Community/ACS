/*
 * ASIException.java
 *
 * Created on March 6, 2002, 5:54 PM
 */
package cern.laser.source.alarmsysteminterface;


/**
 * Exception class.
 * @author  fracalde
 * @version 1.0
 */
public class ASIException extends java.lang.Exception {
  private Throwable cause = null;

  /**
   * Create new <code>ASIException</code>.
   */
  public ASIException() {
    super();
  }

  /**
   * Construct an <code>ASIException</code> with the specified detail message.
   * @param msg the detail message.
   */
  public ASIException(String msg) {
    super(msg);
  }

  /**
   * Record that the root cause of this ASIException.
   * @param t The possibly null exception that caused the ASI operation to fail. If null, it means this ASI exception has no root cause.
   */
  public void setRootCause(Throwable t) {
    cause = t;
  }

  /**
   * Retrieve the root cause of this ASIException, if any.
   * @return The possibly null exception that caused the ASI operation to fail. If null, it means this ASI exception has no root cause.
   */
  public Throwable getRootCause() {
    return cause;
  }
}
