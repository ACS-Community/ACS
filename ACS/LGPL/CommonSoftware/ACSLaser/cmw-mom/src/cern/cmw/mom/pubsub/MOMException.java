package cern.cmw.mom.pubsub;


/**
 * Generic exception. It may be raised when instantiating a Publisher or Subscriber
 * through the factory class <EM>PubSubFactory</EM> or thrown whenever there is
 * a problem in the underlying JMS communication. In this case the <EM>ExceptionListener</EM>
 * is called passing to the onException method the MOMException with the corresponding
 * exception code.
 * @version 0.9.0   21 Nov 2000
 * @author   Controls Middleware Project
 * @see PubSubFactory
 * @see ExceptionListener
 */
public class MOMException extends Exception {
  /** Field GENERIC_EXCEPTION */
  public final static int GENERIC_EXCEPTION = 0;

  /**
   * Field CONNECTION_LOST. A MOMException with
   *  testException(MOMException.CONNECTION_LOST_EXCEPTION) == true
   *  means that a break in the connection to the broker has been
   *  detected.
   */
  public final static int CONNECTION_LOST_EXCEPTION = 1;

  /**
   * Field CONNECTION_RECOVERED. A MOMException with
   *  testException(MOMException.CONNECTION_RECOVERED) == true
   *  means that a reconnection to the broker has been detected.
   */
  public final static int CONNECTION_RECOVERED_EXCEPTION = 2;
  private int exceptionCode = GENERIC_EXCEPTION;

  /**
   * Creates an empty MOMException.
   */
  public MOMException() {
    super();
  }

  /**
   * Creates a MOMException setting the text.
   * @param s the String value of the text
   */
  public MOMException(String s) {
    super(s);
  }

  /**
   * Creates a MOMException setting the text and the exception code.
   * @param s the String value of the text
   * @param code the exception code
   */
  public MOMException(String s, int code) {
    super(s);

    exceptionCode = code;
  }

  /**
   * Check the exception type. Utilized to check the exception type
   * from the onException handler.
   *
   * @param code the exception code to test
   * @return true if the exception caught corresponds to code
   */
  public boolean testException(int code) {
    return (exceptionCode == code);
  }
}

