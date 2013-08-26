package cern.laser.definition;


/** Laser definition generic exception. */
public class LaserDefinitionException extends Exception {
  private Throwable cause = null;

  /** Default constructor.
   */
  public LaserDefinitionException() {
    super();
  }

  /** Constructor.
   * @param message the text message
   */
  public LaserDefinitionException(String message) {
    super(message);
  }

  /** Constructor.
   * @param message the text message
   * @param t the root cause exception
   */
  public LaserDefinitionException(String message, Throwable t) {
    super(message);
    cause = t;
  }

  /** Return the root exception.
   * @return the root exception, null if none
   */
  public Throwable getCause() {
    if (cause == null) {
      return super.getCause();
    }

    return cause;
  }
}
