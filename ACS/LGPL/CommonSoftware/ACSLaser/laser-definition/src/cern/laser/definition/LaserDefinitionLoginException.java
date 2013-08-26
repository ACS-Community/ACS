package cern.laser.definition;


/** Laser definition login exception. Thrown when the user can not be authenticated.
 */
public class LaserDefinitionLoginException extends LaserDefinitionException {
  /** Default constructor.
   */
  public LaserDefinitionLoginException() {
    super();
  }

  /** Constructor.
   * @param message the text message
   */
  public LaserDefinitionLoginException(String message) {
    super(message);
  }

  /** Constructor.
   * @param message the text message
   * @param t the root cause exception
   */
  public LaserDefinitionLoginException(String message, Throwable t) {
    super(message, t);
  }
}
