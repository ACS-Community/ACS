package cern.laser.definition;


/** Laser definition not allowed exception. Thrown when the user does not have permission
 * to perform a given operation.
 */
public class LaserDefinitionNotAllowedException extends LaserDefinitionException {
  /** Default constructor.
   */
  public LaserDefinitionNotAllowedException() {
    super();
  }

  /** Constructor.
   * @param message the text message
   */
  public LaserDefinitionNotAllowedException(String message) {
    super(message);
  }

  /** Constructor.
   * @param message the text message
   * @param t the root cause exception
   */
  public LaserDefinitionNotAllowedException(String message, Throwable t) {
    super(message, t);
  }
}
