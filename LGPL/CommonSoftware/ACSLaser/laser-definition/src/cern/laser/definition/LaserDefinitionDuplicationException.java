package cern.laser.definition;


/** Laser definition duplication exception. Thrown when trying to create a definition
 * that already exists.
 */
public class LaserDefinitionDuplicationException extends LaserDefinitionException {
  /** Default constructor.
   */
  public LaserDefinitionDuplicationException() {
    super();
  }

  /** Constructor.
   * @param message the text message
   */
  public LaserDefinitionDuplicationException(String message) {
    super(message);
  }

  /** Constructor.
   * @param message the text message
   * @param t the root cause exception
   */
  public LaserDefinitionDuplicationException(String message, Throwable t) {
    super(message, t);
  }
}
