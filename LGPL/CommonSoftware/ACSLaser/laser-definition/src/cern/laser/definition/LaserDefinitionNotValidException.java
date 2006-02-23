package cern.laser.definition;


/** Laser definition not found exception. Thrown when using a non valid definition.
 */
public class LaserDefinitionNotValidException extends LaserDefinitionException {
  /** Default constructor.
   */
  public LaserDefinitionNotValidException() {
    super();
  }

  /** Constructor.
   * @param message the text message
   */
  public LaserDefinitionNotValidException(String message) {
    super(message);
  }

  /** Constructor.
   * @param message the text message
   * @param t the root cause exception
   */
  public LaserDefinitionNotValidException(String message, Throwable t) {
    super(message, t);
  }
}
