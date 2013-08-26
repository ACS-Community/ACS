package cern.laser.definition;


/** Laser definition not found exception. Thrown when referencing a non existing
 * definition.
 */
public class LaserDefinitionNotFoundException extends LaserDefinitionException {
  /** Default constructor.
   */
  public LaserDefinitionNotFoundException() {
    super();
  }

  /** Constructor.
   * @param message the text message
   */
  public LaserDefinitionNotFoundException(String message) {
    super(message);
  }

  /** Constructor.
   * @param message the text message
   * @param t the root cause exception
   */
  public LaserDefinitionNotFoundException(String message, Throwable t) {
    super(message, t);
  }
}
