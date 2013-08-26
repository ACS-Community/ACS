package cern.laser.definition;


/** Laser definition XML exception. Thrown when an error is encountererd while unmarshelling XML definitions.
 */
public class LaserDefinitionXMLException extends LaserDefinitionException {
  /** Default constructor.
   */
  public LaserDefinitionXMLException() {
    super();
  }

  /** Constructor.
   * @param message the text message
   */
  public LaserDefinitionXMLException(String message) {
    super(message);
  }

  /** Constructor.
   * @param message the text message
   * @param t the root cause exception
   */
  public LaserDefinitionXMLException(String message, Throwable t) {
    super(message, t);
  }
}
