package cern.laser.business.definition;

public class LaserDefinitionNotFoundException extends LaserDefinitionException {
  public LaserDefinitionNotFoundException(String msg) {
    super(msg);
  }

  public LaserDefinitionNotFoundException(String msg, Throwable t) {
    super(msg, t);
  }
}