package cern.laser.business.definition;

public class LaserDefinitionException extends Exception
{
  public LaserDefinitionException(String msg)
  {
    super(msg);
  }

  public LaserDefinitionException(Throwable t) {
    super(t);
  }
  
  public LaserDefinitionException(String msg, Throwable t) {
    super(msg, t);
  }
}