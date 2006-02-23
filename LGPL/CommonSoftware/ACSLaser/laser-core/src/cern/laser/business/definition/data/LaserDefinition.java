package cern.laser.business.definition.data;
import cern.laser.business.definition.LaserDefinitionNotValidException;

public interface LaserDefinition 
{
  public void validate() throws LaserDefinitionNotValidException;
}