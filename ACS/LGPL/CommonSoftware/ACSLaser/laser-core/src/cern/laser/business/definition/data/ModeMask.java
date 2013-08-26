package cern.laser.business.definition.data;
import java.io.Serializable;

public class ModeMask implements Serializable
{
  private String machineMode;

  public ModeMask(String machineMode)
  {
    if (machineMode == null) 
    {
      throw new IllegalArgumentException("parameter is null");
    }
    this.machineMode = machineMode;
  }

  public String getMachineMode()
  {
    return machineMode;
  }

  public void setMachineMode(String newMachineMode)
  {
    if (newMachineMode == null) 
    {
      throw new IllegalArgumentException("parameter is null");
    }
    machineMode = newMachineMode;
  }

  public String toString() 
  {
    return "[" + machineMode + "]";
  }
  
  public boolean equals(Object obj) 
  {
    if (obj == null) 
    {
      return false;
    }
    if (!(obj instanceof ModeMask)) 
    {
      return false;
    }
    ModeMask mask = (ModeMask)obj;
    
    return (getMachineMode().equals(mask.getMachineMode()));
  }

  public int hashCode()
  {
    return toString().hashCode();
  }

}