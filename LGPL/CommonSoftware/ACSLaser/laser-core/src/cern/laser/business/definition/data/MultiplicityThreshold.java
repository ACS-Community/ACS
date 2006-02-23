package cern.laser.business.definition.data;
import java.io.Serializable;

public class MultiplicityThreshold implements Serializable
{
  private AlarmDefinition parent;
  private Integer threshold;

  public MultiplicityThreshold(AlarmDefinition parent, Integer threshold)
  {
    setParent(parent);
    setThreshold(threshold);
  }

  public AlarmDefinition getParent()
  {
    return parent;
  }

  public void setParent(AlarmDefinition newParent)
  {
    parent = newParent;
  }

  public Integer getThreshold()
  {
    return threshold;
  }

  public void setThreshold(Integer newThreshold)
  {
    threshold = newThreshold;
  }

  public String toString() 
  {
    return "[" + parent.getAlarmId() + "," + threshold + "]";
  }
  
  public boolean equals(Object obj) 
  {
    if (obj == null) 
    {
      return false;
    }
    if (!(obj instanceof MultiplicityThreshold)) 
    {
      return false;
    }
    MultiplicityThreshold mult_threshold = (MultiplicityThreshold)obj;
    
    return (getParent().equals(mult_threshold.getParent()) && getThreshold().equals(mult_threshold.getThreshold()));
  }

  public int hashCode()
  {
    return toString().hashCode();
  }

}