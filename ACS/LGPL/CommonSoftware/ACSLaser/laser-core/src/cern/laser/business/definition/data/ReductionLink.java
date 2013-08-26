package cern.laser.business.definition.data;
import java.io.Serializable;

public class ReductionLink implements Serializable
{
  private AlarmDefinition parent;
  private AlarmDefinition child;

  public ReductionLink(AlarmDefinition parent, AlarmDefinition child)
  {
    setParent(parent);
    setChild(child);
  }

  public AlarmDefinition getParent()
  {
    return parent;
  }

  public AlarmDefinition getChild()
  {
    return child;
  }

  public void setChild(AlarmDefinition newChild)
  {
    child = newChild;
  }

  public void setParent(AlarmDefinition newParent)
  {
    parent = newParent;
  }

  public String toString() 
  {
    return "[" + parent.getAlarmId() + "," + child.getAlarmId() + "]";
  }
  
  public boolean equals(Object obj) 
  {
    if (obj == null) 
    {
      return false;
    }
    if (!(obj instanceof ReductionLink)) 
    {
      return false;
    }
    ReductionLink link = (ReductionLink)obj;
    
    return (getParent().equals(link.getParent()) && getChild().equals(link.getChild()));
  }

  public int hashCode()
  {
    return toString().hashCode();
  }

}