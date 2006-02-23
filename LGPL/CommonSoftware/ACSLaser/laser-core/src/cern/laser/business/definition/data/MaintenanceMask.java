package cern.laser.business.definition.data;
import java.io.Serializable;
import java.util.Date;

public class MaintenanceMask implements Serializable
{
  private Date begin;
  private Date end;

  public MaintenanceMask(Date begin, Date end)
  {
    if ((begin == null) || (end == null)) 
    {
      throw new IllegalArgumentException("parameter is null");
    }
    this.begin = begin;
    this.end = end;
  }

  public Date getBegin()
  {
    return begin;
  }

  public void setBegin(Date newBegin)
  {
    if (newBegin == null)
    {
      throw new IllegalArgumentException("parameter is null");
    }
    begin = newBegin;
  }

  public Date getEnd()
  {
    return end;
  }

  public void setEnd(Date newEnd)
  {
    if (newEnd == null)
    {
      throw new IllegalArgumentException("parameter is null");
    }
    end = newEnd;
  }

  public String toString() 
  {
    return "[" + begin.toString() + "," + end.toString() + "]";
  }
  
  public boolean equals(Object obj) 
  {
    if (obj == null) 
    {
      return false;
    }
    if (!(obj instanceof MaintenanceMask)) 
    {
      return false;
    }
    MaintenanceMask mask = (MaintenanceMask)obj;
    
    return (getBegin().equals(mask.getBegin()) && getEnd().equals(mask.getEnd()));
  }

  public int hashCode()
  {
    return toString().hashCode();
  }

}