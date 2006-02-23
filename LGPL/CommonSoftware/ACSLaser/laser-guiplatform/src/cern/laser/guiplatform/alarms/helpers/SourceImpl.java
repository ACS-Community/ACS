package cern.laser.guiplatform.alarms.helpers;

import cern.laser.client.data.ResponsiblePerson;
import cern.laser.client.data.Source;

public class SourceImpl implements Source, Cloneable
{
  private String sourceId;
  private String name;
  private String description;
  //private Location location;
  private ResponsiblePerson resposiblePerson;
  
  //public SourceImpl(cern.laser.business.data.Source source)
  public SourceImpl()
  {
    sourceId = "-1000";
    name = "";
    description = "";
    //location = new LocationImpl();
    resposiblePerson = new ResponsiblePersonImpl();
    /*
    if (source == null) 
    {
      throw new IllegalArgumentException("parameter can not be null");
    }
    sourceId = source.getSourceId().intValue();
    name = source.getName();
    description = source.getDescription();
    */
  }

  private SourceImpl(Source source)
  {
    if (source == null) 
    {
      throw new IllegalArgumentException("parameter can not be null");
    }
    sourceId = source.getSourceId();
    name = new String(source.getName());
    description = new String(source.getDescription());
    //location = source.getLocation();
    resposiblePerson = source.getResponsiblePerson();
  }

  public String getSourceId()
  {
    return sourceId;
  }

  public String getName()
  {
    return name;
  }

  public String getDescription()
  {
    return description;
  }

  //public Location getLocation() {
  //    return location;
  //}
  
  public ResponsiblePerson getResponsiblePerson() {
      return resposiblePerson;
  }
  
  public Object clone() throws CloneNotSupportedException 
  {
    return new SourceImpl(this);
  }  
  
  public String toString() 
  {
    StringBuffer str_buf = new StringBuffer();
    str_buf.append("\nID : ");
    str_buf.append(getSourceId());
    str_buf.append("\nNAME : ");
    str_buf.append(getName());
    str_buf.append("\nDESCRIPTION : ");
    str_buf.append(getDescription());
    
    return str_buf.toString();
  }

}
