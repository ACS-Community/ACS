package cern.laser.guiplatform.alarms.helpers;
import java.sql.Timestamp;
import java.util.Calendar;
import java.util.Properties;

import cern.laser.client.data.Status;

public class StatusImpl implements Status, Cloneable
{
  private static long timeMillis = -1;
  static {
    Calendar calendar = Calendar.getInstance();
    timeMillis = calendar.getTime().getTime();
  }
  private boolean active;
  private boolean masked;
  private boolean reduced;
  private Timestamp sourceTimestamp;
  private String sourceHostname;
  private Timestamp userTimestamp;
  private Timestamp systemTimestamp;
  private Properties userProperties;

  //public StatusImpl(cern.laser.business.data.Status status)
  public StatusImpl(boolean active, boolean masked, boolean reduced)
  {
    this.active = active;
    this.reduced = reduced;
    this.masked = masked;
  
  sourceTimestamp = new Timestamp(timeMillis);
  sourceHostname = "sourceHostname";
  userTimestamp = new Timestamp(timeMillis);
  systemTimestamp = new Timestamp(timeMillis);
  userProperties = new Properties();
  userProperties.put("property1", "property1");
  userProperties.put("property2", "");
  userProperties.put("property3", "property3");
  userProperties.put("property4", "");
  /*
    if (status == null) 
    {
      throw new IllegalArgumentException("parameter can not be null");
    }
    active = status.getActive().booleanValue();
    masked = status.getMasked().booleanValue();
    reduced = status.getReduced().booleanValue();
    sourceHostname = status.getSourceHostname();
    sourceTimestamp = status.getSourceTimestamp();
    userTimestamp = status.getUserTimestamp();
    systemTimestamp = status.getSystemTimestamp();
    userProperties = status.getUserProperties();
    */
  }
  public StatusImpl(boolean active) 
  {
    this(active, false, false);
  }

  private StatusImpl(Status status)
  {
    if (status == null) 
    {
      throw new IllegalArgumentException("parameter can not be null");
    }
    active = status.isActive();
    masked = status.isMasked();
    reduced = status.isReduced();
    sourceHostname = new String(status.getSourceHostname());
    sourceTimestamp = (Timestamp)status.getSourceTimestamp().clone();
    userTimestamp = (Timestamp)status.getUserTimestamp().clone();
    systemTimestamp = (Timestamp)status.getSystemTimestamp().clone();
    userProperties = new Properties(status.getUserProperties());
  }

  public boolean isActive()
  {
    return active;
  }


  public boolean isMasked()
  {
    return masked;
  }


  public boolean isReduced()
  {
    return reduced;
  }


  public String getSourceHostname()
  {
    return sourceHostname;
  }


  public Timestamp getSourceTimestamp()
  {
    return sourceTimestamp;
  }


  public Timestamp getUserTimestamp()
  {
    return userTimestamp;
  }


  public Timestamp getSystemTimestamp()
  {
    return systemTimestamp;
  }


  public Properties getUserProperties()
  {
    return userProperties;
  }

  public Object clone() throws CloneNotSupportedException 
  {
    return new StatusImpl(this);
  }  
  
  public String toString() 
  {
    StringBuffer str_buf = new StringBuffer();
    str_buf.append("\nACTIVE : ");
    str_buf.append(isActive());
    str_buf.append("\nMASKED : ");
    str_buf.append(isMasked());
    str_buf.append("\nREDUCED : ");
    str_buf.append(isReduced());
    str_buf.append("\nSOURCE HOSTNAME : ");
    str_buf.append(getSourceHostname());
    str_buf.append("\nSOURCE TIMESTAMP : ");
    str_buf.append(getSourceTimestamp());
    str_buf.append("\nUSER TIMESTAMP : ");
    str_buf.append(getUserTimestamp());
    str_buf.append("\nSYSTEM TIMESTAMP : ");
    str_buf.append(getSystemTimestamp());
    str_buf.append("\nUSER PROPERTIES : \n");
    str_buf.append(getUserProperties());

    return str_buf.toString();
  }

}
