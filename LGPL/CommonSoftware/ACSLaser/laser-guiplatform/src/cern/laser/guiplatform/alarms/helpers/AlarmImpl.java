package cern.laser.guiplatform.alarms.helpers;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import cern.laser.client.data.Alarm;
import cern.laser.client.data.Category;
import cern.laser.client.data.Location;
import cern.laser.client.data.ResponsiblePerson;
import cern.laser.client.data.Source;
import cern.laser.client.data.Status;
import cern.laser.client.data.Triplet;
import cern.laser.guiplatform.category.helpers.CategoryImpl;

public class AlarmImpl implements Alarm, Cloneable
{
  private String alarmId;
  private Triplet triplet;
  private String problemDescription;
  private Integer priority;
  private Source source;
  private Location location;
  private URL helpURL;
  private ResponsiblePerson responsiblePerson;
  private Collection categories;
  private Status status;
  private boolean instant;
  private boolean nodeParent;
  private boolean multiplicityParent;
  private boolean nodeChild;
  private boolean multiplicityChild;
  
  private String systemName;
  private String identifier;
  private String consequence;
  private String action;
  private String cause;
  
  //public AlarmImpl(cern.laser.business.data.Alarm alarm) 
  public AlarmImpl(String alarmId, boolean active, String faultFamily, String faultMember,
                    int faultCode) 
  {
      this(alarmId, active, false, false, faultFamily, faultMember, faultCode);
  }
  public AlarmImpl(String alarmId, boolean active, boolean reduced, boolean masked,
                   String faultFamily, String faultMember, int faultCode) 
  {
    this.alarmId = alarmId;
    triplet = new TripletImpl(faultFamily, faultMember, faultCode);
    problemDescription = "problem Description";
    //priority = alarm.getPriority().intValue();
    source = new SourceImpl();
    location = new LocationImpl();
    helpURL = null;
    responsiblePerson = new ResponsiblePersonImpl();
    categories = new ArrayList();
    categories.add(new CategoryImpl("category1", "category1 description"));
    categories.add(new CategoryImpl("category2", "category2 description"));
    categories.add(new CategoryImpl("category3", "category3 description"));
    categories.add(new CategoryImpl("category4", "category4 description"));
    
    /*
    Iterator iterator = alarm.getCategories().iterator();
    while (iterator.hasNext()) 
    {
      categories.add(new CategoryImpl((cern.laser.business.data.Category)iterator.next()));
    }
    */
    status = new StatusImpl(active, masked, reduced);
    instant = false;
    nodeParent = false;
    multiplicityParent = false; 
    nodeChild = false; 
    multiplicityChild = false; 
    
    systemName = "systemName";
    identifier = "system identifier";
    consequence = "consequence";
    action = "action";
    cause = "cause";
    /*
    if (alarm == null) 
    {
      throw new IllegalArgumentException("parameter can not be null");
    }
    alarmId = alarm.getAlarmId().intValue();
    triplet = new TripletImpl(alarm.getTriplet());
    problemDescription = alarm.getProblemDescription();
    priority = alarm.getPriority().intValue();
    source = new SourceImpl(alarm.getSource());
    location = new LocationImpl(alarm.getLocation());
    helpURL = alarm.getHelpURL();
    responsiblePerson = new ResponsiblePersonImpl(alarm.getResponsiblePerson());
    categories = new ArrayList();
    Iterator iterator = alarm.getCategories().iterator();
    while (iterator.hasNext()) 
    {
      categories.add(new CategoryImpl((cern.laser.business.data.Category)iterator.next()));
    }
    status = new StatusImpl(alarm.getStatus());
    instant = alarm.getInstant().booleanValue();
    nodeParent = (!alarm.getNodeChildrenId().isEmpty());
    multiplicityParent = (!alarm.getMultiplicityChildrenId().isEmpty());
    nodeChild = (!alarm.getNodeParentsId().isEmpty());
    multiplicityChild = (!alarm.getMultiplicityParentsId().isEmpty());
    */
  }
  

  private AlarmImpl(Alarm alarm) throws CloneNotSupportedException 
  {
    if (alarm == null) 
    {
      throw new IllegalArgumentException("parameter can not be null");
    }
    alarmId = alarm.getAlarmId();
    triplet = (Triplet)alarm.getTriplet().clone();
    problemDescription = new String(alarm.getProblemDescription());
    priority = alarm.getPriority();
    source = (Source)alarm.getSource().clone();
    location = (Location)alarm.getLocation().clone();
    try {
      helpURL = (alarm.getHelpURL() == null ? null : new URL(alarm.getHelpURL().toExternalForm()));
    } catch (MalformedURLException e) 
    {
      helpURL = null;
    }
    responsiblePerson = (ResponsiblePerson)alarm.getResponsiblePerson().clone();
    categories = new ArrayList();
    Iterator iterator = alarm.getCategories().iterator();
    while (iterator.hasNext()) 
    {
      categories.add(((Category)iterator.next()).clone());
    }
    status = (Status)alarm.getStatus().clone();
    instant = alarm.isInstant();
    
    systemName = alarm.getSystemName();
    identifier = alarm.getIdentifier();
    consequence = alarm.getConsequence();
    action = alarm.getAction();
    cause = alarm.getCause();
  }
  
  public String getAlarmId() 
  {
    return alarmId;
  }
  
  public Triplet getTriplet() 
  {
    return triplet;
  }
  
  public String getProblemDescription() 
  {
    return problemDescription;
  }

  public Integer getPriority() 
  {
    return priority;
  }
  public void setPriority(int priority) 
  {
    this.priority = new Integer(priority);
  }

  public Source getSource() 
  {
    return source;
  }

  public Location getLocation() 
  {
    return location;
  }

  public URL getHelpURL() 
  {
    return helpURL;
  }
  
  public Collection getCategories() 
  {
    return categories;
  }

  public ResponsiblePerson getResponsiblePerson() 
  {
    return responsiblePerson;
  }
  
  public Status getStatus() 
  {
    return status;
  }

  public boolean isInstant()
  {
    return instant;
  }
  public void setIsInstant(boolean flag) {
      this.instant = flag;
  }
  
  public boolean isNodeParent()
  {
    return nodeParent;
  }

  public boolean isMultiplicityParent()
  {
    return multiplicityParent;
  }


  public boolean isNodeChild()
  {
    return nodeChild;
  }

  public boolean isMultiplicityChild()
  {
    return multiplicityChild;
  }

  public String getSystemName() {
      return systemName;
  }
  
  public String getIdentifier() {
      return identifier;
  }
  
  public String getConsequence() {
      return consequence;
  }
  
  public String getAction() {
      return action;
  }
  
  public String getCause() {
      return cause;
  }
  
  public String getPiquetEmail() {
      return "piquetEmail";
  }
  
  public String getPiquetGSM() {
      return "piquetGSM";
  }
  
  public boolean equals(Object obj) 
  {
    if (!(obj instanceof Alarm)) 
    {
      return false;
    }
    Alarm alarm = (Alarm)obj;

    return getAlarmId() == alarm.getAlarmId();
  }

  public Object clone() throws CloneNotSupportedException 
  {
    return new AlarmImpl(this);
  }  
  
  public String toString() 
  {
    StringBuffer str_buf = new StringBuffer();
    str_buf.append("\nALARM :");
    str_buf.append("\nID : ");
    str_buf.append(getAlarmId());
    str_buf.append("\nTRIPLET : ");
    str_buf.append(getTriplet());
    str_buf.append("\nPROBLEM DESCRIPTION : ");
    str_buf.append(getProblemDescription());
    str_buf.append("\nPRIORITY : ");
    str_buf.append(getPriority());
    str_buf.append("\nHELP URL : ");
    str_buf.append(getHelpURL());
    str_buf.append("\nINSTANT : ");
    str_buf.append(isInstant());
    str_buf.append("\nNODE PARENT : ");
    str_buf.append(isNodeParent());
    str_buf.append("\nMULTIPLICITY PARENT : ");
    str_buf.append(isMultiplicityParent());
    str_buf.append("\nLOCATION : ");
    str_buf.append(getLocation());
    str_buf.append("\nSOURCE : ");
    str_buf.append(getSource());
    str_buf.append("\nRESPONSIBLE : ");
    str_buf.append(getResponsiblePerson());
    str_buf.append("\nSTATUS : ");
    str_buf.append(getStatus());
    str_buf.append("\nCATEGORIES : ");
    str_buf.append(getCategories());
    
    return str_buf.toString();
  }

}
