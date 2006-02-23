package cern.laser.guiplatform.alarms.helpers;
import java.net.URL;

import cern.laser.client.data.ResponsiblePerson;

public class ResponsiblePersonImpl implements ResponsiblePerson, Cloneable
{
  private Integer responsibleId;
  private String name;
  private String familyName;
  private String eMail;
  private String gsmNumber;
  private String phoneNumber;
  private URL whoURL;

  //public ResponsiblePersonImpl(cern.laser.business.data.ResponsiblePerson responsible)
  public ResponsiblePersonImpl()
  {
      responsibleId = new Integer(-1);
      name = "Bartek";
      familyName = "Pawlowski";
      eMail = "Bartek";
      gsmNumber = "Bartek";
      phoneNumber = "Bartek";
      whoURL = null;


    /*
    if (responsible == null) 
    {
      throw new IllegalArgumentException("parameter can not be null");
    }
    responsibleId = responsible.getResponsibleId().intValue();
    name = responsible.getName();
    eMail = responsible.getEMail();
    gsmNumber = responsible.getGsmNumber();
    phoneNumber = responsible.getPhoneNumber();
    whoURL = responsible.getWhoURL();
    */
  }

  private ResponsiblePersonImpl(ResponsiblePerson responsible)
  {
    if (responsible == null) 
    {
      throw new IllegalArgumentException("parameter can not be null");
    }
    responsibleId = responsible.getResponsibleId();
    name = new String(responsible.getFirstName());
    familyName = new String(responsible.getFamilyName());
    eMail = new String(responsible.getEMail());
    gsmNumber = new String(responsible.getGsmNumber());
    phoneNumber = new String(responsible.getPhoneNumber());
    //try {
    //  whoURL = (responsible.getWhoURL() == null ? null : new URL(responsible.getWhoURL().toExternalForm()));
    //} catch (MalformedURLException e) 
    //{
    //  whoURL = null;
    //}
  }

  public Integer getResponsibleId()
  {
    return responsibleId;
  }

  public String getFirstName()
  {
    return name;
  }
  public String getFamilyName()
  {
    return familyName;
  }

  public String getEMail()
  {
    return eMail;
  }

  public String getGsmNumber()
  {
    return gsmNumber;
  }

  public String getPhoneNumber()
  {
    return phoneNumber;
  }

  public URL getWhoURL()
  {
    return whoURL;
  }

  public Object clone() throws CloneNotSupportedException 
  {
    return new ResponsiblePersonImpl(this);
  }  
  
  public String toString() 
  {
    StringBuffer str_buf = new StringBuffer();
    str_buf.append("\nID : ");
    str_buf.append(getResponsibleId());
    str_buf.append("\nNAME : ");
    str_buf.append(getFirstName());
    str_buf.append("\nE-MAIL : ");
    str_buf.append(getEMail());
    str_buf.append("\nGSM : ");
    str_buf.append(getGsmNumber());
    str_buf.append("\nPHONE : ");
    str_buf.append(getPhoneNumber());
    str_buf.append("\nURL : ");
    str_buf.append(getWhoURL());
    
    return str_buf.toString();
  }
}
