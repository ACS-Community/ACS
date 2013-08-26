package cern.laser.business.definition.data;
import java.io.Serializable;
import java.net.URL;

import cern.laser.business.definition.LaserDefinitionNotValidException;

public class ResponsiblePersonDefinition implements Serializable, LaserDefinition
{
  private Integer responsibleId;
  private String firstName;
  private String familyName;
  private String eMail;
  private String gsmNumber;
  private String phoneNumber;
  private URL whoURL;

  public ResponsiblePersonDefinition()
  {
  }

  public ResponsiblePersonDefinition(Integer responsibleId, String firstName, String familyName, String eMail, String gsmNumber, String phoneNumber, URL whoURL)
  {
    this.responsibleId = responsibleId;
    this.firstName = firstName;
    this.familyName = familyName;
    this.eMail = eMail;
    this.gsmNumber = gsmNumber;
    this.phoneNumber = phoneNumber;
    this.whoURL = whoURL;
  }

  public Integer getResponsibleId()
  {
    return responsibleId;
  }

  public void setResponsibleId(Integer newResponsibleId)
  {
    responsibleId = newResponsibleId;
  }

  public String getFirstName()
  {
    return firstName;
  }

  public void setFirstName(String newFirstName)
  {
    firstName = newFirstName;
  }

  public String getFamilyName()
  {
    return familyName;
  }

  public void setFamilyName(String newFamilyName)
  {
    familyName = newFamilyName;
  }

  public String getEMail()
  {
    return eMail;
  }

  public void setEMail(String newEMail)
  {
    eMail = newEMail;
  }

  public String getGsmNumber()
  {
    return gsmNumber;
  }

  public void setGsmNumber(String newGsmNumber)
  {
    gsmNumber = newGsmNumber;
  }

  public String getPhoneNumber()
  {
    return phoneNumber;
  }

  public void setPhoneNumber(String newPhoneNumber)
  {
    phoneNumber = newPhoneNumber;
  }

  public URL getWhoURL()
  {
    return whoURL;
  }

  public void setWhoURL(URL newWhoURL)
  {
    whoURL = newWhoURL;
  }

  public void validate() throws LaserDefinitionNotValidException 
  {
    
  }

  public String toString() 
  {
    StringBuffer str_buf = new StringBuffer();
    str_buf.append("\nALARM RESPONSIBLE PERSON DEFINITION:");
    str_buf.append("\nID : ");
    str_buf.append(getResponsibleId());
    str_buf.append("\nFIRST NAME : ");
    str_buf.append(getFirstName());
    str_buf.append("\nFAMILY NAME : ");
    str_buf.append(getFamilyName());
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