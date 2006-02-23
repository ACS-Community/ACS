/*
 * $Id: ResponsiblePersonImpl.java,v 1.1 2005/06/06 18:19:40 kzagar Exp $
 *
 * $Date: 2005/06/06 18:19:40 $ 
 * $Revision: 1.1 $ 
 * $Author: kzagar $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client.impl.data;

import java.io.Serializable;

import cern.laser.client.data.ResponsiblePerson;

public class ResponsiblePersonImpl implements ResponsiblePerson, Cloneable, Serializable {
  private Integer responsibleId;
  private String firstName;
  private String familyName;
  private String eMail;
  private String gsmNumber;
  private String phoneNumber;

  public ResponsiblePersonImpl(cern.laser.business.data.ResponsiblePerson responsible) {
    if (responsible == null) { throw new IllegalArgumentException("parameter can not be null"); }
    responsibleId = responsible.getResponsibleId();
    firstName = responsible.getFirstName();
    familyName = responsible.getFamilyName();
    eMail = responsible.getEMail();
    gsmNumber = responsible.getGsmNumber();
    phoneNumber = responsible.getPhoneNumber();
  }

  public Integer getResponsibleId() {
    return responsibleId;
  }

  public String getFirstName() {
    return firstName;
  }

  public String getFamilyName() {
    return familyName;
  }

  public String getEMail() {
    return eMail;
  }

  public String getGsmNumber() {
    return gsmNumber;
  }

  public String getPhoneNumber() {
    return phoneNumber;
  }

  public boolean equals(Object obj) {
    if ((obj == null) || (!(obj instanceof ResponsiblePerson))) { return false; }
    ResponsiblePerson person = (ResponsiblePerson) obj;

    return getResponsibleId().equals(person.getResponsibleId());
  }

  public int hashCode() {
    return getResponsibleId().hashCode();
  }

  public Object clone() throws CloneNotSupportedException {
    try {
      ResponsiblePersonImpl responsible = (ResponsiblePersonImpl) super.clone();

      return responsible;
    } catch (Exception e) {
      throw new CloneNotSupportedException("clone failed : " + e.getMessage());
    }
  }

  public String toString() {
    StringBuffer str_buf = new StringBuffer();
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

    return str_buf.toString();
  }

}