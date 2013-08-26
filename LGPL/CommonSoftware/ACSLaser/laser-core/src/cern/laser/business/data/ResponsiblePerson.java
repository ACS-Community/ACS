/*
 * $Id: ResponsiblePerson.java,v 1.3 2008/02/04 14:42:14 acaproni Exp $
 *
 * $Date: 2008/02/04 14:42:14 $ 
 * $Revision: 1.3 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.business.data;

import java.io.Serializable;

/**
 * 
 * 
 * 
 * @version $Revision: 1.3 $ $Date: 2008/02/04 14:42:14 $
 * @author Niall Stapley
 * @author Katarina Sigerud
 * 
 * @hibernate.class table="PUBLIC_PERS_INFO" mutable="false"
 */
public class ResponsiblePerson implements Serializable, Cloneable {
  private Integer responsibleId;
  private String firstName;
  private String familyName;
  private String eMail;
  private String gsmNumber;
  private String phoneNumber;

  //
  // -- CONSTRUCTORS ------------------------------------------------
  //

  /**
   * Empty constructor for Hibernate.
   *  
   */
  public ResponsiblePerson() {
  }

  /**
   * @param responsible_id
   * @param string
   * @param string2
   * @param string3
   * @param string4
   * @param string5
   */
  public ResponsiblePerson(Integer responsibleId, String familyName, String firstName, String email, String gsm,
      String phone) {
    setResponsibleId(responsibleId);
    setFamilyName(familyName);
    setFirstName(firstName);
    if (email!=null) {
    	setEMail(email);
    } else {
    	setEMail("");
    }
    if (gsm!=null) {
    	setGsmNumber(gsm);
    } else {
    	setGsmNumber("");
    }
    setPhoneNumber(phone);
  }

  //
  // -- PUBLIC METHODS ----------------------------------------------
  //

  /**
   * @return Returns the eMail.
   * @hibernate.property name="getEMail" column="EMAIL" not-null="false" unique="false"
   */
  public String getEMail() {
    return eMail;
  }

  /**
   * @return Returns the familyName.
   * @hibernate.property name="getFamilyName" column="NAME" not-null="false" unique="false"
   */
  public String getFamilyName() {
    return familyName;
  }

  /**
   * @return Returns the firstName.
   * @hibernate.property name="getFirstName" column="FIRST_NAME" not-null="false" unique="false"
   */
  public String getFirstName() {
    return firstName;
  }

  /**
   * @return Returns the gsmNumber.
   * @hibernate.property name="getGsmNumber" column="PORTABLE_PHONE" not-null="false" unique="false"
   */
  public String getGsmNumber() {
    return gsmNumber;
  }

  /**
   * @return Returns the phoneNumber.
   * @hibernate.property name="getPhoneNumber" column="TEL_1" not-null="false" unique="false"
   */
  public String getPhoneNumber() {
    return phoneNumber;
  }

  /**
   * 
   * @hibernate.id generator-class="assigned" column="IDENT"
   */
  public Integer getResponsibleId() {
    return responsibleId;
  }

  /**
   * @param mail The eMail to set.
   */
  public void setEMail(String mail) {
    eMail = mail;
  }

  /**
   * @param familyName The familyName to set.
   */
  public void setFamilyName(String familyName) {
    this.familyName = familyName;
  }

  /**
   * @param firstName The firstName to set.
   */
  public void setFirstName(String firstName) {
    this.firstName = firstName;
  }

  /**
   * @param gsmNumber The gsmNumber to set.
   */
  public void setGsmNumber(String gsmNumber) {
    this.gsmNumber = gsmNumber;
  }

  /**
   * @param phoneNumber The phoneNumber to set.
   */
  public void setPhoneNumber(String phoneNumber) {
    this.phoneNumber = phoneNumber;
  }

  /**
   * @param responsibleId The responsibleId to set.
   */
  public void setResponsibleId(Integer responsibleId) {
    this.responsibleId = responsibleId;
  }

  public Object clone() {
    try {
      ResponsiblePerson person = (ResponsiblePerson) super.clone();

      return person;
    } catch (Exception e) {
      throw new InternalError();
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
    str_buf.append("\nEMAIL : ");
    str_buf.append(getEMail());
    str_buf.append("\nGSM : ");
    str_buf.append(getGsmNumber());
    str_buf.append("\nPHONE : ");
    str_buf.append(getPhoneNumber());

    return str_buf.toString();
  }

  //
  // -- PROTECTED METHODS -------------------------------------------
  //

  //
  // -- PRIVATE METHODS ---------------------------------------------
  //
}