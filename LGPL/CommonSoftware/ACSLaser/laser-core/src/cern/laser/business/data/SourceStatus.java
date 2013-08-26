/*
 * $Id: SourceStatus.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.business.data;

import java.io.Serializable;
import java.sql.Timestamp;

/**
 * 
 * 
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 * @author Katarina Sigerud
 * @hibernate.class table="SOURCE_STATUS"
 */
 public class SourceStatus implements Serializable, Cloneable {
  public static final SourceStatus INITIAL_STATUS = new SourceStatus(Boolean.TRUE, Boolean.TRUE, null);

  private String statusId;
  private Boolean connected;
  private Timestamp lastContact;
  private Boolean enabled;

  //
  // -- CONSTRUCTORS ------------------------------------------------
  //

  private SourceStatus() {
  }

  public SourceStatus(Boolean connected, Boolean enabled, Timestamp lastContact) {
    setConnected(connected);
    setEnabled(enabled);
    setLastContact(lastContact);
  }
  
  //
  // -- PUBLIC METHODS ----------------------------------------------
  //

  /**
   * 
   * @hibernate.id generator-class="assigned" column="SOURCE_ID"
   * @return Integer
   */
  public String getStatusId() {
    return statusId;
  }

  public void setStatusId(String sourceId) {
    this.statusId = sourceId;
  }

  /**
   * @return Returns the connected.
   * @hibernate.property name="getConnected" column="CONNECTED" type="yes_no" not-null="false" unique="false"
   * 
   * @return boolean
   */
  public Boolean getConnected() {
    return connected;
  }

  public void setConnected(Boolean connected) {
    this.connected = connected;
  }
  /**
   * @return Returns the enabled.
   * @hibernate.property name="getEnabled" column="ENABLED" type="yes_no" not-null="false" unique="false"
   * 
   * @return Boolean
   */
  public Boolean getEnabled() {
    return enabled;
  }

  public void setEnabled(Boolean enabled) {
    this.enabled = enabled;
  }

  /**
   * @return Returns the lastContact.
   * @hibernate.property name="getLastContact" column="LAST_CONTACT" type="java.sql.Timestamp" not-null="false"
   *                     unique="false"
   * 
   * @return Timestamp
   */
  public Timestamp getLastContact() {
    return lastContact;
  }

  public void setLastContact(Timestamp lastContact) {
    this.lastContact = lastContact;
  }

  public int hashCode() {
    return getStatusId().hashCode();
  }

  public boolean equals(Object obj) {
    if (obj == null || !(obj instanceof SourceStatus)) return false;
    SourceStatus status = (SourceStatus) obj;
    return getStatusId().equals(status.getStatusId());
  }

  public Object clone() {
    try {
      SourceStatus status = (SourceStatus) super.clone();
      status.setLastContact(lastContact == null ? null : (Timestamp) lastContact.clone());

      return status;
    } catch (Exception e) {
      throw new InternalError();
    }
  }

  //
  // -- implements XXX ----------------------------------------------
  //

  //
  // -- PROTECTED METHODS -------------------------------------------
  //

  //
  // -- PRIVATE METHODS ---------------------------------------------
  //

}
