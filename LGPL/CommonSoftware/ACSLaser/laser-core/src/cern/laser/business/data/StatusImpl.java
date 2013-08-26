/*
 * $Id: StatusImpl.java,v 1.3 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.3 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.business.data;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Properties;
import java.util.Set;


/**
 * 
 * 
 * @version $Revision: 1.3 $ $Date: 2006/09/25 08:52:36 $
 * @author Katarina Sigerud
 * @hibernate.class table="ALARM_STATUS"
 */
public class StatusImpl implements Serializable, Cloneable, Status {
  private String statusId;
  private Boolean active;
  private Boolean masked;
  private Boolean reduced;
  private Boolean globalAcknowledged;
  private Boolean terminatedByBackup;
  private Boolean activatedByBackup;
  private Timestamp sourceTimestamp;
  private String sourceHostname;
  private Timestamp userTimestamp;
  private Timestamp systemTimestamp;
  private Set persistentUserProperties;
  
//  private Set transientUserProperties;
  private Properties properties;

  //
  // -- CONSTRUCTORS ------------------------------------------------
  //

  private StatusImpl() {
  }

  public StatusImpl(Boolean active, Boolean masked, Boolean reduced, Boolean activeByBackup,
      Boolean terminatedByBackup,
      /* Boolean globalAcknowledged, */String sourceHostname, Timestamp sourceTimestamp, Timestamp userTimestamp,
      Timestamp systemTimestamp, Properties newUserProperties) {
    setActive(active);
    setMasked(masked);
    setReduced(reduced);
    setActivatedByBackup(activeByBackup);
    setTerminatedByBackup(terminatedByBackup);
    //    setActiveAgain(activeAgain);
    //    setTerminatedAgain(terminatedAgain);
    //    setGlobalAcknowledge(globalAcknowledged);
    //    setNotFound(notFound);
    //    setWrongSource(wrongSource);
    setSourceHostname(sourceHostname);
    setSourceTimestamp(sourceTimestamp);
    setUserTimestamp(userTimestamp);
    setSystemTimestamp(systemTimestamp);

    setProperties(newUserProperties);
  }

  //
  // -- PUBLIC METHODS ----------------------------------------------
  //

  /**
   * @hibernate.id generator-class="assigned" column="STATUS_ID" unsaved-value="null"
   */
  public String getStatusId() {
    return statusId;
  }

  public void setStatusId(String alarmId) {
    this.statusId = alarmId;
  }

  /**
   * @hibernate.property name="getActive" column="ACTIVE" type="yes_no" not-null="false" unique="false"
   * 
   * @return Boolean
   */
  public Boolean getActive() {
    return active;
  }

  /**
   * @hibernate.property name="getMasked" column="MASKED" type="yes_no" not-null="false" unique="false"
   *  
   */
  public Boolean getMasked() {
    return masked;
  }

  /**
   * @hibernate.property name="getReduced" column="REDUCED" type="yes_no" not-null="false" unique="false"
   *  
   */
  public Boolean getReduced() {
    return reduced;
  }

  /**
   * @hibernate.property name="getActivatedByBackup" column="ACTIVATED_BY_BACKUP" type="yes_no" not-null="false"
   *                     unique="false"
   *  
   */
  public Boolean getActivatedByBackup() {
    return activatedByBackup;
  }

  public void setActivatedByBackup(Boolean newActiveByBackup) {
    activatedByBackup = newActiveByBackup;
  }

  /**
   * @hibernate.property name="getTerminatedByBackup" column="TERMINATED_BY_BACKUP" type="yes_no" not-null="false"
   *                     unique="false"
   */
  public Boolean getTerminatedByBackup() {
    return terminatedByBackup;
  }

  public void setTerminatedByBackup(Boolean newTerminatedByBackup) {
    terminatedByBackup = newTerminatedByBackup;
  }

  //  /**
  //   * @hibernate.property name="getGlobalAcknowledged" column="GLOBAL_ACKNOWLEDGED" type="yes_no"
  //   * not-null="false" unique="false"
  //	 *
  //   */
  //  public Boolean getGlobalAcknowledged() {
  //    return globalAcknowledged;
  //  }
  //  
  //  private void setGlobalAcknowledged(Boolean newGlobalAcknowledged) {
  //    globalAcknowledged = newGlobalAcknowledged;
  //  }

  /**
   * @hibernate.property name="getSourceHostname" column="SOURCE_HOSTNAME" not-null="false" unique="false"
   * 
   * @return String
   */
  public String getSourceHostname() {
    return sourceHostname;
  }

  /**
   * @hibernate.property name="getSourceTimestamp" column="SOURCE_TIMESTAMP" type="java.sql.Timestamp" not-null="false"
   *                     unique="false"
   * 
   * @return Timestamp
   * @return
   */
  public Timestamp getSourceTimestamp() {
    return sourceTimestamp;
  }

  /**
   * @hibernate.property name="getUserTimestamp" column="USER_TIMESTAMP" type="java.sql.Timestamp" not-null="false"
   *                     unique="false"
   * 
   * @return Timestamp
   * @return
   */
  public Timestamp getUserTimestamp() {
    return userTimestamp;
  }

  public void setUserTimestamp(Timestamp newUserTimestamp) {
    userTimestamp = newUserTimestamp;
  }

  /**
   * @hibernate.property name="getSystemTimestamp" column="SYSTEM_TIMESTAMP" type="java.sql.Timestamp" not-null="false"
   *                     unique="false"
   * 
   * @return Timestamp
   * @return
   */
  public Timestamp getSystemTimestamp() {
    return systemTimestamp;
  }

  public void setActive(Boolean newActive) {
    active = newActive;
  }

  public void setMasked(Boolean newMasked) {
    masked = newMasked;
  }

  public void setReduced(Boolean newReduced) {
    reduced = newReduced;
  }

  public void setSourceHostname(String newSourceHostname) {
    sourceHostname = newSourceHostname;
  }

  public void setSourceTimestamp(Timestamp newSourceTimestamp) {
    sourceTimestamp = newSourceTimestamp;
  }

  public void setSystemTimestamp(Timestamp newSystemTimestamp) {
    systemTimestamp = newSystemTimestamp;
  }

  public Properties getProperties() {
    return properties == null ? new Properties() : properties;
  }

  public void setProperties(Properties newProperties) {
    properties = newProperties;
  }

  private Properties translateFromUserProperties() {
    Properties result = new Properties();
    Iterator properties_iterator = getPersistentUserProperties().iterator();
    while (properties_iterator.hasNext()) {
      UserProperty property = (UserProperty) properties_iterator.next();
      result.setProperty(property.getName(), property.getValue());
    }

    return result;
  }

  public Set translateToUserProperties(Properties newProperties) {
    Set new_user_properties = new HashSet();
    if (newProperties != null) {
      Enumeration properties_enumeration = newProperties.propertyNames();
      while (properties_enumeration.hasMoreElements()) {
        String property_name = (String) properties_enumeration.nextElement();
        if (property_name != null && !property_name.equals("")) {
          UserProperty new_user_property = new UserProperty(property_name, newProperties.getProperty(property_name),
              getStatusId());
          new_user_properties.add(new_user_property);
        }
      }
    }
    return new_user_properties;
  }

  public int hashCode() {
    return getStatusId().hashCode();
  }

  public boolean equals(Object obj) {
    if (obj == null) { return false; }
    if (!(obj instanceof Status)) { return false; }
    Status status = (Status) obj;

    return getStatusId().equals(status.getStatusId());
  }

  public Object clone() {
    try {
      Status status = (Status) super.clone();
      status.setSourceTimestamp(sourceTimestamp == null ? null : (Timestamp) sourceTimestamp.clone());
      status.setSystemTimestamp(systemTimestamp == null ? null : (Timestamp) systemTimestamp.clone());
      status.setUserTimestamp(userTimestamp == null ? null : (Timestamp) userTimestamp.clone());
      status.setProperties((Properties) getProperties().clone());

      return status;
    } catch (Exception e) {
      throw new InternalError("unable to clone status : " + e.getMessage());
    }
  }

  public String toString() {
    StringBuffer str_buf = new StringBuffer();
    str_buf.append("\nACTIVE : ");
    str_buf.append(getActive());
    str_buf.append("\nMASKED : ");
    str_buf.append(getMasked());
    str_buf.append("\nREDUCED : ");
    str_buf.append(getReduced());
    str_buf.append("\nSOURCE HOSTNAME : ");
    str_buf.append(getSourceHostname());
    str_buf.append("\nSOURCE TIMESTAMP : ");
    str_buf.append(getSourceTimestamp());
    str_buf.append("\nUSER TIMESTAMP : ");
    str_buf.append(getUserTimestamp());
    str_buf.append("\nSYSTEM TIMESTAMP : ");
    str_buf.append(getSystemTimestamp());
    str_buf.append("\nUSER PROPERTIES : \n");
    str_buf.append(getProperties());

    return str_buf.toString();
  }

  /**
   * @return Returns the persistentUserProperties.
   * @hibernate.set name="getPersistentUserProperties" table="USER_PROPERTY" inverse="false" cascade="all-delete-orphan"
   *                lazy="false"
   * @hibernate.collection-key column="STATUS_ID"
   * @hibernate.collection-one-to-many class="cern.laser.business.data.UserProperty"
   * 
   * @return Collection
   */
  public Set getPersistentUserProperties() {
    return persistentUserProperties == null ? new HashSet(0) : persistentUserProperties;
  }

//  public Set getTransientUserProperties() {
//    return transientUserProperties == null ? new HashSet(0) : transientUserProperties;
//  }

  //
  // -- PROTECTED METHODS -------------------------------------------
  //

  //
  // -- PRIVATE METHODS ---------------------------------------------
  //

  /**
   * @param persistentUserProperties The persistentUserProperties to set.
   */
  public void setPersistentUserProperties(Set userProperties) {
    this.persistentUserProperties = userProperties;
  }
}