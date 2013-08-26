/*
 * $Id: Source.java,v 1.4 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.4 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.business.data;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

import cern.laser.business.definition.data.SourceDefinition;

/**
 * 
 * 
 * 
 * @version $Revision: 1.4 $ $Date: 2006/09/25 08:52:36 $
 * @author Niall Stapley
 * @author Katarina Sigerud
 * 
 * @hibernate.class table="SOURCE_DEFINITION"
 */
public class Source implements Serializable, Cloneable {
  private String sourceId;
  private String description;
  private Integer connectionTimeout;
  private String surveillanceAlarmId;
  private ResponsiblePerson responsiblePerson;
  private String hostName;
  private Set alarmIds;
  private SourceStatus status;

  //
  // -- CONSTRUCTORS ------------------------------------------------
  //

  /**
   * Empty constructor for Hibernate.
   *  
   */
  public Source() {
  }

  public Source(SourceDefinition definition, ResponsiblePerson responsiblePerson) {
    setSourceId(definition.getSourceId());
    setDefinition(definition);
    setResponsiblePerson(responsiblePerson);
    status = (SourceStatus) SourceStatus.INITIAL_STATUS.clone();
    status.setStatusId(getSourceId());
  }

  //
  // -- PUBLIC METHODS ----------------------------------------------
  //

  /**
   */
//  public Boolean getConnected() {
//    return status.getConnected();
//  }

  /**
   * @return Returns the connectionTimeout.
   * @hibernate.property name="getConnectionTimeout" column="CONNECTION_TIMEOUT" not-null="false"
   *                     unique="false"
   */
  public Integer getConnectionTimeout() {
    return connectionTimeout;
  }

  /**
   * @return Returns the description.
   * @hibernate.property name="getDescription" column="DESCRIPTION" not-null="false" unique="false"
   */
  public String getDescription() {
    return description;
  }

  /**
   */
  public String getName() {
    return getSourceId();
  }

  /**
   * @return Returns the responsible.
   * @hibernate.many-to-one column="RESPONSIBLE_ID" class="cern.laser.business.data.ResponsiblePerson"
   */
  public ResponsiblePerson getResponsiblePerson() {
    return responsiblePerson;
  }

  /**
   * 
   * @hibernate.id generator-class="assigned" column="SOURCE_ID"
   * @return integer
   */
  public String getSourceId() {
    return sourceId;
  }

  /**
   * @return Returns the surveillanceAlarmId.
   * @hibernate.property name="getSurveillanceAlarmId" column="SURVEILLANCE_ALARM_ID" not-null="false"
   *                     unique="false"
   */
  public String getSurveillanceAlarmId() {
    return surveillanceAlarmId;
  }

  /**
   * Sets up the bi-directional relationship between this source and the given alarm. If the alarm is already associate
   * to a source, it is removed from the old source.
   * 
   * @param alarm the alarm to add to this source.
   */
  public void addAlarm(Alarm alarm) {
    alarm.getSource().getAlarmIds().remove(this);
    alarm.setSource(this);
    getAlarmIds().add(alarm.getAlarmId());
  }

  /**
   * @param connected The connected to set.
   */
//  public void setConnected(Boolean connected) {
//    status.setConnected(connected);
//  }

  /**
   * @param connectionTimeout The connectionTimeout to set.
   */
  public void setConnectionTimeout(Integer connectionTimeout) {
    this.connectionTimeout = connectionTimeout;
  }

  /**
   * @param description The description to set.
   */
  public void setDescription(String description) {
    this.description = description;
  }

  /**
   * @param responsible The responsible to set.
   */
  public void setResponsiblePerson(ResponsiblePerson responsible) {
    this.responsiblePerson = responsible;
  }

  /**
   * @param sourceId The sourceId to set.
   */
  public void setSourceId(String sourceId) {
    this.sourceId = sourceId;
  }

  /**
   * @param surveillanceAlarmId The surveillanceAlarmId to set.
   */
  public void setSurveillanceAlarmId(String surveillanceAlarmId) {
    this.surveillanceAlarmId = surveillanceAlarmId;
  }

  /**
   * @hibernate.many-to-one column="STATUS_ID" class="cern.laser.business.data.SourceStatus" cascade="all" unique="true"
   */
  public SourceStatus getStatus() {
    return status;
  }

  public void setStatus(SourceStatus status) {
    this.status = status;
  }

  public Boolean isConnected() {
    return status.getConnected();
  }

  public Boolean isEnabled() {
    return status.getEnabled();
  }

  /**
   * @hibernate.property name="getPhysicalMachine" column="HOSTNAME" not-null="false" unique="false"
   * 
   * @return String
   */
  public String getHostName() {
    return hostName;
  }

  public void setHostName(String hostName) {
    this.hostName = hostName;
  }

  public SourceDefinition getDefinition() {
    SourceDefinition definition = new SourceDefinition(getName());
    definition.setDescription(getDescription());
    definition.setHostName(getHostName());
    definition.setConnectionTimeout(getConnectionTimeout());
    definition
        .setResponsiblePersonId(getResponsiblePerson() == null ? null : getResponsiblePerson().getResponsibleId());

    return definition;
  }

  public void setDefinition(SourceDefinition definition) {
    setDescription(definition.getDescription());
    setHostName(definition.getHostName());
    setConnectionTimeout(definition.getConnectionTimeout());
  }

  public int hashCode() {
    return getSourceId().hashCode();
  }

  public boolean equals(Object obj) {
    if (obj == null || !(obj instanceof Source)) return false;
    Source source = (Source) obj;
    return getSourceId().equals(source.getSourceId());
  }

  public Object clone() {
    try {
      Source source = (Source) super.clone();
      source.setStatus((SourceStatus) status.clone());
      source.setResponsiblePerson(responsiblePerson == null ? null : (ResponsiblePerson) responsiblePerson.clone());
      source.setSourceId(this.sourceId);

      return source;
    } catch (Exception e) {
      throw new InternalError();
    }
  }

  public String toString() {
    StringBuffer str_buf = new StringBuffer();
    str_buf.append("\nID : ");
    str_buf.append(getSourceId());
    str_buf.append("\nNAME : ");
    str_buf.append(getName());
    str_buf.append("\nDESCRIPTION : ");
    str_buf.append(getDescription());
    str_buf.append("\nHOST NAME : ");
    str_buf.append(getHostName());
    str_buf.append("\nCONNECTION TIMEOUT : ");
    str_buf.append(getConnectionTimeout());
    str_buf.append("\nRESPONSIBLE : ");
    str_buf.append(getResponsiblePerson());
    str_buf.append("\nCONNECTED : ");
    str_buf.append(isConnected());
    str_buf.append("\nLAST CONTACT : ");
    str_buf.append(status.getLastContact());
    str_buf.append("\nENABLED : ");
    str_buf.append(isEnabled());

    return str_buf.toString();
  }

  //
  // -- PROTECTED METHODS -------------------------------------------
  //

  //
  // -- PRIVATE METHODS ---------------------------------------------
  //

  /**
   * Returns the ids of the alarms for this source, or an empty collection if there are none.
   * 
   * @return the ids of the alarms for this source, or an empty collection if there are none.
   * 
   * @hibernate.set name="getAlarmIds" table="ALARM_DEFINITION" inverse="true" lazy="true"
   *                outer-join="false"
   * @hibernate.collection-key column="SOURCE_ID"
   * @hibernate.collection-element column="ALARM_ID" type="string"
   */
  public Set getAlarmIds() {
    return alarmIds == null ? new HashSet(0) : alarmIds;
  }

  /**
   * @param alarms The alarms to set.
   */
  public void setAlarmIds(Set newAlarmIds) {
    alarmIds = newAlarmIds;
  }
}