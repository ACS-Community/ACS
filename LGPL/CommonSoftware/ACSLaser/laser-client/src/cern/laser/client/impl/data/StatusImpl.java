/*
 * $Id: StatusImpl.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client.impl.data;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.Properties;

import cern.laser.client.data.Status;

public class StatusImpl implements Status, Cloneable, Serializable {
  private boolean active;
  private boolean masked;
  private boolean reduced;
  private Timestamp sourceTimestamp;
  private String sourceHostname;
  private Timestamp userTimestamp;
  private Timestamp systemTimestamp;
  private Properties userProperties;

  public StatusImpl(cern.laser.business.data.Status status) {
    if (status == null) { throw new IllegalArgumentException("parameter can not be null"); }
    active = status.getActive().booleanValue();
    masked = status.getMasked().booleanValue();
    reduced = status.getReduced().booleanValue();
    sourceHostname = status.getSourceHostname();
    sourceTimestamp = status.getSourceTimestamp();
    userTimestamp = status.getUserTimestamp();
    systemTimestamp = status.getSystemTimestamp();
    userProperties = status.getProperties();
  }

  public boolean isActive() {
    return active;
  }

  public boolean isMasked() {
    return masked;
  }

  public boolean isReduced() {
    return reduced;
  }

  public String getSourceHostname() {
    return sourceHostname;
  }

  public Timestamp getSourceTimestamp() {
    return sourceTimestamp;
  }

  public Timestamp getUserTimestamp() {
    return userTimestamp;
  }

  public Timestamp getSystemTimestamp() {
    return systemTimestamp;
  }

  public Properties getUserProperties() {
    return userProperties;
  }

  public void setActive(boolean newActive) {
    active = newActive;
  }

  public void setMasked(boolean newMasked) {
    masked = newMasked;
  }

  public void setReduced(boolean newReduced) {
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

  public void setUserProperties(Properties newUserProperties) {
    userProperties = newUserProperties;
  }

  public void setUserTimestamp(Timestamp newUserTimestamp) {
    userTimestamp = newUserTimestamp;
  }

  public boolean equals(Object obj) {
    if ((obj == null) || (!(obj instanceof Status))) { return false; }

    return toString().equals(obj.toString());
  }

  public int hashCode() {
    return toString().hashCode();
  }

  public Object clone() throws CloneNotSupportedException {
    try {
      StatusImpl status = (StatusImpl) super.clone();
      status.setSourceTimestamp(sourceTimestamp == null ? null : (Timestamp) sourceTimestamp.clone());
      status.setUserTimestamp(userTimestamp == null ? null : (Timestamp) status.userTimestamp.clone());
      status.setSystemTimestamp(systemTimestamp == null ? null : (Timestamp) systemTimestamp.clone());
      status.setUserProperties(userProperties == null ? null : (Properties) userProperties.clone());

      return status;
    } catch (Exception e) {
      throw new CloneNotSupportedException("clone failed : " + e.getMessage());
    }
  }

  public String toString() {
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