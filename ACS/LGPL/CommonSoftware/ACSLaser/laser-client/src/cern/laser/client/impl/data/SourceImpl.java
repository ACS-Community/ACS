/*
 * $Id: SourceImpl.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client.impl.data;

import java.io.Serializable;

import cern.laser.client.data.ResponsiblePerson;
import cern.laser.client.data.Source;

public class SourceImpl implements Source, Cloneable, Serializable {
  private String sourceId;
  private String description;
  private ResponsiblePerson responsiblePerson;

  public SourceImpl(cern.laser.business.data.Source source) {
    if (source == null) { throw new IllegalArgumentException("parameter can not be null"); }
    sourceId = source.getSourceId();
    description = source.getDescription();
    responsiblePerson = (source.getResponsiblePerson() == null ? null : new ResponsiblePersonImpl(source
        .getResponsiblePerson()));
  }

  public String getSourceId() {
    return sourceId;
  }

  public String getName() {
    return sourceId;
  }

  public String getDescription() {
    return description;
  }

  public ResponsiblePerson getResponsiblePerson() {
    return responsiblePerson;
  }

  public void setDescription(String newDescription) {
    description = newDescription;
  }

  public void setResponsiblePerson(ResponsiblePerson newResponsiblePerson) {
    responsiblePerson = newResponsiblePerson;
  }

  public void setSourceId(String newSourceId) {
    sourceId = newSourceId;
  }

  public boolean equals(Object obj) {
    if ((obj == null) || (!(obj instanceof Source))) { return false; }
    Source source = (Source) obj;

    return getSourceId().equals(source.getSourceId());
  }

  public int hashCode() {
    return getSourceId().hashCode();
  }

  public Object clone() throws CloneNotSupportedException {
    try {
      SourceImpl source = (SourceImpl) super.clone();
      source.setResponsiblePerson(responsiblePerson == null ? null : (ResponsiblePerson) responsiblePerson.clone());

      return source;
    } catch (Exception e) {
      throw new CloneNotSupportedException("clone failed : " + e.getMessage());
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
    str_buf.append("\nRESPONSIBLE : ");
    str_buf.append(getResponsiblePerson());

    return str_buf.toString();
  }

}