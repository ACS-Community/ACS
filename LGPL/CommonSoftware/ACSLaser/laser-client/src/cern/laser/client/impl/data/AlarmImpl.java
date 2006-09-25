/*
 * $Id: AlarmImpl.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client.impl.data;

import java.io.Serializable;
import java.net.URL;
import java.util.Collection;
import java.util.Iterator;
import java.util.Vector;

import cern.laser.client.data.Category;
import cern.laser.client.data.Location;
import cern.laser.client.data.ResponsiblePerson;
import cern.laser.client.data.Source;
import cern.laser.client.data.Status;
import cern.laser.client.data.Triplet;

public class AlarmImpl implements cern.laser.client.data.Alarm, Cloneable, Serializable {
  private String alarmId;
  private Triplet triplet;
  private String systemName;
  private String identifier;
  private String problemDescription;
  private Integer priority;
  private String cause;
  private String action;
  private String consequence;
  private Source source;
  private URL helpURL;
  private String piquetGSM;
  private String piquetEmail;
  private ResponsiblePerson responsiblePerson;
  private Location location;
  private Vector categories;
  private Status status;
  private boolean instant;
  private boolean nodeParent;
  private boolean multiplicityParent;
  private boolean nodeChild;
  private boolean multiplicityChild;

  public AlarmImpl(cern.laser.business.data.Alarm alarm) {
    if (alarm == null) { throw new IllegalArgumentException("parameter can not be null"); }
    alarmId = alarm.getAlarmId();
    triplet = new TripletImpl(alarm.getTriplet());
    systemName = alarm.getSystemName();
    identifier = alarm.getIdentifier();
    problemDescription = alarm.getProblemDescription();
    priority = alarm.getPriority();
    cause = alarm.getCause();
    action = alarm.getAction();
    consequence = alarm.getConsequence();
    source = (alarm.getSource() == null ? null : new SourceImpl(alarm.getSource()));
    helpURL = alarm.getHelpURL();
    piquetGSM = alarm.getPiquetGSM();
    piquetEmail = alarm.getPiquetEmail();
    responsiblePerson = (alarm.getResponsiblePerson() == null ? null : new ResponsiblePersonImpl(alarm
        .getResponsiblePerson()));
    location = (alarm.getLocation() == null ? null : new LocationImpl(alarm.getLocation()));
    categories = new Vector();
    if (alarm.getCategories() != null) {
      Iterator iterator = alarm.getCategories().iterator();
      while (iterator.hasNext()) {
        categories.add(new CategoryImpl((cern.laser.business.data.Category) iterator.next()));
      }
    }
    status = (alarm.getStatus() == null ? null : new StatusImpl(alarm.getStatus()));
    instant = alarm.getInstant().booleanValue();
    nodeParent = alarm.hasNodeChildren();
    multiplicityParent = alarm.hasMultiplicityChildren();
    nodeChild = alarm.hasNodeParents();
    multiplicityChild = alarm.hasMultiplicityParents();
  }

  public String getAlarmId() {
    return alarmId;
  }

  public Triplet getTriplet() {
    return triplet;
  }

  public String getProblemDescription() {
    return problemDescription;
  }

  public Integer getPriority() {
    return priority;
  }

  public Source getSource() {
    return source;
  }

  public URL getHelpURL() {
    return helpURL;
  }

  public String getPiquetGSM() {
    return piquetGSM;
  }

  public String getPiquetEmail() {
    return piquetEmail;
  }

  public String getSystemName() {
    return systemName;
  }

  public String getIdentifier() {
    return identifier;
  }

  public String getCause() {
    return cause;
  }

  public String getAction() {
    return action;
  }

  public String getConsequence() {
    return consequence;
  }

  public Collection getCategories() {
    return (categories == null ? null : (Collection) categories.clone());
  }

  public Location getLocation() {
    return location;
  }

  public ResponsiblePerson getResponsiblePerson() {
    return responsiblePerson;
  }

  public Status getStatus() {
    return status;
  }

  public void setAction(String newAction) {
    action = newAction;
  }

  public void setAlarmId(String newAlarmId) {
    alarmId = newAlarmId;
  }

  public void setCategories(Collection newCategories) {
    categories = (newCategories == null ? null : new Vector(newCategories));
  }

  public void setCause(String newCause) {
    cause = newCause;
  }

  public void setConsequence(String newConsequence) {
    consequence = newConsequence;
  }

  public void setHelpURL(URL newHelpURL) {
    helpURL = newHelpURL;
  }

  public void setIdentifier(String newIdentifier) {
    identifier = newIdentifier;
  }

  public void setInstant(boolean newInstant) {
    instant = newInstant;
  }

  public void setLocation(Location newLocation) {
    location = newLocation;
  }

  public void setMultiplicityChild(boolean newMultiplicityChild) {
    multiplicityChild = newMultiplicityChild;
  }

  public void setMultiplicityParent(boolean newMultiplicityParent) {
    multiplicityParent = newMultiplicityParent;
  }

  public void setNodeChild(boolean newNodeChild) {
    nodeChild = newNodeChild;
  }

  public void setNodeParent(boolean newNodeParent) {
    nodeParent = newNodeParent;
  }

  public void setPiquetEmail(String newPiquetEmail) {
    piquetEmail = newPiquetEmail;
  }

  public void setPiquetGSM(String newPiquetGSM) {
    piquetGSM = newPiquetGSM;
  }

  public void setPriority(Integer newPriority) {
    priority = newPriority;
  }

  public void setProblemDescription(String newProblemDescription) {
    problemDescription = newProblemDescription;
  }

  public void setResponsiblePerson(ResponsiblePerson newResponsiblePerson) {
    responsiblePerson = newResponsiblePerson;
  }

  public void setSource(Source newSource) {
    source = newSource;
  }

  public void setStatus(Status newStatus) {
    status = newStatus;
  }

  public void setSystemName(String newSystemName) {
    systemName = newSystemName;
  }

  public void setTriplet(Triplet newTriplet) {
    triplet = newTriplet;
  }

  public boolean isInstant() {
    return instant;
  }

  public boolean isNodeParent() {
    return nodeParent;
  }

  public boolean isMultiplicityParent() {
    return multiplicityParent;
  }

  public boolean isNodeChild() {
    return nodeChild;
  }

  public boolean isMultiplicityChild() {
    return multiplicityChild;
  }

  public boolean equals(Object obj) {
    if ((obj == null) || (!(obj instanceof cern.laser.client.data.Alarm))) { return false; }
    cern.laser.client.data.Alarm alarm = (cern.laser.client.data.Alarm) obj;

    return getAlarmId().equals(alarm.getAlarmId());
  }

  public int hashCode() {
    return getAlarmId().hashCode();
  }

  public Object clone() throws CloneNotSupportedException {
    try {
      AlarmImpl alarm = (AlarmImpl) super.clone();
      alarm.setLocation(location == null ? null : (Location) location.clone());
      alarm.setResponsiblePerson(responsiblePerson == null ? null : (ResponsiblePerson) responsiblePerson.clone());
      alarm.setSource(source == null ? null : (Source) source.clone());
      alarm.setStatus(status == null ? null : (Status) status.clone());
      alarm.setTriplet(triplet == null ? null : (Triplet) triplet.clone());
      if (categories == null) {
        alarm.setCategories(null);
      } else {
        Vector categories_copy = new Vector();
        Iterator iterator = categories.iterator();
        while (iterator.hasNext()) {
          categories_copy.add(((Category) iterator.next()).clone());
        }
        alarm.setCategories(categories_copy);
      }

      return alarm;
    } catch (Exception e) {
      throw new CloneNotSupportedException("clone failed : " + e.getMessage());
    }
  }

  public String toString() {
    StringBuffer str_buf = new StringBuffer();
    str_buf.append("\nALARM :");
    str_buf.append("\nID : ");
    str_buf.append(getAlarmId());
    str_buf.append("\nTRIPLET : ");
    str_buf.append(getTriplet());
    str_buf.append("\nSYSTEM NAME : ");
    str_buf.append(getSystemName());
    str_buf.append("\nIDENTIFIER : ");
    str_buf.append(getIdentifier());
    str_buf.append("\nPROBLEM DESCRIPTION : ");
    str_buf.append(getProblemDescription());
    str_buf.append("\nPRIORITY : ");
    str_buf.append(getPriority());
    str_buf.append("\nHELP URL : ");
    str_buf.append(getHelpURL());
    str_buf.append("\nPIQUET GSM : ");
    str_buf.append(getPiquetGSM());
    str_buf.append("\nPIQUET EMAIL : ");
    str_buf.append(getPiquetEmail());
    str_buf.append("\nINSTANT : ");
    str_buf.append(isInstant());
    str_buf.append("\nNODE PARENT : ");
    str_buf.append(isNodeParent());
    str_buf.append("\nMULTIPLICITY PARENT : ");
    str_buf.append(isMultiplicityParent());
    str_buf.append("\nSOURCE : ");
    str_buf.append(getSource());
    str_buf.append("\nLOCATION : ");
    str_buf.append(getLocation());
    str_buf.append("\nRESPONSIBLE : ");
    str_buf.append(getResponsiblePerson());
    str_buf.append("\nSTATUS : ");
    str_buf.append(getStatus());
    str_buf.append("\nCATEGORIES : ");
    str_buf.append(getCategories());

    return str_buf.toString();
  }

}