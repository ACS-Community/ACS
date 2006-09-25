/*
 * $Id: Alarm.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.business.data;

import java.net.URL;
import java.util.Collection;

import cern.laser.business.definition.data.AlarmDefinition;

/**
 * 
 * 
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 * @author Katarina Sigerud
 */
public interface Alarm {
  //
  public abstract String getAlarmId();

  /**
   * @return Returns the action.
   */
  public abstract String getAction();

  /**
   * @return Returns the categories.
   */
  public abstract Collection getCategories();

  /**
   * @param categories The categories to set.
   */
  public abstract void setCategories(Collection newCategories);

  /**
   * @return Returns the cause.
   */
  public abstract String getCause();

  /**
   * @return Returns the consequence.
   */
  public abstract String getConsequence();

  /**
   * @return Returns the identifier.
   */
  public abstract String getIdentifier();

  /**
   * @return Returns the instant.
   */
  public abstract Boolean getInstant();

  public abstract void setHelpURL(URL newHelpURL);

  /**
   * Returns the help URL for this alarm, or <code>null</code> if there is none or if it is malformed.
   * 
   * @return the help URL for this alarm, or <code>null</code> if there is none or if it is malformed.
   */
  public abstract URL getHelpURL();

  /**
   * @return Returns the location. 
   * This was mapped as one-to-one, but in the future the table Location will have its own id.
   */
  public abstract Location getLocation();

  /**
   * Returns the ids of the multiplicity children alarms.
   * 
   * @return the ids of the multiplicity children alarms.
   */
  public abstract String[] getMultiplicityChildren();

  /**
   * @return
   */
  public abstract boolean hasMultiplicityChildren();

  /**
   * @return
   */
  public abstract boolean hasMultiplicityParents();

  /**
   * Returns the ids of the multiplicity parent alarms.
   * 
   * @return the ids of the multiplicity parent alarms.
   */
  public abstract String[] getMultiplicityParents();

  /**
   * Sets up the bi-directional relation between the multiplicity parent and the multiplicity child
   * 
   * @param multiplicityChild the alarm to add to this alarm as a multiplicity child. This alarm will at the same time
   *          be added to the child's multiplicity parents.
   */
  public abstract void addMultiplicityChild(Alarm multiplicityChild);

  /**
   * Removes the bi-directional relation between the multiplicity parent and the multiplicity child
   * 
   * @param multiplicityChild the alarm to remove from this alarm as a multiplicity child. This alarm will at the same
   *          time be removed from the child's multiplicity parents.
   */
  public abstract void removeMultiplicityChild(Alarm multiplicityChild);

  /**
   * Returns the ids of the node children alarms.
   * 
   * @return the ids of the node children alarms.
   */
  public abstract String[] getNodeChildren();

  /**
   * @return
   */
  public abstract boolean hasNodeChildren();

  /**
   * Returns the ids of the node parent alarms.
   * 
   * @return the ids of the node parent alarms.
   */
  public abstract String[] getNodeParents();

  /**
   * @return
   */
  public abstract boolean hasNodeParents();

  /**
   * Sets up the bi-directional relation between the node parent and the node child
   * 
   * @param nodeChild the alarm to add to this alarm as a node child. This alarm will at the same time be added to the
   *          child's node parents.
   */
  public abstract void addNodeChild(Alarm nodeChild);

  /**
   * Removes the bi-directional relation between the node parent and the multiplicity child
   * 
   * @param nodeChild the alarm to remove from this alarm as a node child. This alarm will at the same time be removed
   *          from the child's node parents.
   */
  public abstract void removeNodeChild(Alarm nodeChild);

  /**
   */
  public abstract Integer getMultiplicityThreshold();

  /**
   */
  public abstract void setMultiplicityThreshold(Integer multiplicityThreshold);

  /**
   */
  public abstract String getPiquetEmail();

  /**
   */
  public abstract String getPiquetGSM();

  /**
   */
  public abstract Integer getPriority();

  /**
   */
  public abstract String getProblemDescription();

  /**
   */
  public abstract ResponsiblePerson getResponsiblePerson();

  /**
   *  
   */
  public abstract Source getSource();

  /**
   * @return Returns the status.
   */
  public abstract Status getStatus();

  /**
   * @param status The status to set.
   */
  public abstract void setStatus(Status status);

  /**
   * @return Returns the systemName.
   */
  public abstract String getSystemName();

  /**
   * @return Returns the triplet.
   */
  public abstract Triplet getTriplet();

  /**
   * @return
   */
  public abstract AlarmDefinition getDefinition();

  public abstract void setDefinition(AlarmDefinition definition);

  /**
   * @param action The action to set.
   */
  public abstract void setAction(String action);

  /**
   * @param alarmId The alarmId to set.
   */
  public abstract void setAlarmId(String alarmId);

  /**
   * @param cause The cause to set.
   */
  public abstract void setCause(String cause);

  /**
   * @param consequence The consequence to set.
   */
  public abstract void setConsequence(String consequence);

  /**
   * @param identifier The identifier to set.
   */
  public abstract void setIdentifier(String identifier);

  /**
   * @param instant The instant to set.
   */
  public abstract void setInstant(Boolean instant);

  /**
   * @param location The location to set.
   */
  public abstract void setLocation(Location location);

  /**
   * @param piquetEmail The piquetEmail to set.
   */
  public abstract void setPiquetEmail(String piquetEmail);

  /**
   * @param piquetGSM The piquetGSM to set.
   */
  public abstract void setPiquetGSM(String piquetGSM);

  /**
   * @param priority The priority to set.
   */
  public abstract void setPriority(Integer priority);

  /**
   * @param problemDescription The problemDescription to set.
   */
  public abstract void setProblemDescription(String problemDescription);

  /**
   * @param responsiblePerson The responsiblePerson to set.
   */
  public abstract void setResponsiblePerson(ResponsiblePerson responsiblePerson);

  /**
   * @param source The source to set.
   */
  public abstract void setSource(Source source);

  /**
   * @param systemName The systemName to set.
   */
  public abstract void setSystemName(String systemName);

  /**
   * @param triplet The triplet to set.
   */
  public abstract void setTriplet(Triplet triplet);
}