/*
 * $Id: Alarm.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client.data;

import java.net.URL;
import java.util.Collection;

/**
 * The alarm class. It describes a fault state in terms of its static and dynamic information.
 */
public interface Alarm {
  /**
   * Accessor method.
   * 
   * @return the alarm unique identifier
   */
  public String getAlarmId();

  /**
   * Accessor method.
   * 
   * @return the alarm unique triplet identifier
   */
  public Triplet getTriplet();

  /**
   * Accessor method.
   * 
   * @return the alarm system name
   */
  public String getSystemName();

  /**
   * Accessor method.
   * 
   * @return the alarm system identifier
   */
  public String getIdentifier();

  /**
   * Accessor method.
   * 
   * @return the problem description
   */
  public String getProblemDescription();

  /**
   * Accessor method.
   * 
   * @return the alarm priority (1..4)
   */
  public Integer getPriority();

  /**
   * Accessor method.
   * 
   * @return the alarm cause
   */
  public String getCause();

  /**
   * Accessor method.
   * 
   * @return the alarm action to be taken
   */
  public String getAction();

  /**
   * Accessor method.
   * 
   * @return the alarm consequence
   */
  public String getConsequence();

  /**
   * Accessor method.
   * 
   * @return the alarm source
   */
  public Source getSource();

  /**
   * Accessor method.
   * 
   * @return the alarm URL, null if not available
   */
  public URL getHelpURL();

  /**
   * Accessor method.
   * 
   * @return the alarm piquet GSM, null if not available
   */
  public String getPiquetGSM();

  /**
   * Accessor method.
   * 
   * @return the alarm piquet email, null if not available
   */
  public String getPiquetEmail();

  /**
   * Accessor method.
   * 
   * @return the alarm attached categories
   */
  public Collection getCategories();

  /**
   * Accessor method.
   * 
   * @return the alarm location
   */
  public Location getLocation();

  /**
   * Accessor method.
   * 
   * @return the alarm responsible
   */
  public ResponsiblePerson getResponsiblePerson();

  /**
   * Accessor method.
   * 
   * @return the alarm dynamic information
   */
  public Status getStatus();

  /**
   * Accessor method.
   * 
   * @return true, iff it is an instant alarm
   */
  public boolean isInstant();

  /**
   * Accessor method.
   * 
   * @return true iff it is a multiplicity parent
   */
  public boolean isMultiplicityParent();

  /**
   * Accessor method.
   * 
   * @return true iff it is a node parent
   */
  public boolean isNodeParent();

  /**
   * Accessor method.
   * 
   * @return true iff it is a multiplicity child
   */
  public boolean isMultiplicityChild();

  /**
   * Accessor method.
   * 
   * @return true iff it is a node child
   */
  public boolean isNodeChild();

  /**
   * Cloning method. The cloned object is a deep copy.
   * 
   * @return a deep copy of the object
   * @throws CloneNotSupportedException if clone failed
   */
  public Object clone() throws CloneNotSupportedException;

}