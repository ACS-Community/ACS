/*
 * $Id: Location.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client.data;

/** The alarm location class. */
public interface Location 
{
  /** Accessor method.
   * @return the CERN building number
   */  
  public String getBuilding();

  /** Accessor method.
   * @return the floor number
   */  
  public String getFloor();

  /** Accessor method.
   * @return the room number
   */  
  public String getRoom();

  /** Accessor method.
   * @return the CERN site
   */  
  public String getSite();

  /** Accessor method.
   * @return the CERN safety zone
   */  
  public Integer getZone();

  /** Accessor method.
   * @return the alarm position reference
   */  
  public String getPosition();

  /** Accessor method.
   * @return the location mnemonic
   */  
  public String getMnemonic();

  /** Accessor method.
   * @return the map coordinates
   */  
  public String getMap();

  /** Cloning method. The cloned object is a deep copy.
   * @return a deep copy of the object
   * @throws CloneNotSupportedException if clone failed
   */  
  public Object clone() throws CloneNotSupportedException;
  
}