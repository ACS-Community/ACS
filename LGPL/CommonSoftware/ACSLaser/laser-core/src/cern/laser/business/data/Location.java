/*
 * $Id: Location.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.business.data;

import java.io.Serializable;

/**
 * 
 * 
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 * @author Niall Stapley
 * @author Katarina Sigerud
 * 
 * @hibernate.class table="ALARM_LOCATION"
 */
public class Location implements Serializable, Cloneable {

  private String locationId;
  private String floor;
  private String room;
  private String mnemonic;
  private String position;
  private Building building;

  //
  // -- CONSTRUCTORS ------------------------------------------------
  //

  /**
   * Empty constructor for Hibernate.
   */
  private Location() {
  }

  /**
   * @param string
   * @param string2
   * @param string3
   * @param string4
   * @param string5
   * @param string6
   * @param string7
   * @param integer
   */
  public Location(String locationId, String floor, String mnemonic, String position, String room) {
    setLocationId(locationId);
    setFloor(floor);
    setMnemonic(mnemonic);
    setPosition(position);
    setRoom(room);
  }

  //
  // -- PUBLIC METHODS ----------------------------------------------
  //

  /**
   * 
   * @hibernate.id generator-class="assigned" column="LOCATION_ID"
   */
  public String getLocationId() {
    return locationId;
  }

  /**
   * @return Returns the floor.
   * @hibernate.property name="getFloor" column="FLOOR" not-null="false" unique="false"
   */
  public String getFloor() {
    return floor;
  }

  /**
   * @return Returns the mnemonic.
   * @hibernate.property name="getMnemonic" column="MNEMONIC" not-null="false" unique="false"
   */
  public String getMnemonic() {
    return mnemonic;
  }

  /**
   * @return Returns the position.
   * @hibernate.property name="getPosition" column="POSITION" not-null="false" unique="false"
   */
  public String getPosition() {
    return position;
  }

  /**
   * @return Returns the room.
   * @hibernate.property name="getRoom" column="ROOM" not-null="false" unique="false"
   */
  public String getRoom() {
    return room;
  }

  /**
   * @param locationId The locationId to set.
   */
  public void setLocationId(String alarmId) {
    this.locationId = alarmId;
  }

  /**
   * @param floor The floor to set.
   */
  public void setFloor(String floor) {
    this.floor = floor;
  }

  /**
   * @param mnemonic The mnemonic to set.
   */
  public void setMnemonic(String mnemonic) {
    this.mnemonic = mnemonic;
  }

  /**
   * @param position The position to set.
   */
  public void setPosition(String position) {
    this.position = position;
  }

  /**
   * @param room The room to set.
   */
  public void setRoom(String room) {
    this.room = room;
  }

  /**
   * @hibernate.many-to-one column="BUILDING" class="cern.laser.business.data.Building"
   *                        cascade="none" not-null="false"
   */
  public Building getBuilding() {
    return building;
  }

  public void setBuilding(Building building) {
    this.building = building;
  }

  public Object clone() {
    try {
      Location location_clone = (Location) super.clone();
      setBuilding(building == null ? null : (Building) building.clone());

      return location_clone;
    } catch (Exception e) {
      throw new InternalError();
    }

  }

  public int hashCode() {
    return getLocationId().hashCode();
  }

  public boolean equals(Object obj) {
    if (obj == null) { return false; }
    if (!(obj instanceof Location)) { return false; }
    Location location = (Location) obj;

    return getLocationId().equals(location.getLocationId());
  }

  public String toString() {
    StringBuffer str_buf = new StringBuffer();
    str_buf.append("\nMNEMONIC : ");
    str_buf.append(mnemonic);
    str_buf.append("\nFLOOR : ");
    str_buf.append(floor);
    str_buf.append("\nROOM : ");
    str_buf.append(room);
    str_buf.append("\nPOSITION : ");
    str_buf.append(position);

    if (building != null) {
      str_buf.append("\nBUILDING : ");
      str_buf.append(building.getBuildingNumber());
      str_buf.append("\nSITE : ");
      str_buf.append(building.getSite());
      str_buf.append("\nZONE : ");
      str_buf.append(building.getZone());
      str_buf.append("\nMAP : ");
      str_buf.append(building.getMap());
    }

    return str_buf.toString();
  }

  //
  // -- PROTECTED METHODS -------------------------------------------
  //

  //
  // -- PRIVATE METHODS ---------------------------------------------
  //

}