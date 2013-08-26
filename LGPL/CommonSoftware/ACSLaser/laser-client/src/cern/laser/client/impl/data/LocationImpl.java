/*
 * $Id: LocationImpl.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client.impl.data;

import java.io.Serializable;

import cern.laser.business.data.Building;
import cern.laser.client.data.Location;

public class LocationImpl implements Location, Cloneable, Serializable {
  private String buildingNb;
  private String floor;
  private String room;
  private String site;
  private Integer zone;
  private String position;
  private String map;
  private String mnemonic;

  public LocationImpl(cern.laser.business.data.Location location) {
    if (location == null) { throw new IllegalArgumentException("parameter can not be null"); }
    floor = location.getFloor();
    room = location.getRoom();
    position = location.getPosition();
    mnemonic = location.getMnemonic();

    Building building = location.getBuilding();
    if (building != null) {
      buildingNb = building.getBuildingNumber();
      site = building.getSite();
      zone = building.getZone();
      map = building.getMap();
    }
  }

  public String getBuilding() {
    return buildingNb;
  }

  public String getFloor() {
    return floor;
  }

  public String getRoom() {
    return room;
  }

  public String getSite() {
    return site;
  }

  public Integer getZone() {
    return zone;
  }

  public String getPosition() {
    return position;
  }

  public String getMap() {
    return map;
  }

  public String getMnemonic() {
    return mnemonic;
  }

  public void setBuildingNb(String newBuilding) {
    buildingNb = newBuilding;
  }

  public void setFloor(String newFloor) {
    floor = newFloor;
  }

  public void setMap(String newMap) {
    map = newMap;
  }

  public void setMnemonic(String newMnemonic) {
    mnemonic = newMnemonic;
  }

  public void setPosition(String newPosition) {
    position = newPosition;
  }

  public void setRoom(String newRoom) {
    room = newRoom;
  }

  public void setSite(String newSite) {
    site = newSite;
  }

  public void setZone(Integer newZone) {
    zone = newZone;
  }

  public boolean equals(Object obj) {
    if ((obj == null) || (!(obj instanceof Location))) { return false; }

    return toString().equals(obj.toString());
  }

  public int hashCode() {
    return toString().hashCode();
  }

  public Object clone() throws CloneNotSupportedException {
    try {
      LocationImpl location = (LocationImpl) super.clone();

      return location;
    } catch (Exception e) {
      throw new CloneNotSupportedException("clone failed : " + e.getMessage());
    }
  }

  public String toString() {
    StringBuffer str_buf = new StringBuffer();
    str_buf.append("\nBUILDING : ");
    str_buf.append(getBuilding());
    str_buf.append("\nSITE : ");
    str_buf.append(getSite());
    str_buf.append("\nZONE : ");
    str_buf.append(getZone());
    str_buf.append("\nFLOOR : ");
    str_buf.append(getFloor());
    str_buf.append("\nROOM : ");
    str_buf.append(getRoom());
    str_buf.append("\nPOSITION : ");
    str_buf.append(getPosition());
    str_buf.append("\nMAP : ");
    str_buf.append(getMap());
    str_buf.append("\nMNEMONIC : ");
    str_buf.append(getMnemonic());

    return str_buf.toString();
  }

}