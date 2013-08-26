/*
 * $Id: Building.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
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
 * @author Katarina Sigerud
 * @hibernate.class table="BUILDINGS" mutable="false"
 */
 public class Building implements Serializable, Cloneable {
  private String buildingNumber;
  private String site;
  private Integer zone;
  private String map;

  //
  // -- CONSTRUCTORS ------------------------------------------------
  //

  private Building() {
  }

  /**
   * @param buildingNumber
   * @param site2
   * @param zone2
   * @param map2
   */
  public Building(String buildingNumber, String site, Integer zone, String map) {
    setBuildingNumber(buildingNumber);
    setSite(site);
    setZone(zone);
    setMap(map);
  }

  //
  // -- PUBLIC METHODS ----------------------------------------------
  //

  /**
   * 
   * @hibernate.id generator-class="assigned" column="BUILDING"
   * @return string
   */
  public String getBuildingNumber() {
    return buildingNumber;
  }

  /**
   * @param buildingNumber The buildingNumber to set.
   */
  public void setBuildingNumber(String building) {
    this.buildingNumber = building;
  }

  /**
   * @return Returns the site.
   * @hibernate.property name="getSite" column="SITE" not-null="false" unique="false"
   */
  public String getSite() {
    return site;
  }

  /**
   * @param site The site to set.
   */
  private void setSite(String site) {
    this.site = site;
  }

  /**
   * @return Returns the zone.
   * @hibernate.property name="getZone" column="ZONE" not-null="false" unique="false"
   */
  public Integer getZone() {
    return zone;
  }

  /**
   * @param zone The zone to set.
   */
  private void setZone(Integer zone) {
    this.zone = zone;
  }

  /**
   * @return Returns the map.
   * @hibernate.property name="getMap" column="MAP" not-null="false" unique="false"
   */
  public String getMap() {
    return map;
  }

  /**
   * @param map The map to set.
   */
  private void setMap(String map) {
    this.map = map;
  }
  
  public Object clone() {
    try {
      Building building_clone = (Building) super.clone();

      return building_clone;
    } catch (Exception e) {
      throw new InternalError();
    }

  }

  public int hashCode() {
    return getBuildingNumber().hashCode();
  }

  public boolean equals(Object obj) {
    if (obj == null) { return false; }
    if (!(obj instanceof Building)) { return false; }
    Building another_building = (Building) obj;

    return getBuildingNumber().equals(another_building.getBuildingNumber());
  }

  //
  // -- PROTECTED METHODS -------------------------------------------
  //

  //
  // -- PRIVATE METHODS ---------------------------------------------
  //

}