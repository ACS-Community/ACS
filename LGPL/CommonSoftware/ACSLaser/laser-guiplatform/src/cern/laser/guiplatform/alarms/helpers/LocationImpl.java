package cern.laser.guiplatform.alarms.helpers;
import java.net.URL;

import cern.laser.client.data.Location;

public class LocationImpl implements Location, Cloneable
{
  private Integer locationId;
  private String description;
  private String building;
  private String floor;
  private String room;
  private String site;
  private Integer safetyZone;
  private String position;
  private URL mapURL;
  private String installation;

  //public LocationImpl(cern.laser.business.data.Location location)
  public LocationImpl()
  {
      locationId = new Integer(-1);
      description = "description";
      building = "-1";
      floor = "-1";
      room = "-1";
      site = "site";
      safetyZone = new Integer(-1);
      position = "position";
      mapURL = null;;
      installation = "installation";


    /*
    if (location == null) 
    {
      throw new IllegalArgumentException("parameter can not be null");
    }
    
    locationId = (location.getLocationId() == null ? 0 : location.getLocationId().intValue());
    description = location.getDescription();
    building = (location.getBuilding() == null ? 0 : location.getBuilding().intValue());
    floor = (location.getFloor() == null ? 0 : location.getFloor().intValue());
    room = (location.getRoom() == null ? 0 : location.getRoom().intValue());
    site = location.getSite();
    safetyZone = (location.getSafetyZone() == null ? 0 : location.getSafetyZone().intValue());
    position = location.getPosition();
    mapURL = location.getMapURL();
    installation = location.getInstallation();
    */
  }

  private LocationImpl(Location location)
  {
    if (location == null) 
    {
      throw new IllegalArgumentException("parameter can not be null");
    }
    //locationId = location.getLocationId();
    //description = new String(location.getDescription());
    building = location.getBuilding();
    floor = location.getFloor();
    room = location.getRoom();
    site = new String(location.getSite());
    //safetyZone = location.getSafetyZone();
    position = new String(location.getPosition());
    //try 
    //{
    //  mapURL = (location.getMapURL() == null ? null : new URL(location.getMapURL().toExternalForm()));
    //} catch (MalformedURLException e) 
    //{
    //  mapURL = null;
    //}
    //installation = new String(location.getInstallation());
  }

  //public Integer getLocationId()
  //{
  //  return locationId;
  //}

  public String getDescription()
  {
    return description;
  }

  public String getBuilding()
  {
    return building;
  }

  public String getFloor()
  {
    return floor;
  }

  public String getRoom()
  {
    return room;
  }

  public String getSite()
  {
    return site;
  }

  public Integer getSafetyZone()
  {
    return safetyZone;
  }

  public String getPosition()
  {
    return position;
  }

  public URL getMapURL()
  {
    return mapURL;
  }

  public String getInstallation()
  {
    return installation;
  }

  /////////////////////////////////////////////////////////////
  public String getMap() {
      return null;
  }
  public String getMnemonic() {
      return "mnemonic";
  }
  public Integer getZone() {
      return null;
  }
  
  //////////////////////////////////////////////////////////////
  
  public Object clone() throws CloneNotSupportedException 
  {
    return new LocationImpl(this);
  }  
  
  public String toString() 
  {
    StringBuffer str_buf = new StringBuffer();
    str_buf.append("\nID : ");
    //str_buf.append(getLocationId());
    str_buf.append("\nDESCRIPTION : ");
    str_buf.append(getDescription());
    str_buf.append("\nBUILDING : ");
    str_buf.append(getBuilding());
    str_buf.append("\nFLOOR : ");
    str_buf.append(getFloor());
    str_buf.append("\nROOM : ");
    str_buf.append(getRoom());
    str_buf.append("\nSITE : ");
    str_buf.append(getSite());
    str_buf.append("\nSAFETY ZONE : ");
    str_buf.append(getSafetyZone());
    str_buf.append("\nPOSITION : ");
    str_buf.append(getPosition());
    str_buf.append("\nMAP URL : ");
    str_buf.append(getMapURL());
    str_buf.append("\nINSTALLATION : ");
    str_buf.append(getInstallation());
    
    return str_buf.toString();
  }
}
