/*
 * $Id: UserProperty.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
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
 * @hibernate.class table="USER_PROPERTY"
 */
public class UserProperty implements Serializable {

  private String propertyId;
  private String name;
  private String value;
  private String statusId;
  
//
// -- CONSTRUCTORS ------------------------------------------------
//

  private UserProperty() {   
  }
  
  public UserProperty(String name, String value, String statusId) {
    setPropertyId(stringValue(name, statusId));
    setName(name);
    setValue(value);
    setStatusId(statusId);
  }
  
//
// -- PUBLIC METHODS ----------------------------------------------
//

  /**
   * Note: unsaved-value An identifier property value that indicates that an instance 
	 * is newly instantiated (unsaved), distinguishing it from transient instances that 
	 * were saved or loaded in a previous session.  If not specified you will get an exception like this:
	 * another object associated with the session has the same identifier
	 *
	 * @hibernate.id generator-class="assigned" column="PROPERTY_ID"
	 * @return integer
   */
  public String getPropertyId() {
    return propertyId;
  }

  public void setPropertyId(String newPropertyId) {
    propertyId = newPropertyId;
  }

  /**
   * @hibernate.property name="getName" column="NAME" not-null="true" unique="false"
	 * 
	 * @return String
   * @return
   */
  public String getName() {
    return name;
  }

  public void setName(String newName) {
    name = newName;
  }

  /**
   * @hibernate.property name="getValue" column="VALUE" not-null="false" unique="false"
	 * 
	 * @return String
   * @return
   */
  public String getValue() {
    return value;
  }

  public void setValue(String newValue) {
    value = newValue;
  }

  /**
   * @hibernate.property name="getStatusId" column="STATUS_ID" not-null="true" unique="false"
	 * 
   */
  public String getStatusId() {
    return statusId;
  }

  public void setStatusId(String newAlarmId) {
    statusId = newAlarmId;
  }

  public int hashCode() {
    return getPropertyId().hashCode();
  }

  public boolean equals(Object obj) {
    if (obj == null) { return false; }
    if (!(obj instanceof Alarm)) { return false; }
    UserProperty property = (UserProperty) obj;

    return getPropertyId().equals(property.getPropertyId());
  }
  
//
// -- PROTECTED METHODS -------------------------------------------
//

//
// -- PRIVATE METHODS ---------------------------------------------
//

  private static final String stringValue(String name, String statusId) {
    StringBuffer str_buf = new StringBuffer();
    str_buf.append(statusId);
    str_buf.append(":");
    str_buf.append(name);

    return str_buf.toString();
  }

}