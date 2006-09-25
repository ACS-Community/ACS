/*
 * $Id: ConsoleUser.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.business.data;


/**
 * 
 * 
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 * @author Katarina Sigerud
 * 
 * @hibernate.class table="LASER_USER" discriminator-value="false"
 * @hibernate.discriminator column="IS_ADMIN" type="boolean"
 */
public class ConsoleUser {
  public String userId;
  public Integer ident;
  public String password;
  public String defaultPrinter;
  public Integer defaultConfiguration;

  //
  // -- CONSTRUCTORS ------------------------------------------------
  //
  protected ConsoleUser() {
  }

  public ConsoleUser(String name, String password) {
      setUserId(name);
      setPassword(password);
  }

  //
  // -- PUBLIC METHODS ----------------------------------------------
  //

  /**
	 *
	 * @hibernate.id generator-class="assigned" column="USER_ID"
   */
  public String getUserId() {
    return userId;
  }

  /**
   * @hibernate.property name="getIdent" column="IDENT" not-null="false" unique="false"
   */
  public Integer getIdent() {
    return ident;
  }

  public String getName() {
    return getUserId();
  }

  /**
   * @return Returns the password.
   * @hibernate.property name="getPassword" column="PASSWORD" not-null="false" unique="false"
   */
  public String getPassword() {
    return password;
  }

  /**
   * @hibernate.property name="getDefaultPrinter" column="DEFAULT_PRINTER" not-null="false" unique="false"
   */
  public String getDefaultPrinter() {
    return defaultPrinter;
  }

  public void setDefaultPrinter(String defaultPrinter) {
    this.defaultPrinter = defaultPrinter;
  }

  public Integer getDefaultConfiguration() {
    return defaultConfiguration;
  }

  public void setDefaultConfiguration(Integer defaultConfiguration) {
    this.defaultConfiguration = defaultConfiguration;
  }

  public int hashCode() {
    return getUserId().hashCode();
  }

  public boolean equals(Object obj) {
    if (obj == null) { return false; }
    if (!(obj instanceof ConsoleUser)) { return false; }
    ConsoleUser user = (ConsoleUser) obj;

    return getUserId().equals(user.getUserId());
  }

  //
  // -- PROTECTED METHODS -------------------------------------------
  //

  /**
   * @param userId The userId to set.
   */
  protected void setUserId(String userId) {
    this.userId = userId;
  }

  protected void setIdent(Integer ident) {
    this.ident = ident;
  }

  /**
   * @param password The password to set.
   */
  protected void setPassword(String password) {
    this.password = password;
  } 

  //
  // -- PRIVATE METHODS ---------------------------------------------
  //
}