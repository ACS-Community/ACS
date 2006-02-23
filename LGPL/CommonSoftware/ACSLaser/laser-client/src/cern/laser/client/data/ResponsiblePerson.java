/*
 * $Id: ResponsiblePerson.java,v 1.1 2005/06/06 18:19:40 kzagar Exp $
 *
 * $Date: 2005/06/06 18:19:40 $ 
 * $Revision: 1.1 $ 
 * $Author: kzagar $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client.data;

/** The alarm responisble person class. */
public interface ResponsiblePerson 
{
    /** Accessor method.
     * @return the responsible unique identifier
     */    
  public Integer getResponsibleId();

  /** Accessor method.
   * @return the responsible first name
   */  
  public String getFirstName();

  /** Accessor method.
   * @return the responsible family name
   */  
  public String getFamilyName();

  /** Accessor method.
   * @return the e-mail address
   */  
  public String getEMail();

  /** Accessor method.
   * @return the GSM number
   */  
  public String getGsmNumber();

  /** Accessor method.
   * @return the telephone number
   */  
  public String getPhoneNumber();

  /** Cloning method. The cloned object is a deep copy.
   * @return a deep copy of the object
   * @throws CloneNotSupportedException if clone failed
   */  
  public Object clone() throws CloneNotSupportedException;
  
}