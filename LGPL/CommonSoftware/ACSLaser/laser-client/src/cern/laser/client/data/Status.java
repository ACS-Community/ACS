/*
 * $Id: Status.java,v 1.1 2005/06/06 18:19:40 kzagar Exp $
 *
 * $Date: 2005/06/06 18:19:40 $ 
 * $Revision: 1.1 $ 
 * $Author: kzagar $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client.data;
import java.sql.Timestamp;
import java.util.Properties;

/** The alarm dynamic information class. It describes fault state information
 * related to its status. The alarm sources determine the status changes to apply and
 * distribute.*/
public interface Status 
{
    /** Accessor method.
     * @return true iff the alarm is active
     */    
  public boolean isActive();

  /** Accessor method.
   * @return true iff the alarm is maintenance/mode masked
   */  
  public boolean isMasked();

  /** Accessor method.
   * @return true iff the alarm is multiplicity/node reduced
   */  
  public boolean isReduced();

  /** Accessor method.
   * @return the alarm source hostname
   */  
  public String getSourceHostname();

  /** Accessor method.
   * @return the most recent alarm change source timestamp
   */  
  public Timestamp getSourceTimestamp();

  /** Accessor method.
   * @return the most recent alarm change user timestamp
   */  
  public Timestamp getUserTimestamp();

  /** Accessor method.
   * @return the most recent alarm change system timestamp
   */  
  public Timestamp getSystemTimestamp();

  /** Accessor method.
   * @return the alarm change user properties
   */  
  public Properties getUserProperties();

  /** Cloning method. The cloned object is a deep copy.
   * @return a deep copy of the object
   * @throws CloneNotSupportedException if clone failed
   */  
  public Object clone() throws CloneNotSupportedException;
  
}