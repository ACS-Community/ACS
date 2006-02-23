/*
 * $Id: Triplet.java,v 1.1 2005/06/06 18:19:40 kzagar Exp $
 *
 * $Date: 2005/06/06 18:19:40 $ 
 * $Revision: 1.1 $ 
 * $Author: kzagar $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client.data;

/** The alarm unique public identifier class. */
public interface Triplet 
{
    /** Accessor method.
     * @return the fault family
     */    
  public String getFaultFamily();

  /** Accessor method.
   * @return the fault member
   */  
  public String getFaultMember();

  /** Accessor method.
   * @return the fault code
   */  
  public Integer getFaultCode();

  /** Cloning method. The cloned object is a deep copy.
   * @return a deep copy of the object
   * @throws CloneNotSupportedException if clone failed
   */  
  public Object clone() throws CloneNotSupportedException;
  
}