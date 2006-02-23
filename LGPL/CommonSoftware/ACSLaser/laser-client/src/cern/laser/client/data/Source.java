/*
 * $Id: Source.java,v 1.1 2005/06/06 18:19:40 kzagar Exp $
 *
 * $Date: 2005/06/06 18:19:40 $ 
 * $Revision: 1.1 $ 
 * $Author: kzagar $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client.data;

/** The alarm source class. */
public interface Source 
{
    /** Accessor method.
     * @return the source unique identifier
     */    
  public String getSourceId();

  /** Accessor method.
   * @return the source name
   */  
  public String getName();

  /** Accessor method.
   * @return the source description
   */  
  public String getDescription();

  /** Accessor method.
   * @return the source responsible
   */  
  public ResponsiblePerson getResponsiblePerson() ;

  /** Cloning method. The cloned object is a deep copy.
   * @return a deep copy of the object
   * @throws CloneNotSupportedException if clone failed
   */  
  public Object clone() throws CloneNotSupportedException;
  
}