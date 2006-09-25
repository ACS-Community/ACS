/*
 * $Id: Source.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
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