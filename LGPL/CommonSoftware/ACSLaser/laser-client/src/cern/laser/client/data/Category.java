/*
 * $Id: Category.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client.data;

/** The alarm category class. */
public interface Category 
{
    /** Accessor method.
     * @return the category unique identifier
     */    
  public Integer getCategoryId();

  /** Accessor method.
   * @return the category name
   */  
  public String getName();

  /** Accessor method.
   * @return the category description
   */  
  public String getDescription();

  /** Accessor method.
   * @return the category path from the category root (<I>name.name.name...name</I>)
   */  
  public String getPath();

  /** Accessor method.
   * @return true iff the category is a leaf in the category tree
   */  
  public boolean isLeaf();

  /** Cloning method. The cloned object is a deep copy.
   * @return a deep copy of the object
   * @throws CloneNotSupportedException if clone failed
   */  
  public Object clone() throws CloneNotSupportedException;
  
}