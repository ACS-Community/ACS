/*
 * $Id: Filter.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client.services.selection;
import java.io.Serializable;

import cern.laser.client.LaserException;
import cern.laser.client.impl.services.selection.FilterImpl;

/** An alarm filter object. It defines a filtering criteria of the form <I>property
 * operator value</I>, where <I>property</I> is an alarm property (<I>fault family,
 * ..., priority, building number, etc</I>)
 */
public abstract class Filter implements Serializable
{
  /** The predefined operators array. */  
  public static final String operators[] = FilterImpl.operators();
  
  /** The predefined properties array. */  
  public static final String properties[] = FilterImpl.properties(); 

  /** Factory method. Create a filter instance.
   * @return a new Filter instance
   */  
  public static Filter create() 
  {
    return new FilterImpl();
  }
  
  /** Factory method. Create a filter instance given the parameters.
   * @param property the alarm property to filter against
   * @param operator the filtering operator to apply
   * @param value the value to filter
   * @return a new Filter instance
   */  
  public static Filter create(String property, String operator, String value) 
  {
    return new FilterImpl(property, operator, value);
  }

  /** Accessor method.
   * @return the alarm property to filter against
   */  
  public abstract String getProperty();
  /** Accessor method.
   * @param newProperty the alarm property to filter against
   */  
  public abstract void setProperty(String newProperty);
  /** Accessor method.
   * @return the filtering operator
   */  
  public abstract String getOperator();
  /** Accessor method.
   * @param newOperator Accessor method.
   */  
  public abstract void setOperator(String newOperator);
  /** Accessor method.
   * @return the value to filter
   */  
  public abstract String getValue();
  /** Accessor method.
   * @param newValue the value to filter
   */  
  public abstract void setValue(String newValue);
  /** Validation method.
   * @throws LaserException if the filter is not valid
   */  
  public abstract void validate() throws LaserException;

}