/*
 * $Id: BeanDependentPropertyEditor.java,v 1.1 2005/06/07 03:26:13 kzagar Exp $
 *
 * $Date: 2005/06/07 03:26:13 $
 * $Revision: 1.1 $
 * $Author: kzagar $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.beans.editors.support;

/**
 * A <code>PropertyEditor</code> implementing this interface signals that it is dependant on the bean the property
 * belongs to. In other words, such a property editor needs a reference on the bean in order to perform the edition of
 * the property.
 * <p>
 * An entity instantiating a <code>PropertyEditor</code> implementing this interface should 
 * invoke the method <code>initializePropertyEditor</code> in order to pass the needed
 * information.
 * </p>
 *
 * @version $Revision: 1.1 $  $Date: 2005/06/07 03:26:13 $
 * @author Lionel Mestre
 */
public interface BeanDependentPropertyEditor {
  
  /**
   * Initializes this PropertyEditor with the bean the property belongs to and the 
   * name of the property.
   * @param bean the bean the property belongs to
   * @param propertyName the name of the property edited by this editor.
   */
  public void initializePropertyEditor(Object bean, String propertyName);

}
