/*
 * DummyBeanBeanInfo.java
 *
 * Created on September 30, 2002, 2:26 PM
 */

package cern.gp.explorer.test.helpers;

import cern.gp.beans.BeanInfoSupport;
import cern.gp.explorer.test.helpers.ColoredIntegerEditor;
import java.beans.IntrospectionException;
import java.beans.PropertyDescriptor;

/**
 * A bean info class for a ColoredBean
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 * @author  Vito Baggiolini
 */
public class ColoredBeanBeanInfo extends BeanInfoSupport {
  
  public PropertyDescriptor[] getPropertyDescriptorsLazy() {
    try {
      PropertyDescriptor p1 = new PropertyDescriptor("int", ColoredBean.class);
      p1.setPropertyEditorClass(ColoredIntegerEditor.class);
      p1.setShortDescription("an integer tooltip");
      return new PropertyDescriptor[] { p1 };
    } catch (IntrospectionException ex) {
      ex.printStackTrace();
      System.err.println("ColoredBeanBeanInfo.getPropertyDescriptorsLazy() - exceptional return"); //@
      return null;
    }
  }
}
