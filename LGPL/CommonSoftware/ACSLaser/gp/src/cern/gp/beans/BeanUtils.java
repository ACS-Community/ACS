/*
 * $Id: BeanUtils.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.beans;

import java.beans.Introspector;
import java.beans.PropertyChangeListener;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import org.openide.util.WeakListener;

/**
 * A few utility methods for Beans.
 * 
 * @author Lionel Mestre
 * @version $REvision: $ $Date: 2006/09/25 08:52:36 $
 */
public class BeanUtils {

  private BeanUtils() {
  }

  /**
   * Generates a unique name for the bean using the same convention as the 
   * implementation of toString() in Object, which is :
   *   "bean class name"+@+"hash code of the bean in hexadecimal"
   * @param bean the bean the name has to be generated from
   * @return a unique name for the bean
   */
  public static final String generateUniqueBeanName(Object bean) {
    return bean.getClass().getName() + "@" + Integer.toHexString(bean.hashCode());
  }

  /**
   * Registers a set of paths in the <code>PropertyEditor</code> search path of 
   * the <code>java.beans.PropertyEditorManager</code>.
   * <p>
   * Use this method when you have <code>PropertyEditor</code> named following the convention
   * <i>Typename</i>Editor that are not located in the same package as <i>Typename</i>. 
   * For a given java type <i>a.b.X</i>, the <code>java.beans.PropertyEditorManager</code> will 
   * first check if there is an editor registered for the type, if not it will look for the editor
   * <i>a.b.XEditor</i> and if not found it will look in the search path for the class <i>XEditor</i>.
   * You can use this method to register the path where <i>XEditor</i> is. For instance, if the editor is located in
   * <code>a.c.d.XEditor</code> you would pass in parameter to this method <code>new String[] {"a.c.d"}</code>.
   * </p><p>
   * Note that paths are not added if they are already present.
   * </p>
   * @param pathsToRegister the array of paths to register in the search path of the PropertyEditorManager.
   */
  public static final void registerEditorSearchPaths(String[] pathsToRegister) {
    java.beans.PropertyEditorManager.setEditorSearchPath(
      computePaths(java.beans.PropertyEditorManager.getEditorSearchPath(), pathsToRegister));
  }

  /**
   * Registers a set of paths in the <code>BeanInfo</code> search path of 
   * the <code>java.beans.Introspector</code>.
   * <p>
   * Use this method when you have <code>BeanInfo</code> named following the convention <i>BeanName</i>BeanInfo that are
   * not located in the same package as <i>BeanName</i>. For a given java bean <i>a.b.X</i>, the <code>java.beans.
   * Introspector</code> will first check if there is a BeanInfo <i>a.b.XBeanInfo</i> and if not found it will look in
   * the search path for the class <i>XBeanInfo</i>. You can use this method to register the path where <i>XBeanInfo</i>
   * is. For instance, if the BeanInfo is located in <code>a.c.d.XBeanInfo</code> you would pass in parameter
   * to this method <code>new String [] {"a.c.d"}</code>.
   * </p><p>
   * Note that paths are not added if they are already present.
   * </p>
   * @param pathsToRegister the array of paths to register in the search path of the Introspector.
   */
  public static final void registerBeanInfoSearchPaths(String[] pathsToRegister) {
    Introspector.setBeanInfoSearchPath(computePaths(Introspector.getBeanInfoSearchPath(), pathsToRegister));
  }

  private static final String[] computePaths(String[] existingPaths, String[] pathsToRegister) {
    if (pathsToRegister == null || pathsToRegister.length == 0) {
      return existingPaths;
    }
    if (existingPaths == null || existingPaths.length == 0) {
      return pathsToRegister;
    }
    String[] newPaths = (String[]) pathsToRegister.clone();
    int n = 0;
    for (int i = 0; i < newPaths.length; i++) {
      if (checkDoesBeanInfoPathExist(existingPaths, newPaths[i])) {
        newPaths[i] = null;
      } else {
        n++;
      }
    }
    if (n == 0)
      return existingPaths;
    int m = existingPaths.length;
    String[] resultingPaths = new String[n + m];
    System.arraycopy(existingPaths, 0, resultingPaths, 0, m);
    for (int i = 0; i < newPaths.length; i++) {
      if (newPaths[i] != null) {
        resultingPaths[m] = newPaths[i];
        m++;
      }
    }
    return resultingPaths;
  }

  private static final boolean checkDoesBeanInfoPathExist(String[] paths, String path) {
    int n = paths.length;
    for (int i = 0; i < n; i++) {
      if (paths[i].equals(path))
        return true;
    }
    return false;
  }

  /**
   * This is a utility method to help in loading icon images. It takes the pathname of a resource file 
   * associated with the a given class and loads an image object from that file. Typically images will be GIFs.
   * <p>
   * The pathname should be relative to the given class and be contained in the classpath.
   * For instance if the related class is in <code>cern.gp.beans</code> and the icon is stored in
   * <code>cern/gp/beans/images/MyIcon.gif</code> the pathname to give would be 
   * <code>images/MyIcon.gif</code>.
   * </p>
   * @param imagePathname  A pathname relative to the directory holding the class file of 
   * the given relatedClass. For example, <code>images/MyIcon.gif</code>.
   * @param relatedClass the class the image is related with.
   * @return  an image object. May be null if the load failed.
   */
  public static final java.awt.Image loadImage(final String imagePathname, final Class relatedClass) {
    try {
      java.awt.image.ImageProducer ip = (java.awt.image.ImageProducer) java.security.AccessController.doPrivileged(
      new java.security.PrivilegedAction() {
        public Object run() {
          java.net.URL url = relatedClass.getResource(imagePathname);
          if (url == null)
            return null;
          try {
            return url.getContent();
          } catch (java.io.IOException ioe) {
            return null;
          }
        }
      });
      if (ip == null)
        return null;
      java.awt.Toolkit tk = java.awt.Toolkit.getDefaultToolkit();
      return tk.createImage(ip);
    } catch (Exception ex) {
      return null;
    }
  }

  /**
   * Finds the first public superclass of the given bean.
   * Should not introspect on a private class, because then the method objects
   * used for the property descriptors will not be callable without an
   * IllegalAccessException, even if overriding a public method from a public superclass.
   * @param bean the bean to find the public class from
   * @return the first public superclass, possibly the class of the given bean
   */
  public static final Class findTargetClass(Object bean) {
    Class clazz = bean.getClass();
    while (!Modifier.isPublic(clazz.getModifiers()) && !hasExplicitBeanInfo(clazz)) {
      clazz = clazz.getSuperclass();
      if (clazz == null)
        clazz = Object.class; // in case it was an interface
    }
    return clazz;
  }

  /**
   * Checks whether there is an explicit bean info for given class.
   * @param clazz the class to test
   * @return true if explicit bean info exists
   */
  public static final boolean hasExplicitBeanInfo(Class clazz) {
    String className = clazz.getName();
    int indx = className.lastIndexOf('.');
    className = className.substring(indx + 1);
    String[] paths = Introspector.getBeanInfoSearchPath();
    for (int i = 0; i < paths.length; i++) {
      String s = paths[i] + '.' + className + "BeanInfo";
      try {
        // test if such class exists
        Class.forName(s);
        return true;
      } catch (ClassNotFoundException ex) {
        // OK, this is normal.
      }
    }
    return false;
  }

  /**
   * Add a weak PropertyChangeListener to an object, using reflection to look up the addPropertyChangeListener method
   * @param listener the listener to add
   * @param source the object to which a PCL shall be added
   */
  public static void addWeakListenerByReflection(PropertyChangeListener listener, Object source) {
    try {
      PropertyChangeListener pcl = WeakListener.propertyChange(listener, source);
      Method addPCL =
        source.getClass().getMethod(
          "addPropertyChangeListener",
          new Class[] { java.beans.PropertyChangeListener.class });
      addPCL.invoke(source, new Object[] { pcl });
    } catch (Exception ex) {
      // [PENDING] should use logging API here
      System.err.println("Warning: unable to add a property change listener to object " + source);
      //ex.printStackTrace();
    }
  }

}
