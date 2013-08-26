/*
 * BeanActionSupport.java
 *
 * Created on October 4, 2002, 9:07 PM
 */

package cern.gp.actions.support;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import cern.gp.capabilities.Capability;
import cern.gp.nodes.GPNode;
import cern.gp.util.GPManager;

/**
 * An action to be invoked on Beans, and to be used in combination with Capabilities.
 * Your actions should inherit from this action and instantiate their superclass
 * (this class) passing the class of the Capability as an argument.
 * For an example, please refer to {<code>gp.example.actions.beans.SmileAction</code>)
 * 
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author  Vito Baggiolini
 */
public class BeanActionSupport extends BeanAction {

  private Method capabilityMethod;
  private boolean isNoArg;

  /**
   * Constructor to use if you don't need to specify the selection mode.
   * @param capabilityClass the class of the corresponding capability
   * @see cern.gp.actions.support.BeanAction#BeanAction(Class)
   */
  public BeanActionSupport(Class capabilityClass) throws IllegalArgumentException {
  	this(capabilityClass, MODE_ANY);
  }
  
  /**
   * Constructor that allows to specify the selectionMode
   * The selection mode determines how many nodes must be selected to enable the
   * action. The possible modes are defined in the class 
   * {@link org.openide.util.actions.CookieAction}
   * @param capabilityClass the class of the corresponding capability
   * @param selectionMode see fields of 
   *   {@link org.openide.util.actions.CookieAction}
   */
  public BeanActionSupport(Class capabilityClass, int selectionMode) throws IllegalArgumentException {
    super(capabilityClass, selectionMode);
    Method[] methods;
    methods = capabilityClass.getDeclaredMethods();
    if (methods.length != 1) {
      throw new IllegalArgumentException(
        "parameter " + capabilityClass + " must have exactly one method with no argument");
    }
    capabilityMethod = methods[0];
    if (capabilityMethod.getParameterTypes().length == 0) {
      isNoArg = true;
    } else if (
      capabilityMethod.getParameterTypes().length != 1
        || !GPNode.class.isAssignableFrom(capabilityMethod.getParameterTypes()[0])) {
      throw new IllegalArgumentException(
        "parameter " + capabilityClass + " must have exactly one method with no argument or with a GPNode as argument");
    }
  }

  /** 
   * Performs the associated capability for the given node.
   * @param node the node for which the capability is performed
   * @param capability the capability to perform
   *
   */
  protected void performCapability(GPNode node, Capability capability) throws InvocationTargetException {
    try {
      if (isNoArg) {
        capabilityMethod.invoke(capability, null);
      } else {
        capabilityMethod.invoke(capability, new Object[] {node});
      }
    } catch (IllegalAccessException ex) {
      GPManager.notify(GPManager.EXCEPTION, ex);
    }
  }
}
