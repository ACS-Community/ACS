/*
 * Created on Mar 3, 2003
 */
package cern.gp.actions.support;

import java.lang.ref.WeakReference;

import cern.gp.nodes.GPNode;

/**
 * An action that contains a context in a static variable. <p>
 * (The implementation uses a weak reference and to help you avoid memory leaks.)
 * Please remember that GP (NetBeans) actions are singletons, and you are not supposed 
 * to instantiate them.
 *  
 * @author Vito Baggiolini
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $ 
 * 
 */
public abstract class ContextAction extends NodeAction {
  private static WeakReference ref;

  /**
   * set a context object. Please remember that Actions are singletons, and therfore
   * this context is shared by all places where this action is used.
   * 
   * @param ctx the object containing the context
   */
  public static void setContext(Object ctx) {
    ref = new WeakReference(ctx);
  }

  /**
   * Clear the context and return it.
   * @return the context set. May be null
   */
  public static Object unsetContext() {
    if (ref == null) { 
      return null;
    }
    Object ctx = ref.get();
    ref = null;
    return ctx;
  }

  /**
   * the action will be enabled only if the super class enables it
   * and if the context is not null.
   * @see org.openide.util.actions.NodeAction#enable(Node[])
   *
  protected boolean enable(Node[] nodes) {
    return super.enable(nodes) && (ref != null) && (ref.get() != null);
  }*/
  
  /* (non-Javadoc)
   * @see cern.gp.actions.support.NodeAction#performAction(cern.gp.nodes.GPNode[])
   */
  protected final void performAction(GPNode[] activatedNodes) {
    Object ctx = null;
    if (ref != null) {
      ctx = ref.get();
      if (ctx == null) {
        ref = null;
      }
    }
    performAction(activatedNodes, ctx);
  }

  /**
   * A method called by GP when you invoke this action. You have to implement this
   * method with the action you want to execute. Please note that the context parameter
   * may be null.
   * 
   * @param activatedNodes the nodes currently selected
   * @param context the object set with {@link #setContext(Object)} can be null
   */
  protected abstract void performAction(GPNode[] activatedNodes, Object context);

}
