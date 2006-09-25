/*
 * $Id: WorkspaceSwitchListenerSupport.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */

package cern.gp.windows;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import org.openide.util.WeakListener;
import org.openide.windows.WindowManager;
import org.openide.windows.Workspace;

/**
 * A helper class to listen to workspace switches.
 * It listens to the Workspace changes and notifies the appropriate xxEntered and xxExited methods .
 * The user has to write a sub-class that overrides the methods s/he is interested in. For instance,
 * if a program wants to be informed any time a workspace is exited, s/he will override the method
 * workspaceExited(). Or, as another example, if a window wants to be informed whenever its own
 * Workspace is entered, it will use the constructor WorkspaceSwitchListenerSupport(Workspace) and
 * override the method myWorkspaceEntered().
 *
 * @author  Vito Baggiolini
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 */
public class WorkspaceSwitchListenerSupport implements PropertyChangeListener {
  private final Workspace myWorkspace;
  private final boolean mineOnly;
  // a boolean to keep track whether we are already listening
  private boolean listening;
  private final Object listeningLock = new Object(); // the lock to synchronize access to stop/startListening
  
  /**
   * constructor to be used if you want to be informed about which workspace is
   * being entered. This constructor will only invoke the methods workspaceEntered/Exited
   */
  protected WorkspaceSwitchListenerSupport() {
    this(null, false);
  }
  
  /**
   * constructor to be used if you want to be informed about a specific workspace.
   * if this constructor is used and mineOnly==false all four Entered/Exited methods will be called;
   * if mineOnly == true, only myWorkspaceEntered/Exited will be called
   * @param myWorkspace the workspace for which entering and exiting is monitored
   * @param whether only my workspace shall be monitored, if true, the methodsworkspaceEntered/Exited
   * will not be invoked.
   */
  protected WorkspaceSwitchListenerSupport(Workspace myWorkspace, boolean mineOnly) {
    this.myWorkspace = myWorkspace;
    this.mineOnly = mineOnly;
    startListening();
  }
  
  /**
   * tell the workspace listener to stop listening.
   * This method is thread safe with regards to its counterpart
   * You don't <em>have</em> to call this in the end, as we use a weaklistener
   * @see #startListening
   */
  public void stopListening() {
    if (listening) {
      synchronized (listeningLock) {
        if (listening) { // double-check condition          
          WindowManager.getDefault().removePropertyChangeListener(this);
          listening = !listening;
        }
      }
    }
  }
  /**
   * tell this object to start listening again. This method is called in the constructor.
   * It is not necessary (but not harmful either) to call this menthod again even if we are
   * already listening. Precautions are taken to avoid adding a second listener.
   * This method is thread safe with regards to its counterpart
   * @see #stopListening
   */
  public void startListening() {
    if (!listening) {
      synchronized(listeningLock) {
        if (!listening) { // double-check condition
          PropertyChangeListener propL = WeakListener.propertyChange(this, WindowManager.getDefault());
          
          WindowManager.getDefault().addPropertyChangeListener(propL);
          listening = !listening;
        }
      }
    }
  }
  
  public void propertyChange(PropertyChangeEvent pce) {
    if (!WindowManager.PROP_CURRENT_WORKSPACE.equals(pce.getPropertyName())) { return; }
    if (!mineOnly) {
      workspaceExited((Workspace)pce.getOldValue());
      workspaceEntered((Workspace)pce.getNewValue());
    }
    if (myWorkspace != null) {
      if (myWorkspace == pce.getOldValue()) {
        myWorkspaceExited();
      } else if (myWorkspace == pce.getNewValue()) {
        myWorkspaceEntered();
      }
    }
  }
  protected void workspaceEntered(Workspace ws) {}
  protected void workspaceExited(Workspace ws) {}
  protected void myWorkspaceEntered() {}
  protected void myWorkspaceExited() {}
}
