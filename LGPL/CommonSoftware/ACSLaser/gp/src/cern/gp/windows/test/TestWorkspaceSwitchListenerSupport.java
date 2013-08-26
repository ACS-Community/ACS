/*
 * TestWorkspaceListenerSupport.java
 *
 * Created on October 2, 2002, 5:42 PM
 */

package cern.gp.windows.test;

import cern.gp.windows.WorkspaceSwitchListenerSupport;
import org.openide.windows.WindowManager;
import org.openide.windows.Workspace;
import cern.gp.util.GPManager;

/**
 * An subclass of the WorspaceSwitchListenerSupport that simply prints information 
 * for each of the 4 workspaceEntered/Exited methods
 * 
 * @author Vito Baggiolini
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 */
public class TestWorkspaceSwitchListenerSupport extends WorkspaceSwitchListenerSupport {
  
  /** Creates a new instance of TestWorkspaceListenerSupport */
  public TestWorkspaceSwitchListenerSupport() {
    super();
  }
  
  public TestWorkspaceSwitchListenerSupport(Workspace myWorkspace, boolean mineOnly) {
    super(myWorkspace, mineOnly);
  }
  protected void myWorkspaceEntered() {
    GPManager.getStdOut().println("entered my workspace");
  }
  protected void myWorkspaceExited() {
    GPManager.getStdOut().println("exited my workspace");
  }
  protected void workspaceEntered(Workspace ws) {
    GPManager.getStdOut().println("entered " + ws.getDisplayName());
  }
  protected void workspaceExited(Workspace ws) {
    GPManager.getStdOut().println("exited " + ws.getDisplayName());
  }
  protected void finalize() {
    try { super.finalize(); } catch (Throwable thr) { /*ignore*/ }
  }
  
  public static void main(String[] args) {
    WorkspaceSwitchListenerSupport wsls = 
      new TestWorkspaceSwitchListenerSupport(WindowManager.getDefault().getCurrentWorkspace(), false);
    GPManager.getStdOut().println("---- WorkspaceSwitchListener registered (listening for 30') ----");
    try { Thread.sleep(30000); } catch (InterruptedException ex) { ex.printStackTrace(); }
    // stopping is not necessary, if it is not called, the WeakListener will finalize after some 20'
    // wsls.stopListening(); 
    GPManager.getStdOut().println("---- WorkspaceSwitchListener unregistered ----------------------");
  }
}
