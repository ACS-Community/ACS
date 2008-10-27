/*
 * Created on Oct 24, 2005 by mschilli
 */
package alma.acs.commandcenter.gui;

import java.util.concurrent.Executor;

import alma.acs.commandcenter.meta.GuiMaciSupervisor;
import alma.acs.commandcenter.meta.Firestarter.OrbInitException;




public interface DeploymentTreeController {

	/** supervisor factory method used when adding managers */
	public GuiMaciSupervisor giveMaciSupervisor(String managerLoc) throws OrbInitException;

	/** used to perform work triggered from gui-buttons */
	public Executor getBackgroundExecutor();
	
}


