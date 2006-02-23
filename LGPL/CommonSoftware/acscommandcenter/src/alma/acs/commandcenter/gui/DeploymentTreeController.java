/*
 * Created on Oct 24, 2005 by mschilli
 */
package alma.acs.commandcenter.gui;

import java.util.logging.Logger;

import alma.acs.commandcenter.meta.IMaciSupervisor;




public interface DeploymentTreeController {

	/** supervisor factory method used when adding managers */
	public IMaciSupervisor giveMaciSupervisor(String managerLoc) throws Exception;

}


