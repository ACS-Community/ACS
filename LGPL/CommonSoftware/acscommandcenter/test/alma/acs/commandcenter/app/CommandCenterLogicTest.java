/*
 * Created on Dec 21, 2004 by mschilli
 */
package alma.acs.commandcenter.app;

import java.io.File;

import junit.framework.TestCase;
import alma.entity.xmlbinding.acscommandcenterproject.AcsCommandCenterProject;



/**
 *
 * @author mschilli
 */
public class CommandCenterLogicTest extends TestCase {

	// =================== ProjectHandling ======================

	public void test_Write_Project () throws Exception {
		_Tests.enter(this);

		CommandCenterLogic logic = new CommandCenterLogic();
		logic.projectMaker = new ProjectMaker("v100.0");
		AcsCommandCenterProject p = logic.createProject();

		File f = new File("test_Write_Project.prj");
		logic.writeProject(p, f);

		AcsCommandCenterProject p2 = logic.readProject(f);

		assertEquals(p.getScriptBase(), p2.getScriptBase());
		assertEquals(p.getServicesLocalJavaRoot(), p2.getServicesLocalJavaRoot());

		f.delete();
	}

}

//
//
//
//
//
//
//
//
//
//
//
//