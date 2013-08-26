/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2002 Copyright
 * by ESO (in the framework of the ALMA collaboration), All rights reserved
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms
 * of the GNU Lesser General Public License as published by the Free Software Foundation;
 * either version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with
 * this library; if not, write to the Free Software Foundation, Inc., 59 Temple Place,
 * Suite 330, Boston, MA 02111-1307 USA
 */

package alma.acs.commandcenter.engine;

import java.util.HashMap;

import junit.framework.TestCase;
import alma.acs.commandcenter.AccTests;
import alma.acs.commandcenter.engine.ExecuteTools.ToolStarter;
import alma.acs.commandcenter.engine._Tests.LogWriter;
import alma.acs.util.ACSPorts;
import alma.entity.xmlbinding.acscommandcentertools.Tool;


/**
 * This test is not a pure unit test, since it starts acs instances. That means,
 * it alters the (operating system level) environment. That means, errors in foregoing
 * test methods will have the consequence of errors in subsequent test methods.
 *   
 * 
 * @author mschilli
 */
public class ExternalToolsTest extends TestCase {

	static final int acs_instance = 9;
	static final String ACS_HOST = "localhost";
	
	static final String ACS_INSTANCE = String.valueOf(acs_instance);
	static final String MGR_PORT = ACSPorts.globalInstance(acs_instance).giveManagerPort();
	static final String IR_PORT = ACSPorts.globalInstance(acs_instance).giveIRPort();
	static final String NS_PORT = ACSPorts.globalInstance(acs_instance).giveNamingServicePort();

	static final int TIMEOUT=120;
	
	public ExternalToolsTest(String name) {
		super(name);
	}

	/**
	 * @see TestCase#setUp()
	 */
	@Override
	protected void setUp () throws Exception {
		super.setUp();
	}


	// =================== Acs ======================

	public void test_SetUp_Local_Acs () throws Throwable {
		_Tests.enter(this);

		final LogWriter listener = _Tests.giveTaskListener(super.getName());
		final ExecuteAcs x = _Tests.createExecuteAcs(ACS_INSTANCE);
		AccTests.assertCompletion(TIMEOUT, new AccTests.AssertableTask("Start Acs") {

			@Override
			public void action () throws Throwable {
				x.startLocalScript(listener);
			}
		});

		_Tests.assertExistsInstance(ACS_INSTANCE);
	}



	// =================== Some of the Extra Tools ======================

	
	public void test_Start_Local_ObjectExplorer () throws Throwable {
		_Tests.enter(this);
		
		Tool tool = _Tests.giveTool("Object Explorer");
		HashMap<String,Object> input = new HashMap<String,Object>();

		LogWriter listener = _Tests.giveTaskListener(super.getName());
		ExecuteTools x = _Tests.createExecuteTools(ACS_INSTANCE, "127.0.0.1", MGR_PORT, IR_PORT, NS_PORT);
		ToolStarter ts = x.addTool(tool, input);

		ts.start(listener);
		_Tests.sleep(10*1000);
	}
	
	public void test_Start_Local_LoggingClientGraphical () throws Throwable {
		_Tests.enter(this);
		
		Tool tool = _Tests.giveTool("Logging Client (graphical)");
		HashMap<String,Object> input = new HashMap<String,Object>();

		LogWriter listener = _Tests.giveTaskListener(super.getName());
		ExecuteTools x = _Tests.createExecuteTools(ACS_INSTANCE, "127.0.0.1", MGR_PORT, IR_PORT, NS_PORT);
		ToolStarter ts = x.addTool(tool, input);

		ts.start(listener);
		_Tests.sleep(10*1000);
	}
	
	public void test_Start_Local_CdbBrowser () throws Throwable {
		_Tests.enter(this);
		
		Tool tool = _Tests.giveTool("Cdb Browser");
		HashMap<String,Object> input = new HashMap<String,Object>();

		LogWriter listener = _Tests.giveTaskListener(super.getName());
		ExecuteTools x = _Tests.createExecuteTools(ACS_INSTANCE, "127.0.0.1", MGR_PORT, IR_PORT, NS_PORT);
		ToolStarter ts = x.addTool(tool, input);

		ts.start(listener);
		_Tests.sleep(10*1000);
	}

	public void test_Start_Local_DynamicComponentsClient () throws Throwable {
		_Tests.enter(this);
		
		Tool tool = _Tests.giveTool("Dynamic Components Client");
		HashMap<String,Object> input = new HashMap<String,Object>();

		LogWriter listener = _Tests.giveTaskListener(super.getName());
		ExecuteTools x = _Tests.createExecuteTools(ACS_INSTANCE, "127.0.0.1", MGR_PORT, IR_PORT, NS_PORT);
		ToolStarter ts = x.addTool(tool, input);

		ts.start(listener);
		_Tests.sleep(10*1000);
	}
	
	public void test_Start_Local_NameServiceBrowser () throws Throwable {
		_Tests.enter(this);
		
		Tool tool = _Tests.giveTool("Name Service Browser");
		HashMap<String,Object> input = new HashMap<String,Object>();

		LogWriter listener = _Tests.giveTaskListener(super.getName());
		ExecuteTools x = _Tests.createExecuteTools(ACS_INSTANCE, "127.0.0.1", MGR_PORT, IR_PORT, NS_PORT);
		ToolStarter ts = x.addTool(tool, input);

		ts.start(listener);
		_Tests.sleep(10*1000);
	}
	

/* Problem: 
 * I cannot bring the eventbrowser down afterwards, because apparently
 * "actual pid" = "the eventbrowser-pid that bash reports to accHelper" + 1
 * 
 * Solution: 
 * Trusting in the fact that it's always "+ 1" would be crap,
 * so this test is disabled for now
 * 
	public void test_Start_Local_EventBrowser () throws Throwable {
		_Tests.enter(this);
		
		Tool tool = _Tests.giveTool("Event Browser");
		HashMap input = new HashMap();

		LogWriter listener = _Tests.giveTaskListener(super.getName());
		ExecuteTools x = _Tests.createExecuteTools(ACS_INSTANCE, "127.0.0.1", MGR_PORT, IR_PORT, NS_PORT);
		ToolStarter ts = x.addTool(tool, input);

		ts.start(listener);
		_Tests.sleep(10*1000);
	}
*/
	public void test_Kill_Tools() throws Throwable {
		_Tests.enter(this);

		LogWriter listener = _Tests.giveTaskListener(super.getName());
		Executor.localOutProc("accEnableVars accStopper $HOME/.acs/commandcenter/objexp $HOME/.acs/commandcenter/jlog $HOME/.acs/commandcenter/loggingClient $HOME/.acs/commandcenter/cdbBrowser $HOME/.acs/commandcenter/JDynAct $HOME/.acs/commandcenter/IRBrowser $HOME/.acs/commandcenter/NameManager $HOME/.acs/commandcenter/acseventbrowser", true, -1L, null, listener);
	}

	
	// ======================================================================
	
	public void test_TearDown_Local_Acs () throws Throwable {
		_Tests.enter(this);

		final LogWriter listener = _Tests.giveTaskListener(super.getName());
		final ExecuteAcs x = _Tests.createExecuteAcs(ACS_INSTANCE);
		AccTests.assertCompletion(TIMEOUT, new AccTests.AssertableTask("Stop Acs") {

			@Override
			public void action () throws Throwable {
				x.stopLocalScript(listener);
			}
		});

		_Tests.assertNotExistsInstance(ACS_INSTANCE);
	}

	
	
	
}


