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

import junit.framework.TestCase;
import alma.acs.commandcenter.AccTests;
import alma.acs.commandcenter.engine._Tests.LogWriter;
import alma.acs.util.ACSPorts;


/**
 * This test is not a pure unit test, since it starts acs instances. That means, it alters
 * the (operating system level) environment. That means, errors in foregoing test methods
 * will have the consequence of errors in subsequent test methods.
 * 
 * 
 * @author mschilli
 */
public class AcsScriptsTest extends TestCase {

	static final int acs_instance = 9;
	static final String ACS_HOST = "localhost";

	static final String ACS_INSTANCE = String.valueOf(acs_instance);
	static final String MGR_PORT = ACSPorts.globalInstance(acs_instance).giveManagerPort();
	static final String IR_PORT = ACSPorts.globalInstance(acs_instance).giveIRPort();
	static final String NS_PORT = ACSPorts.globalInstance(acs_instance).giveNamingServicePort();
	
	static final int TIMEOUT=120;

	public AcsScriptsTest(String name) {
		super(name);
	}


	/**
	 * @see TestCase#setUp()
	 */
	@Override
	protected void setUp () throws Exception {
		super.setUp();
	}

	@Override
	protected void tearDown () throws Exception {
		super.tearDown();
	}


	// =================== ServicesManager ======================

	public void test_Start_Local_ServicesAndManager () throws Throwable {
		_Tests.enter(this);

		final LogWriter listener = _Tests.giveTaskListener(super.getName());

		final ExecuteServices x = _Tests.createExecuteServices(ACS_INSTANCE);
		AccTests.assertCompletion(TIMEOUT, new AccTests.AssertableTask("Start Services") {

			@Override
			public void action () throws Throwable {
				x.startLocalScript(listener);
			}
		});


		final ExecuteManager y = _Tests.createExecuteManager(ACS_INSTANCE);
		AccTests.assertCompletion(TIMEOUT, new AccTests.AssertableTask("Start Manager") {

			@Override
			public void action () throws Throwable {
				y.startLocalScript(listener);
			}
		});

		_Tests.assertExistsInstance(ACS_INSTANCE);
	}


	public void test_Stop_Local_ServicesAndManager () throws Throwable {
		_Tests.enter(this);

		final LogWriter listener = _Tests.giveTaskListener(super.getName());

		final ExecuteManager y = _Tests.createExecuteManager(ACS_INSTANCE);
		AccTests.assertCompletion(TIMEOUT, new AccTests.AssertableTask("Stop Manager") {

			@Override
			public void action () throws Throwable {
				y.stopLocalScript(listener);
			}
		});

		final ExecuteServices x = _Tests.createExecuteServices(ACS_INSTANCE);
		AccTests.assertCompletion(TIMEOUT, new AccTests.AssertableTask("Stop Services") {

			@Override
			public void action () throws Throwable {
				x.stopLocalScript(listener);
			}
		});


		_Tests.assertNotExistsInstance(ACS_INSTANCE);
	}


	// =================== Acs ======================

	public void test_Start_Local_Acs () throws Throwable {
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


	public void test_Stop_Local_Acs () throws Throwable {
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


	// =================== AcsContainers ======================

	public void test_Start_Local_AcsAndContainers () throws Throwable {
		_Tests.enter(this);

		final LogWriter listener = _Tests.giveTaskListener(super.getName());

		final ExecuteAcs x = _Tests.createExecuteAcs(ACS_INSTANCE);
		AccTests.assertCompletion(TIMEOUT, new AccTests.AssertableTask("Start Acs") {

			@Override
			public void action () throws Throwable {
				x.startLocalScript(listener);
			}
		});


		final ExecuteContainer y1 = new ExecuteContainer(); 
		final RunModel z1 = _Tests.createContainerRunModel(ACS_INSTANCE, "java", "javaContainer");
		AccTests.assertCompletion(TIMEOUT, new AccTests.AssertableTask("Start javaContainer") {

			@Override
			public void action () throws Throwable {
				y1.startLocalScript(z1, listener);
			}
		});


		final ExecuteContainer y2 = new ExecuteContainer(); 
		final RunModel z2 =_Tests.createContainerRunModel(ACS_INSTANCE, "cpp", "cppContainer");
		AccTests.assertCompletion(TIMEOUT, new AccTests.AssertableTask("Start cppContainer") {

			@Override
			public void action () throws Throwable {
				y2.startLocalScript(z2, listener);
			}
		});


		final ExecuteContainer y3 = new ExecuteContainer();
		final RunModel z3 = _Tests.createContainerRunModel(ACS_INSTANCE, "py", "pythonContainer");
		AccTests.assertCompletion(TIMEOUT, new AccTests.AssertableTask("Start pythonContainer") {

			@Override
			public void action () throws Throwable {
				y3.startLocalScript(z3, listener);
			}
		});

		_Tests.assertExistsInstance(ACS_INSTANCE);
	}


	public void test_Stop_Local_AcsAndContainers () throws Throwable {
		_Tests.enter(this);

		final LogWriter listener = _Tests.giveTaskListener(super.getName());

		final ExecuteContainer y1 = new ExecuteContainer();
		final RunModel z1 = _Tests.createContainerRunModel(ACS_INSTANCE, "java", "javaContainer");
		AccTests.assertCompletion(TIMEOUT, new AccTests.AssertableTask("Stop javaContainer") {

			@Override
			public void action () throws Throwable {
				y1.stopLocalScript(z1, listener);
			}
		});


		final ExecuteContainer y2 = new ExecuteContainer();
		final RunModel z2 = _Tests.createContainerRunModel(ACS_INSTANCE, "cpp", "cppContainer");
		AccTests.assertCompletion(TIMEOUT, new AccTests.AssertableTask("Stop cppContainer") {

			@Override
			public void action () throws Throwable {
				y2.stopLocalScript(z2, listener);
			}
		});


		final ExecuteContainer y3 = new ExecuteContainer();
		final RunModel z3 = _Tests.createContainerRunModel(ACS_INSTANCE, "py", "pythonContainer");
		AccTests.assertCompletion(TIMEOUT, new AccTests.AssertableTask("Stop pythonContainer") {

			@Override
			public void action () throws Throwable {
				y3.stopLocalScript(z3, listener);
			}
		});


		final ExecuteAcs x = _Tests.createExecuteAcs(ACS_INSTANCE);
		AccTests.assertCompletion(TIMEOUT, new AccTests.AssertableTask("Stop Acs") {

			@Override
			public void action () throws Throwable {
				x.stopLocalScript(listener);
			}
		});


		_Tests.assertNotExistsInstance(ACS_INSTANCE);
	}


	// ===================================================================
	// ========================== Helpers ================================
	// ===================================================================



}


