/*******************************************************************************
 ALMA - Atacama Large Millimiter Array
 (c) European Southern Observatory, 2015

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
/*
 Created on Mar 4, 2015 by M.Schilling
 */
package alma.acs.commandcenter.meta;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Matchers.anyString;

import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import javax.swing.event.TreeModelEvent;
import javax.swing.event.TreeModelListener;

import junit.framework.TestCase;

import org.mockito.Mockito;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.omg.CORBA.ORB;

import si.ijs.maci.Administrator;
import si.ijs.maci.ClientInfo;
import si.ijs.maci.ComponentInfo;
import si.ijs.maci.ContainerInfo;
import si.ijs.maci.Manager;


public class MaciSupervisorTest extends TestCase {

	MaciSupervisor testee;
	
	ORB orb;
	Manager manager;
	Administrator administrator;
	Logger log;

	@Override
	public void setUp() throws Exception {
		System.out.println("\n--- "+getName()+" ----------------");

		
		// make the manager
		// -----------------------------------------------------------------

		orb = Mockito.mock (ORB.class);
		manager = Mockito.mock (Manager.class);
		administrator = Mockito.mock (Administrator.class);

		final int hhhhh = 0;
		final int[] empty = new int[]{};

		ComponentInfo comp100 = new ComponentInfo("type", "code", null, "comp100", empty, 10, "cont10", 100, 0, new String[]{});
		ComponentInfo comp200 = new ComponentInfo("type", "code", null, "comp200", empty, 20, "cont20", 200, 0, new String[]{});
		ComponentInfo comp300 = new ComponentInfo("type", "code", null, "comp300", empty, 30, "cont30", 300, 0, new String[]{});
		ComponentInfo[] one_comp = {comp100};
		ComponentInfo[] two_comps = {comp100,comp200};
		ComponentInfo[] three_comps = {comp100,comp200,comp300};

		ContainerInfo cont10 = new ContainerInfo("cont10", 10, null, empty);
		ContainerInfo cont20 = new ContainerInfo("cont20", 20, null, empty);
		ContainerInfo cont30 = new ContainerInfo("cont30", 30, null, empty);
		ContainerInfo[] one_cont = {cont10};
		ContainerInfo[] two_conts = {cont10,cont20};
		ContainerInfo[] three_conts = {cont10,cont20,cont30};

		ClientInfo clientA = new ClientInfo(0, null, empty, "clientA", 0);
		ClientInfo client1 = new ClientInfo(1, null, empty, "client1", 0);
		ClientInfo client2 = new ClientInfo(2, null, empty, "client2", 0);
		ClientInfo client3 = new ClientInfo(3, null, empty, "client3", 0);
		ClientInfo[] one_client = {client1};
		ClientInfo[] two_clients = {client1,client2};
		ClientInfo[] three_clients = {client1,client2,client3};

		Mockito.when(orb.string_to_object("dummy")).thenReturn (manager);
		Mockito.when(manager.login(administrator)).thenReturn (clientA);

		Mockito.when(manager.get_component_info (hhhhh, empty, "*", "*", false)).thenReturn (one_comp, two_comps, three_comps);
		Mockito.when(manager.get_container_info(hhhhh, empty, "*")).thenReturn (one_cont, one_cont, two_conts, three_conts);
		Mockito.when(manager.get_client_info (hhhhh, empty, "*")).thenReturn(one_client, two_clients, three_clients, two_clients, three_clients);

		
		// make the supervisor
		// -----------------------------------------------------------------

		log = new Logger ("Test", null) {
			final long start = System.nanoTime();
			@Override public void log(LogRecord r) {
				long sinceStart = (System.nanoTime() - start) /1000/1000/1000;
				System.out.println(String.format("%2d",sinceStart) +"  "+ r.getLevel() +"  "+ r.getMessage());}
		};
		log.setLevel (Level.FINE);


		testee = new MaciSupervisor ("Test", "dummy", orb, log);
		testee.acImpl = testee.new AdministratorImplementation() {
			@Override
			protected Administrator asCorbaObject (ORB orb) {
				return administrator;
			}
		};

		testee.start();


		// assertions
		// ----------------------------------------------------------------

		maciListener = new MaciInfoListener();
		MaciInfo maciInformation = testee.getMaciInformation();
		maciInformation.addTreeModelListener(maciListener);

	}


	MaciInfoListener maciListener;
	
	class MaciInfoListener implements TreeModelListener {
		public void treeNodesChanged (TreeModelEvent e) {fail("documentation says this gets never called");}
		public void treeNodesInserted (TreeModelEvent e) {fail("documentation says this gets never called");}
		public void treeNodesRemoved (TreeModelEvent e) {fail("documentation says this gets never called");}
		public void treeStructureChanged (TreeModelEvent e) {
			nMaciChangeEvents += 1;

			//MaciInfo maciInformation = (MaciInfo) e.getSource();
			//List<ContainerInfo> containers = maciInformation.getContainers();
			//List<ComponentInfo> components = maciInformation.getComponents();
			//List<ClientInfo> clients = maciInformation.getClients();
		}

		volatile int nMaciChangeEvents = 0;

		void checkReceived (int n) {
			assertEquals (n, nMaciChangeEvents);
			nMaciChangeEvents = 0;
			System.out.println("check succeeded, well done!");
		}

	}

	@Override
	public void tearDown() throws Exception {
		testee.stop();
	}
	
	public void testAccumulation() throws Exception {

		// idea: send several events. macisupervisor should
		// collect them and forward them in batches. then we
		// count the batches.

		setAccumulation(2); // default value
		sendEvent();
		sendEvent();
		sendEvent();
		sleep(2);
		sendEvent();
		sendEvent();
		sendEvent();
		sleep(2);
		maciListener.checkReceived(2);

		setAccumulation(6); // ICT-3399 value
		sendEvent();
		sendEvent();
		sendEvent();
		sleep(2);
		sendEvent();
		sendEvent();
		sendEvent();
		sleep(2);
		maciListener.checkReceived(1);
	}

	public void testIdlePolling() throws Exception {

		// idea: similar to above. but this time we
		// don't send events, instead we wait until
		// the supervisor discovers the changes.

		testee.setRefreshesPeriodically(true);

		setIdlePolling(4); // default value is 10 (too big for this test)
		sleep (5);
		maciListener.checkReceived(1);

		setIdlePolling(8); // ICT-3399 value is 30 (too big for this test)
		sleep (5);
		maciListener.checkReceived(0);
		sleep (5);
		maciListener.checkReceived(1);
	}

	void sendEvent() {
		log.info ("sending change event");
		testee.acImpl.container_logged_out (-1, -1);
	}

	void sleep (int secs) throws Exception {
		Thread.sleep (secs * 1000);
	}


	// MaciSupervisor API only allows to set both values in one go.
	// i'm providing two distinct methods to change each value separately.
	
	int currLag = 2, currPeriod = 10; // the default values of MaciSupervisor

	void setAccumulation (int lag) {
		System.out.println("setting accumulation to "+lag+"s");
		testee.setRefreshDelay (currLag = lag, currPeriod);
	}
	void setIdlePolling (int period) {
		System.out.println("setting idle polling to "+period+"s");
		testee.setRefreshDelay (currLag, currPeriod = period);
	}


	// msc: it's not always easy to get the arguments for mocking right.
	// the below code registers for any arguments, and prints the arguments.
	void debug () throws Exception {
		Mockito.when (manager.login(administrator))
					.thenAnswer(new Answer<ClientInfo>() {
			public ClientInfo answer (InvocationOnMock invocation) throws Throwable {
				System.out.println(Arrays.toString(invocation.getArguments()));
				Thread.sleep(5000);
				return new ClientInfo();
			}
		});

		Mockito.when (manager.get_component_info(anyInt(), any(int[].class), anyString(), anyString(), anyBoolean()))
					.thenAnswer(new Answer<ComponentInfo[]>() {
			public ComponentInfo[] answer (InvocationOnMock invocation) throws Throwable {
				System.out.println("get_component_info (" + Arrays.toString(invocation.getArguments()) + ")");
				Thread.sleep(5000);
				return new ComponentInfo[] {};
			}
		});

	}
	
}


