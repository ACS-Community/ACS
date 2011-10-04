/*
 * Created on 25.10.2003
 *  
 */
package alma.acs.commandcenter.app;

import java.awt.Rectangle;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.net.URL;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;

import javax.help.HelpSet;
import javax.help.HelpSetException;

import org.exolab.castor.xml.MarshalException;
import org.exolab.castor.xml.ValidationException;
import org.omg.CORBA.ORB;

import alma.acs.commandcenter.engine.ExecuteAcs;
import alma.acs.commandcenter.engine.ExecuteContainer;
import alma.acs.commandcenter.engine.ExecuteManager;
import alma.acs.commandcenter.engine.ExecuteServices;
import alma.acs.commandcenter.engine.ExecuteTools;
import alma.acs.commandcenter.engine.Executor;
import alma.acs.commandcenter.engine.ToolManager;
import alma.acs.commandcenter.engine.ExecuteTools.ToolStarter;
import alma.acs.commandcenter.gui.CommandCenterGui;
import alma.acs.commandcenter.gui.DeploymentTreeController;
import alma.acs.commandcenter.meta.Firestarter;
import alma.acs.commandcenter.meta.GuiMaciSupervisor;
import alma.acs.commandcenter.meta.IMaciSupervisor;
import alma.acs.commandcenter.meta.Firestarter.OrbInitException;
import alma.acs.commandcenter.util.MiscUtils;
import alma.acs.logging.AcsLogger;
import alma.acs.util.ACSPorts;
import alma.entity.xmlbinding.acscommandcenterproject.AcsCommandCenterProject;
import alma.entity.xmlbinding.acscommandcenterproject.ContainerT;
import alma.entity.xmlbinding.acscommandcentertools.AcsCommandCenterTools;
import alma.entity.xmlbinding.acscommandcentertools.Tool;

/**
 * The business logic for Command Center.
 *  
 */
public class CommandCenterLogic {

	
	//
	// ============ Members / Delegates ====================
	//
	
	public AcsCommandCenterProject project;
	
	public MyProjectRunModel model;
	
	public StartupOptions startupOptions;

	public ExecuteServices executeServices;

	public ExecuteManager executeManager;

	public ExecuteContainer executeContainer;

	public ExecuteAcs executeAcs;

	public ExecuteTools executeTools;

	public Firestarter firestarter;
	

	protected CommandCenterGui gui;

	protected ProjectMaker projectMaker;
	
	protected AcsLogger log;
	

	//
	// ============ Startup / Shutdown ====================
	//

	
	public void prepare (StartupOptions startupOptions) {
		this.startupOptions = startupOptions;
		
		log = MiscUtils.getPackageLogger(this);

		// Make up the creator string for command center projects.
		projectCreatorId = (version().equals(""))? null : "acc-"+version();

		projectMaker = new ProjectMaker(projectCreatorId);
		project = projectMaker.createProject();
		model = new MyProjectRunModel(project);

		executeServices = new ExecuteServices(model);
		executeManager = new ExecuteManager(model);
		executeContainer = new ExecuteContainer();
		executeAcs = new ExecuteAcs(model);
		executeTools = new ExecuteTools(model);

		firestarter = new Firestarter("AcsCommandCenter", log, null);
		Executor.remoteDaemonEnable(firestarter); // msc (2007-11): needed for talking to daemons
		
		deploymentTreeControllerImpl = new DeploymentTreeControllerImpl();
		
		gui = new CommandCenterGui(this);
		gui.prepare();

		// --- read the built-in tools
		try {
			URL url = findResource(ToolManager.getDefaultBuiltinToolsName(), "");
			loadBuiltinTools(url);

		} catch (Exception exc) {
			log.severe("*** FATAL: Could not read definition of built-in tools."
					+ " Printing stacktrace to stderr and exiting. ***");
			exc.printStackTrace(System.err);
			exit(4);
		}


		// --- read the extra tools
		try {
			URL url = findResource(ToolManager.getDefaultExtraToolsName(), "");
			installExtraTools(url);

		} catch (Exception exc) {
			log.info("Failed to read " + ToolManager.getDefaultExtraToolsName() + "; reason was: " + exc);

		}
		
		
		// --- read the helpset

		try {
			ClassLoader cl = null;
			URL url = findResource(HELPSET_NAME, "");
			this.helpSet = new HelpSet(cl, url);
			
		} catch (HelpSetException ex) {
			log.info("couldn't read helpset, no help available");
		}
		
		
	}

	public void go () {
		boolean admincMode = (startupOptions.manager != null);
		gui.go (admincMode);
		
		if (admincMode) {
			gui.deployTree.shieldedAddManager(startupOptions.manager);
		}		
	}

	public void stop () {

		Executor.remoteDownAll();

		if (deploymentTreeControllerImpl != null)
			deploymentTreeControllerImpl.stop();

		if (firestarter != null)
			firestarter.shutdown();

		gui.stop();
		
		// we stop these rather late because the
		// thread that executes this very method
		// may itself be a background thread.
		bgThreads.shutdownNow();

		exit(0);
	}

	/**
	 * System.exit() can be prevented by setting the boolean flag to false through
	 * the corresponding command line switch.
	 */
	public void exit(final int code) {
		log.fine("requested to exit with exit code '" + code + "'");
      
		if (startupOptions.doExitOnClose) {

			// trying to tear down a hanging in-process Acs
         // can freeze the whole application.
         // thus, we use a watchdog to ensure termination.
         new Thread(){@Override
			public void run(){
            try {
               Thread.sleep(8*1000);
            } catch (InterruptedException exc) {}

            System.out.println("VM still up, shooting it now.");
            try {
               Thread.sleep(1*1000);
            } catch (InterruptedException exc) {}
            
            Runtime.getRuntime().halt(code);
               
         }}.start();

         System.exit(code);
         
		} else {
			log.fine("I am configured with the no-exit option, will not shut down the VM");
      }
	}

	//
	// ============ Versioning ====================
	//

	/** assigned in prepare() */
	protected String projectCreatorId;
	
	public String projectCreatorId() {
		return projectCreatorId;
	}
	
	/** assigned in version() */
	protected String version = null;

	/** 
	 * Returns the first non-empty line of file "src/VERSION",
	 * or the empty string in any erroneous case.
	 * @return a valid version info or ""
	 */
	public String version() {
		if (version == null) {
			String ret = null;
			try {
				URL u = this.getClass().getClassLoader().getResource("VERSION");
				BufferedReader r = new BufferedReader(new InputStreamReader(u.openStream()));
				while ((ret = r.readLine()) != null) {
					ret = ret.trim();
					if (! "".equals(ret)) {
						break;
					}
				}
			} catch (Exception e) {}

			ret = (ret == null)? "" : ret;
			this.version = ret;
		}

		return version;
	}

	
	//
	// ============ Resource Loading ====================
	//

	
	/**
	 * Finds a resource in the resource folder.
	 */
	public URL findResource (String name) {
		return findResource(name, "alma/acs/commandcenter/resources/");
	}

	/**
	 * Finds a resource in the specified location.
	 */
	public URL findResource (String name, String where) {
		URL ret = this.getClass().getClassLoader().getResource(where + name);
		if (ret == null) {
			log.fine("couldn't find resource '" + name + "' at " + where);
		} else {
			log.finest("found resource " + ret);
		}
		return ret;
	}
	
	

	
	//
	// ============ HelpSet ====================
	//


	protected final String HELPSET_NAME = "AcsCommandCenter.hs";
	
	protected HelpSet helpSet;	
	
	public HelpSet getHelpSet() {
		return this.helpSet;
	}
	
	


	//
	// ============ Tool Loading / Installing ====================
	//
	
	public URL latestBuiltinToolsUrl;

	public URL currentExtraToolsUrl;

	/**
	 * API method
	 */
	public void installExtraTools (URL url) throws Exception {
		InputStream s = url.openStream();
		installExtraTools(s);
		// we store the Url to be able to show its
		// content in the "view current config" action
		currentExtraToolsUrl = url;
	}

	/**
	 * internal method
	 */
	protected void installExtraTools (InputStream f) throws Exception {

		// --- make delegate read in the xml file
		ToolManager.readExtraTools(new InputStreamReader(f));
		AcsCommandCenterTools tools = ToolManager.getExtraTools();

		// --- loop over all "tool" definitions
		for (int i = 0; i < tools.getToolCount(); i++) {
			Tool tool = tools.getTool(i);

			// this map will be used to transport values from ToolInputPanel to
			// ToolStarter
			HashMap<String,Object> input = new HashMap<String,Object>();

			// ToolStarter knows how to start a tool with the given input
			final ToolStarter ts = executeTools.addTool(tool, input);

			// The gui will show the ToolInputPanel to the user and afterwards
			// notify the ActionListener
			gui.addExtraTool(tool, input, ts);
		}

	}

	public void removeExtraTools () {
		gui.removeAllExtraTools();
	}

	/**
	 * API method
	 */
	public void loadBuiltinTools (URL url) throws Exception {
		InputStream is = url.openStream();
		loadBuiltinTools(is);
		// we store the Url to be able to show its
		// content in the "view current config" action
		latestBuiltinToolsUrl = url;
	}

	/**
	 * internal method
	 */
	protected void loadBuiltinTools (InputStream f) throws Exception {
		InputStreamReader r = new InputStreamReader(f);
		// this call will replace 0 to n builtin-tool definitions
		ToolManager.readBuiltinTools(r);
	}



	
	//
	// ============ Project Management ====================
	//

	
	public AcsCommandCenterProject createProject () {
		return projectMaker.createProject();
	}
	
	public void loadProject (File f) {
		try {
			AcsCommandCenterProject p = projectMaker.loadProject(f);
			
			project = p;
			model.setProject(p);
			gui.setCurrentProjectFile(f);
			gui.currentProjectChanged();

		} catch (Exception exc) {
			log.info("could not load project file '" + f + "' due to " + exc);
		}
	}
	
	public AcsCommandCenterProject readProject (File f) throws FileNotFoundException, MarshalException, ValidationException, IOException {
		return projectMaker.readProject(f);
	}
	
	public void writeProject (AcsCommandCenterProject p, File f) throws IOException, MarshalException, ValidationException {
		projectMaker.writeProject(p, f);
	}

	public void moreContainers () {
		this.project.getContainers().addContainer(projectMaker.createContainer());
	}

	public void lessContainers () {
		if (this.project.getContainers().getContainerCount() == 0)
			return;
		int indexOfLast = this.project.getContainers().getContainerCount() - 1;
		ContainerT last = this.project.getContainers().getContainer(indexOfLast);
		this.project.getContainers().removeContainer(last);
	}

	/** 
	 * Removes a container from the project.
	 * @return the removed container, or null if index invalid 
	 */
	public ContainerT removeContainer (int index) {
		if (index < 0 || index >= this.project.getContainers().getContainerCount()) {
			return null;
		}
		ContainerT cont = this.project.getContainers().getContainer(index);
		this.project.getContainers().removeContainer(cont);
		return cont;
	}
	
	/**
	 * Inserts a container into the project at the given index, that is,
	 * the container will afterwards have the index <code>index</code>.
	 */
	public void insertContainer(ContainerT cont, int index) {
		this.project.getContainers().addContainer(index, cont);
	}

	
	//
	// ============ Variable Management ====================
	//

	public Map<String, Object>[] giveVariableMapsForGui() {
		Map<String, Object>[] ret = new Map[3];
		
		Map<String, Object> m0 = model.getVariables();
		ret[0] = m0;

		List<String> projectVariableNames = giveProjectVariableNames();
		
		// in project (with values from session if set, otherwise null)
		Map<String, Object> m1 = new HashMap<String, Object>();
		for (Iterator<String> it = projectVariableNames.iterator() ; it.hasNext() ;) {
			String key = it.next();
			m1.put(key, m0.get(key));
		}
		ret[1] = m1;

		// in session only
		Map<String, Object> m2 = new HashMap<String, Object>(m0);
		m2.keySet().removeAll(projectVariableNames);
		ret[2] = m2;
		
		return ret;
	}

	
	public void handleUnresolvableVariable(String name) {
		variablesDiscoveredOnTheFly.add(name);
		gui.showUnresolvableVariableErrorDialog(name);
	}
	
	
	private List<String> variablesDiscoveredOnTheFly = new LinkedList<String>();

	/**
	 * For later use by whoever, could currently as well be "protected"
	 */
	public List<String> giveProjectVariableNames() {
		List<String> ret = new LinkedList<String>();
		scanForVariables(project, ret);
		ret.addAll(variablesDiscoveredOnTheFly);
		return ret;
	}

	
	// --- Scanning through project for variables ---
	
	private void scanForVariables(Object[] xx, List<String> l) {
		for (int i = 0; i < xx.length; i++) {
			scanForVariables(xx[i], l);
		}
	}
	private void scanForVariables(Object x, List<String> l) {
		Method[] mm = x.getClass().getDeclaredMethods();
		for (int i = 0; i < mm.length; i++) {
			Method m = mm[i];
			if (m.getName().startsWith("get") && ((m.getModifiers() & Modifier.PUBLIC) != 0) &&
					(m.getParameterTypes().length == 0)) {
				Object rv = null;
				try {
					rv = m.invoke(x, new Object[]{});
				} catch (Exception exc) {}
				if (rv == null) {
					continue;
				}
				if (rv instanceof String) {
					scanForVariables((String)rv, 0, l);
				}
				else 
				if (rv instanceof Object[]) {
					scanForVariables((Object[])rv, l);
				}
				if (rv instanceof Object) {
					scanForVariables((Object)rv, l);
				}
			}
		} 		
	}
	private void scanForVariables(String s, int i, List<String> l) {
		int markerStart = s.indexOf("${", i);
		
		if (markerStart != -1) {
			int markerEnd = s.indexOf("}", markerStart);
			if (markerEnd != -1) {
				String embeddedVarName = s.substring(markerStart + 2, markerEnd);
				l.add(embeddedVarName);
				// continue with next variable
				scanForVariables(s, markerEnd+1, l);
			}
		}
	}

	//
	//  ======= Options from Command Line etc. ==========
	//
	
	/**
	 * A struct to pass command line options from the CLI parser to the application.
	 * Clients of this class must check for <code>null</code> before using its contents.
	 */
	public static class StartupOptions {
		public Rectangle geometry;
		public File project;
		public String manager;
		public boolean doExitOnClose = true;
	}	

	
	
	//
	//  ======= RunModel implementation ==========
	//

	/**
	 * Extension of ProjectRunModel:
	 * 
	 * We do not make use of getManagerLocalJavaAgainstCDBHost() (and the gui will offer
	 * no way to change the value). Instead we always use "localhost".
	 * 
	 *  
	 */
	public class MyProjectRunModel extends ProjectRunModel {

		public MyProjectRunModel(AcsCommandCenterProject project) {
			super(project);
		}

		
		@Override
		public String getManagerLocalJavaAgainstCDBHost () {
			return ACSPorts.getIP();
		}

	}


	// Background Actions
	// ============================================================


	/** Factory for unlimited number of daemons threads */
	private ExecutorService bgThreads = Executors.newCachedThreadPool(new ThreadFactory(){
		ThreadFactory def = Executors.defaultThreadFactory();
		public Thread newThread (Runnable r) {
			Thread ret = def.newThread(r);
			ret.setDaemon(true);
			return ret;
		}
	});


	public void runBackground (Runnable r) {
		bgThreads.execute(r);
	}

	

	// DeploymentTreeController implementation
	// ============================================================

	
	public DeploymentTreeControllerImpl deploymentTreeControllerImpl;
	
	public class DeploymentTreeControllerImpl implements DeploymentTreeController {

		// create MaciSupervisors
		// -----------------------------------------
		// (i) we cache previously created ones
		// (ii) newly created ones will not be start()-ed

		synchronized public GuiMaciSupervisor giveMaciSupervisor(String managerLoc) throws OrbInitException {
			GuiMaciSupervisor ret = managerLoc2instance.get(managerLoc);
			if (ret == null) {
				ORB orb = firestarter.giveOrb();
				ret = new GuiMaciSupervisor("AcsCommandCenter", managerLoc, orb, log);
				ret.setRefreshesPeriodically(true);
				managerLoc2instance.put(managerLoc, ret);
	      }
	      return ret;
		}
		
		private Map<String, GuiMaciSupervisor> managerLoc2instance = new WeakHashMap<String, GuiMaciSupervisor>();

		
		// background actions
		// -----------------------------------------
		
		public java.util.concurrent.Executor getBackgroundExecutor() {
			return bgThreads;
		}


		// lifecycle of this class
		// -----------------------------------------

		/** Call stop() on all MaciSupervisors */
	   synchronized public void stop() {
	   	for (IMaciSupervisor s : managerLoc2instance.values()) {	
	   		try {
					s.stop();
				} catch (Exception exc) {/* ignore */}
	   	}
	   }

	   
	}
	
}



