/*
 * Created on Oct 5, 2004 by mschilli
 */
package alma.acs.commandcenter.app;

import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;

import alma.acs.commandcenter.engine.RunModel;
import alma.acs.commandcenter.util.MiscUtils;
import alma.acs.commandcenter.util.VariableString;
import alma.acs.util.ACSPorts;
import alma.acs.util.AcsLocations;
import alma.entity.xmlbinding.acscommandcenterproject.AcsCommandCenterProject;
import alma.entity.xmlbinding.acscommandcenterproject.types.ModeType;
import alma.entity.xmlbinding.acscommandcenterproject.ContainerT;



/**
 * Makes an AcsCommandCenterProject available as a RunModel.
 * 
 * <p>
 * Some methods may depending on a boolean flag return a runtime value stored in the
 * application (e.g. most recently started manager, or cdb thats most likely to belong to
 * the most recently started manager etc.) or return a value from the  project. </p>
 * 
 * @author mschilli
 */
public class ProjectRunModel implements RunModel {

	
	////////////////////////////////////////////////////////
	/// ---------- Logic of ProjectRunModel ------------ ///
	////////////////////////////////////////////////////////


	// --- construction ---
	
	public ProjectRunModel(AcsCommandCenterProject project) {
		setProject(project);
	}

	
	// --- project ---
	
	protected AcsCommandCenterProject project;

	public void setProject (AcsCommandCenterProject project) {
		this.project = project;
	}

	public AcsCommandCenterProject getProject () {
		return project;
	}
	
	public ModeType getMode() {
		return project.getMode();
	}
	
	// --- thread safety for concurrent starts of containers ---
	
	protected int selectedContainerIndex = 0; //default
	
	public ProjectRunModel createViewOnContainer (int containerIndex) {
		ProjectRunModel ret = new ProjectRunModel(project);
		ret.selectedContainerIndex = containerIndex;
		return ret;
	}
	
	
	// --- internal helpers (also API for Gui) ---
	
	public String deriveIRfromCommonSettings() {
		int basePort = deriveInstanceFromCommonSettings();
		String port = ACSPorts.globalInstance(basePort).giveIRPort();
		String host = deriveMgrHostfromCommonSettings();
		String ret = AcsLocations.convertToInterfaceRepositoryLocation(host, port);
		return ret;
	}

	public String deriveNSfromCommonSettings() {
		int basePort = deriveInstanceFromCommonSettings();
		String port = ACSPorts.globalInstance(basePort).giveNamingServicePort();
		String host = deriveMgrHostfromCommonSettings();
		String ret = AcsLocations.convertToNameServiceLocation(host, port);
		return ret;
	}
	
	public String deriveCDBfromCommonSettings() {
		int basePort = deriveInstanceFromCommonSettings();
		String port = ACSPorts.globalInstance(basePort).giveCDBPort();
		String host = deriveMgrHostfromCommonSettings();
		String ret = AcsLocations.convertToCdbLocation(host, port);
		return ret;
	}
		
	public String deriveMgrHostfromCommonSettings() {
		String host;
		if (ModeType.REMOTE.equals(project.getMode())
				|| ModeType.REMOTE_NATIVE.equals(project.getMode())	
				|| ModeType.REMOTE_DAEMON.equals(project.getMode())) {
			host = read(project.getRemoteHost());
		} else {
			host = ACSPorts.getIP();
		}
		return host;
	}

	public String deriveMgrPortfromCommonSettings() {
		int basePort = deriveInstanceFromCommonSettings();
		String port = ACSPorts.globalInstance(basePort).giveManagerPort();
		return port;
	}

	
	// --- private helpers ---

	protected int deriveInstanceFromCommonSettings() {
		return MiscUtils.parseInt(read(project.getScriptBase()));
	}
	
	protected int selectedContainerIndex() {
		return this.selectedContainerIndex; 
	}
	
	protected ContainerT selectedContainer() {
		int selectedContainerIndex = selectedContainerIndex();
		return project.getContainers().getContainer(selectedContainerIndex);
	}

	
	// --- support for variables ---
	
	protected String read (String stringWithVars) {
		return new VariableString(stringWithVars, false).toString(getVariables());
	}
	
	protected Map<String, Object> variables = null;

	public Map<String, Object> getVariables() {
		if (variables == null) {
			readDefaultVariables();
		}
		return variables;
	}

	/**
	 * The default allocation of the variables map is (an excerpt of) the System Properties. 
	 */
	@SuppressWarnings("unchecked") // JDK API not parameterized
	public void readDefaultVariables() {
		
		Map<String, Object> m = Collections.synchronizedMap(new HashMap<String, Object>());
		
		for (Iterator it = System.getProperties().entrySet().iterator(); it.hasNext() ;) {
			Map.Entry e = (Map.Entry) it.next();
			String n = (String)e.getKey(); 
			if (n.startsWith("java.") || n.startsWith("sun.") || n.startsWith("os.") 
					|| n.startsWith("file.") || n.startsWith("awt.") || n.startsWith("org.omg.")
					|| n.startsWith("jacorb.") || n.startsWith("path.") || n.startsWith("line.")
					|| n.startsWith("user.country") || n.startsWith("user.language") || n.startsWith("user.timezone")
					|| n.startsWith("user.variant") || n.startsWith("deployment.") || n.startsWith("javaplugin.")
					|| n.startsWith("jnlpx.") || n.startsWith("org.apache.")) {
				// skip
			} else {
				m.put(n, e.getValue());
			}
		}
		variables = m;
	}	
	
   /**
    * Use given properties table for variable resolution
    */
	@SuppressWarnings("unchecked")
	public void readVariables (Properties p) {
		 Map m = Collections.synchronizedMap(new HashMap(p));
		 variables = m;
	}
	
	////////////////////////////////////////////////////////
	/// ---- Implementation of interface RunModel  ----- ///
	////////////////////////////////////////////////////////

	
	
	// ====== LOCAL JAVA SERVICES ========

	public String getServicesLocalJavaRoot () {
		return read(project.getServicesLocalJavaRoot());
	}

	public String getServicesLocalJavaPort () {
		ACSPorts ports = ACSPorts.globalInstance(deriveInstanceFromCommonSettings());
		return ports.giveCDBPort();
	}

	// ====== LOCAL JAVA MANAGER ========

	public String getManagerLocalJavaPort () {
		ACSPorts ports = ACSPorts.globalInstance(deriveInstanceFromCommonSettings());
		return ports.giveManagerPort();
	}

	public String getManagerLocalJavaAgainstCDBHost () {
		return read(project.getManagerLocalJavaAgainstCDBHost());
	}

	public String getManagerLocalJavaAgainstCDBPort () {
		return this.getServicesLocalJavaPort();
	}

	// ====== LOCAL JAVA CONTAINER ========

	public String getContainerLocalJavaPort () {
		//TODO(msc): have this container port calculated by class ACSPorts
		int selectedContainerIndex = selectedContainerIndex();
		int scriptbase = deriveInstanceFromCommonSettings();
		return String.valueOf(3000 + (scriptbase * 100) + 50 + (selectedContainerIndex * 2));
	}

	// ====== MANAGER + SERVICES ========

	public String getScriptBase () {
		return read(project.getScriptBase());
	}

	public String getRemoteHost () {
		return read(project.getRemoteHost());
	}

	public String getRemoteAccount () {
		return read(project.getRemoteAccount());
	}

	public String getRemotePassword () {
		return read(project.getRemotePassword());
	}

	// ====== TOOLS ========

	public String getToolAgainstManagerHost () {
		String ret = (project.getToolRunAgainstDedicatedSettings()) ? read(project.getToolAgainstManagerHost()) : deriveMgrHostfromCommonSettings();
		return ret;
	}

	public String getToolAgainstManagerPort () {
		String ret = (project.getToolRunAgainstDedicatedSettings()) ? read(project.getToolAgainstManagerPort()) : deriveMgrPortfromCommonSettings();
		return ret;
	}

	public String getToolAgainstInterfaceRepository () {
		String ret = (project.getToolRunAgainstDedicatedSettings()) ? read(project.getToolAgainstInterfaceRepository()) : deriveIRfromCommonSettings();
		return ret;
	}

	public String getToolAgainstNameService () {
		String ret = (project.getToolRunAgainstDedicatedSettings()) ? read(project.getToolAgainstNameService()) : deriveNSfromCommonSettings();
		return ret;
	}

	// ====== CONTAINER ========

	public String getContainerName () {
		return read(selectedContainer().getName());
	}

	public String getContainerType () {
		return read(selectedContainer().getType());
	}

	public String[] getContainerTypeModifiers () {
		String[] ss = selectedContainer().getTypeModifier();
		for (int i=0; i<ss.length; i++)
			ss[i] = read(ss[i]);
		return ss;
	}

   public String getContainerHeapSize() {  // new in Acs 8.1
   	return read (selectedContainer().getHeapSizeMB());
   }

	public String getContainerScriptBase () {
		if (selectedContainer().getUseDedicatedSettings())
			return read(selectedContainer().getScriptBase());
			
		return read(project.getScriptBase());
	}

	public String getContainerRemoteHost () {
		if (selectedContainer().getUseDedicatedSettings())
			return read(selectedContainer().getRemoteHost());
		
		return read(project.getRemoteHost());
	}

	public String getContainerRemoteAccount () {
		if (selectedContainer().getUseDedicatedSettings())
			return read(selectedContainer().getRemoteAccount());
		
		return read(project.getRemoteAccount());
	}

	public String getContainerRemotePassword () {
		if (selectedContainer().getUseDedicatedSettings())
			return read(selectedContainer().getRemotePassword());
		
		return read(project.getRemotePassword());
	}

	public String getContainerAgainstManagerHost () {
		String ret = (project.getContainers().getRunAgainstDedicatedSettings()) ? read(project.getContainers().getAgainstManagerHost()) : deriveMgrHostfromCommonSettings();
		return ret;
	}

	public String getContainerAgainstManagerPort () {
		String ret = (project.getContainers().getRunAgainstDedicatedSettings()) ? read(project.getContainers().getAgainstManagerPort()) : deriveMgrPortfromCommonSettings();
		return ret;
	}

	public String getContainerAgainstCDB () {
		String ret = (project.getContainers().getRunAgainstDedicatedSettings()) ? read(project.getContainers().getAgainstCDB()) : deriveCDBfromCommonSettings();
		return (ret == null) ? "" : ret;
	}

	public String getContainerAgainstInterfaceRepository () {
		String ret = (project.getContainers().getRunAgainstDedicatedSettings()) ? read(project.getContainers().getAgainstInterfaceRepository()) : deriveIRfromCommonSettings();
		return (ret == null) ? "" : ret;
	}




}

