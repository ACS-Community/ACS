/*
 * Created on Jan 5, 2004 by mschilli
 */
package alma.acs.startup;

import alma.acs.commandcenter.engine.ExecuteContainer;
import alma.acs.commandcenter.engine.ExecuteManager;
import alma.acs.commandcenter.engine.ExecuteServices;
import alma.acs.commandcenter.engine.RunModelAdapter;
import alma.acs.commandcenter.engine.NativeCommand;
import alma.acs.util.ACSPorts;

/**
 * Uses the AcsCommandCenter as a library to run ACS within a single VM.
 * 
 * @author mschilli
 */
public class AcsStartLight extends RunModelAdapter implements NativeCommand.Listener {

	public static void main (String[] args) {
		new AcsStartLight().go(args);
	}


	protected String acsInstance;
	protected String mgrPort;
	protected String cdbPort;
	protected String cntPort;
	protected String cntName;
	protected String cdbRoot;

	protected ExecuteServices executeServices;
	protected ExecuteManager executeManager;
	protected ExecuteContainer executeContainer;

	
	protected void go (String[] args) {
		long soLong = System.currentTimeMillis();

		config(args);

		System.err.println("\n---> now starting cdb\n");
		cdb();
		
		System.err.println("\n---> now starting manager\n");
		manager();
		
		System.err.println("\n---> now starting container\n");
		container();

		System.err.println("\n---> finished (after " + (System.currentTimeMillis() - soLong) + "ms)\n");
	}


	protected void config (String[] args) {
		if (args.length != 3) {
			System.err.println("Usage: (this) <AcsInstance> <CdbRoot> <ContainerName>");
			return;
		}

		
		try {
			acsInstance = String.valueOf(Integer.parseInt(args[0].trim()));
		} catch (NumberFormatException exc) {
			System.err.println(" AcsInstance argument invalid, using 0");
			acsInstance = "0";	
		}

		mgrPort = "3" + acsInstance + "00";
		cdbPort = "3" + acsInstance + "12";
		cntPort = "3" + acsInstance + "51";
		cdbRoot = args[1].trim();
		cntName = args[2].trim();
	}

	
	protected void cdb () {
		executeServices = new ExecuteServices(this);
		executeServices.startLocalJava(this);
	}

	protected void manager () {
		executeManager = new ExecuteManager(this);
		executeManager.startLocalJava(this);
	}

	protected void container () {
		executeContainer = new ExecuteContainer();
		executeContainer.startLocalJava(this, this);
	}


	// --- RunModel interface implemenation ---

	@Override
	public String getScriptBase() {
		return acsInstance;
	}
	
	@Override
	public String getServicesLocalJavaPort () {
		return cdbPort;
	}

	@Override
	public String getServicesLocalJavaRoot () {
		return cdbRoot;
	}

	@Override
	public String getManagerLocalJavaPort () {
		return mgrPort;
	}

	@Override
	public String getContainerLocalJavaPort () {
		return cntPort;
	}

	@Override
	public String getManagerLocalJavaAgainstCDBHost () {
		return ACSPorts.getIP();
	}

	@Override
	public String getManagerLocalJavaAgainstCDBPort () {
		return cdbPort;
	}

	@Override
	public String getContainerAgainstManagerHost () {
		return ACSPorts.getIP();
	}

	@Override
	public String getContainerAgainstManagerPort () {
		return mgrPort;
	}

	@Override
	public String getContainerName () {
		return cntName;
	}

	@Override
	public String getContainerType () {
		return "java";
	}

	// --- TaskListener interface implemenation ---

	public void stdoutWritten (NativeCommand task, String additionalOutput) {
		System.err.print(additionalOutput);
	}

	public void stderrWritten (NativeCommand task, String additionalOutput) {}

	public void statusChanged (NativeCommand task, String status) {}
}


