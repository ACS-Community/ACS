/*
 * Created on Oct 28, 2003 by mschilli
 */
package alma.acs.commandcenter.engine;

import alma.acs.commandcenter.util.MiscUtils;
import alma.acs.util.ACSPorts;
import alma.entity.xmlbinding.acscommandcentertools.Tool;

/**
 * @author mschilli
 */
public class ExecuteAcs {


	public RunModel runModel;

	public ExecuteAcs(RunModel runModel) {
		this.runModel = runModel;
	}

	//
	// ========================================================================================
	//

	/**
	 * @return an array {manager_host, manager_port}
	 */
	public String[] startLocalScript(NativeCommand.Listener listener) throws Throwable {

		Tool t = ToolManager.getBuiltinTool("Acs_startLocalScript");
		String command = ToolManager.generateCommand(t, runModel);
		Executor.localOutProc(command, true, t.getMaxStartupSeconds() * 1000, t.getExpectedOutput(), listener);

		// since we can't find out where the manager runs effectively
		// (by e.g. parsing its output), we make our best guess..
		String acsBasePort = runModel.getScriptBase();
		ACSPorts ports = ACSPorts.globalInstance(MiscUtils.parseInt(acsBasePort));
		return new String[]{ports.giveIP(), ports.giveManagerPort()};
	}

	//
	//

	public void stopLocalScript(NativeCommand.Listener listener) throws Throwable {

		Tool t = ToolManager.getBuiltinTool("Acs_stopLocalScript");
		String command = ToolManager.generateCommand(t, runModel);
		Executor.localOutProc(command, true, t.getMaxStartupSeconds() * 1000, t.getExpectedOutput(), listener);
	}

	//
	//

	public void killLocalScript(NativeCommand.Listener listener) throws Throwable {
		Tool t = ToolManager.getBuiltinTool("Acs_killLocalScript");
		String command = ToolManager.generateCommand(t, runModel);
		Executor.localOutProc(command, true, t.getMaxStartupSeconds() * 1000, t.getExpectedOutput(), listener);
	}

	//
	// ========================================================================================
	//

	/**
	 * @return an array {manager_host, manager_port}
	 */
	public String[] startRemote(NativeCommand.Listener listener) throws Throwable {

		String host = runModel.getRemoteHost();
		String username = runModel.getRemoteAccount();
		String password = runModel.getRemotePassword();

		Tool t = ToolManager.getBuiltinTool("Acs_startRemote");
		String command = ToolManager.generateCommand(t, runModel);
		Executor.remote(username, password, command, t.getExpectedOutput(), listener, host);

		// since we can't find out where the manager runs effectively
		// (by e.g. parsing its output), we make our best guess..
		String acsBasePort = runModel.getScriptBase();
		ACSPorts ports = ACSPorts.globalInstance(MiscUtils.parseInt(acsBasePort));
		return new String[]{host, ports.giveManagerPort()};
	}

	//
	//

	public void stopRemote(NativeCommand.Listener listener) throws Throwable {

		String host = runModel.getRemoteHost();
		String username = runModel.getRemoteAccount();
		String password = runModel.getRemotePassword();

		Tool t = ToolManager.getBuiltinTool("Acs_stopRemote");
		String command = ToolManager.generateCommand(t, runModel);
		Executor.remote(username, password, command, t.getExpectedOutput(), listener, host);
	}

	//
	//

	public void killRemote(NativeCommand.Listener listener) throws Throwable {

		String host = runModel.getRemoteHost();
		String username = runModel.getRemoteAccount();
		String password = runModel.getRemotePassword();

		Tool t = ToolManager.getBuiltinTool("Acs_killRemote");
		String command = ToolManager.generateCommand(t, runModel);
		Executor.remote(username, password, command, t.getExpectedOutput(), listener, host);
	}

	//
	// ========================================================================================
	//

	public void startRemoteDemonic (NativeCommand.Listener listener) {
		
		String host = runModel.getRemoteHost();
		int instance = MiscUtils.parseInt(runModel.getScriptBase());
		
		boolean startStop = true;
		String cmdFlags = "";

		Executor.remoteDaemonForServices(host, instance, startStop, cmdFlags, listener);
	}
	
	public void stopRemoteDemonic (NativeCommand.Listener listener) {
		
		String host = runModel.getRemoteHost();
		int instance = MiscUtils.parseInt(runModel.getScriptBase());
		
		boolean startStop = false;
		String cmdFlags = "";
		
		Executor.remoteDaemonForServices(host, instance, startStop, cmdFlags, listener);
	}

}


