/*
 * Created on Oct 21, 2003 by mschilli
 */
package alma.acs.commandcenter.engine;

import java.util.Properties;

import alma.acs.commandcenter.util.MiscUtils;
import alma.acs.util.ACSPorts;
import alma.acs.util.AcsLocations;
import alma.entity.xmlbinding.acscommandcentertools.Tool;

/**
 * @author mschilli
 */
public class ExecuteManager {

    protected RunModel runModel;

    public ExecuteManager(RunModel runModel) {
        this.runModel = runModel;
    }

    String[] localJavaManagerLocation;

    //
    // ========================================================================================
    //

    public String startLocalJavaPexpect = "Manager Application initialized";
    public String stopLocalJavaPexpect = "Destroyed."; //"remaining timer tasks";

    /**
     * @return an array {manager_host, manager_port}
     */
    public String[] startLocalJava(NativeCommand.Listener listener) {

        /* 
         * Notes on how to set the manager's port:
         * 1) command line parameter does not exist
         * 2) property "OAPort" (with capital 'P', not "OAport") must be set
         */ 

        // --- set Properties

        Properties props = new DefaultProperties();

        // cdb
        String cdbHost = runModel.getManagerLocalJavaAgainstCDBHost();
        String cdbPort = runModel.getManagerLocalJavaAgainstCDBPort();
        props.setProperty("DAL.defaultReference", AcsLocations.convertToCdbLocation(cdbHost, cdbPort));

        // oa-port needed for the manager itself so it knows where it should run
        String mgrPort = runModel.getManagerLocalJavaPort();
        props.setProperty("OAPort", mgrPort);
        
        // msc(2004-04): prevents jManager from calling System.exit()
        props.setProperty("ACS.noExit", "true");

/*
 msc 2009-03: commented out all abeans-related stuff from Acs 8.0.1 on

        // manager-ref needed for "abeans" acs client (starts together with the manager)
        String mgrHost = ACSPorts.getIP();
        props.setProperty("ACS.manager", AcsLocations.convertToManagerLocation(mgrHost, mgrPort));

        // msc(2005-07): we need a reference to the manager object so we can invoke
        // its shutdown() method without going through ACS. Therefore, we can not simply
        // invoke com.cosylab.acs.maci.manager.app.Manager.main(). We duplicate here what
        // the manager's main() method would do
        props.setProperty("Manager.recovery", "false");
        props.setProperty(abeans.framework.FrameworkLayer.PROPERTY_DISABLE_SHUTDOWN_HOOK, "true");
*/

        // msc(2005-07): we want the abeans framework to use the following tmp-dir 
        // (the abeans framework would create the dir if it doesn't exist, but we need
        // to do this ourselves since we're responsible for also removing the directory afterwards)
        String acsInstance = runModel.getScriptBase(); 
        props.setProperty("ACS.tmp", props.getProperty("ACS.data")+"/tmp/ACS_INSTANCE."+acsInstance);
        
        // run in same vm
        Executor.localInProc(props,
                startLocalJavaPexpect, listener, new Executor.RunMain() {

                    public void runMain() {
                        localManager = new com.cosylab.acs.maci.manager.app.Manager();
                    }
                });

        this.localJavaManagerLocation = new String[] { ACSPorts.getIP(), mgrPort};
        return localJavaManagerLocation;
    }

    
    /** our manager object */    
    private com.cosylab.acs.maci.manager.app.Manager localManager = null;
    
    
    public void stopLocalJava() {

        NativeCommand.Listener listener = null;
        Properties props = new Properties();

        Executor.localInProc(props, stopLocalJavaPexpect,
          listener, new Executor.RunMain() {

              public void runMain() {
            	  if (localManager != null) {
            		  boolean sigInt = false;
            		  localManager.shutdown(sigInt);
            	  }
            	  
              }
          });

    }

    //
    // ========================================================================================
    //

    /**
     * @return an array {manager_host, manager_port}
     */
    public String[] startLocalScript(NativeCommand.Listener listener) throws Throwable {

        Tool t = ToolManager.getBuiltinTool("Manager_startLocalScript");
        String command = ToolManager.generateCommand(t, runModel);
        Executor.localOutProc(command, true, t.getMaxStartupSeconds() * 1000, t.getExpectedOutput(),
                listener);

        // since we can't find out where the manager runs effectively
        // (by e.g. parsing its output), we make our best guess..
        String acsBasePort = runModel.getScriptBase();
        ACSPorts ports = ACSPorts.globalInstance(MiscUtils
                .parseInt(acsBasePort));
        return new String[] { ports.giveIP(), ports.giveManagerPort()};
    }

    public void stopLocalScript(NativeCommand.Listener listener) throws Throwable {

        Tool t = ToolManager.getBuiltinTool("Manager_stopLocalScript");
        String command = ToolManager.generateCommand(t, runModel);
        Executor.localOutProc(command, true, t.getMaxStartupSeconds() * 1000, t.getExpectedOutput(),
                listener);
    }

    //
    // ========================================================================================
    //

    public boolean startRemote(boolean nativeSSH, NativeCommand.Listener listener) throws Throwable {

        String host = runModel.getRemoteHost();
        String username = runModel.getRemoteAccount();
        String password = runModel.getRemotePassword();

        Tool t = ToolManager.getBuiltinTool("Manager_startRemote");
        String command = ToolManager.generateCommand(t, runModel);
        return Executor.remote(nativeSSH, username, password, command, t.getExpectedOutput(), listener, host);
    }

    public void stopRemote(boolean nativeSSH, NativeCommand.Listener listener) throws Throwable {

        String host = runModel.getRemoteHost();
        String username = runModel.getRemoteAccount();
        String password = runModel.getRemotePassword();

        Tool t = ToolManager.getBuiltinTool("Manager_stopRemote");
        String command = ToolManager.generateCommand(t, runModel);
        Executor.remote(nativeSSH, username, password, command, t.getExpectedOutput(), listener, host);
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

