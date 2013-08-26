/*
 * Created on Oct 28, 2003 by mschilli
 */
package alma.acs.commandcenter.engine;

import java.util.Properties;

import alma.acs.commandcenter.util.MiscUtils;
import alma.acs.container.AcsContainer;
import alma.acs.container.AcsEmbeddedContainerRunner;
import alma.acs.container.corba.AcsCorba;
import alma.acs.logging.AcsLogger;
import alma.acs.util.AcsLocations;
import alma.entity.xmlbinding.acscommandcentertools.Tool;

/**
 * @author mschilli
 */
public class ExecuteContainer {

	protected AcsLogger log;
	protected AcsLogger acsCorbaLogger;
	
    
    /**
     */
    public ExecuteContainer() {
        this.log = MiscUtils.getPackageLogger(this);
    }

    /**
     * @param acsCorbaLogger
     * @since ACS 5.0
     */
    public ExecuteContainer(AcsLogger acsCorbaLogger) {
       this.acsCorbaLogger = acsCorbaLogger;
   }


    //
    // ========================================================================================
    //

    public String startLocalJavaPexpect = "components activated.";
    public String stopLocalJavaPexpect = "logged out.";

    public void startLocalJava(RunModel runModel, NativeCommand.Listener listener) {
        // note: this variant doesn't care for the containerType, it's always java

		  final String contName = runModel.getContainerName();
		  final String managerHost = runModel.getContainerAgainstManagerHost();
		  final String managerPort = runModel.getContainerAgainstManagerPort();
		  final Integer orbport = Integer.valueOf(runModel.getContainerLocalJavaPort());
		  final String mgrLoc = AcsLocations.convertToManagerLocation(managerHost, managerPort);

        // set system properties
        Properties props = new DefaultProperties();
        props.setProperty("ACS.manager", mgrLoc);

        // run in same vm
        Executor.localInProc(props,
                startLocalJavaPexpect, listener, new Executor.RunMain() {

                    public void runMain() throws Throwable {
							  
							  if (acsCorba == null) {
								  
								  // determine acscorba logger
								  if (acsCorbaLogger == null) {
									  acsCorbaLogger = log;
								  }
								  
								  // create acscorba
								  acsCorba = new AcsCorba(acsCorbaLogger);
								  acsCorba.initCorba(new String[]{}, orbport.intValue());
								  acsCorba.runCorba();							  
							  }
							  
							  // run container
							  customAcsEmbeddedContainerRunner = new CustomAcsEmbeddedContainerRunner();
							  customAcsEmbeddedContainerRunner.run(acsCorba, contName, mgrLoc);
                    }
                });

    }

    private AcsCorba acsCorba;
    private CustomAcsEmbeddedContainerRunner customAcsEmbeddedContainerRunner;

    /** 
     * Needed to gain access to the container instance held by the embedded container
     * runner. By this, we can invoke shutdown() on the container instance without
     * the need to go through ACS.
     */
    private class CustomAcsEmbeddedContainerRunner extends AcsEmbeddedContainerRunner {
	    AcsContainer theContainer() {
	        return m_container;
	    }								  
    }
	 
    public void stopLocalJava(RunModel runModel) {

        NativeCommand.Listener listener = null;
        Properties props = new Properties();

        Executor.localInProc(props, stopLocalJavaPexpect, listener, new Executor.RunMain() {
           public void runMain() {
         	  if (customAcsEmbeddedContainerRunner != null) {
         		  AcsContainer cont = customAcsEmbeddedContainerRunner.theContainer();
         		  if (cont != null) {
         		      int action = 2 * 256 + 0; // hibyte: 2 (= EXIT), lobyte: 0 (exitcode towards the OS)
         			  cont.shutdown(action);
         		  }
         	  }
         	  /* msc(2005-07): no longer sure this is really needed
         	  if (localInProcThread != null) {
         	  	  localInProcThread.stop();
         	  }
         	  */
           }
       });

    }

    //
    // ========================================================================================
    //

    public void startLocalScript(RunModel runModel, NativeCommand.Listener listener) throws Throwable {

        Tool t = ToolManager.getBuiltinTool("Container_startLocalScript_"
                + runModel.getContainerType());
        String command = ToolManager.generateCommand(t, runModel);

        Executor.localOutProc(command, true, t.getMaxStartupSeconds() * 1000, t.getExpectedOutput(),
                listener);
    }

    public void stopLocalScript(RunModel runModel, NativeCommand.Listener listener) throws Throwable {

        Tool t = ToolManager.getBuiltinTool("Container_stopLocalScript_"
                + runModel.getContainerType());
        String command = ToolManager.generateCommand(t, runModel);

        Executor.localOutProc(command, true, t.getMaxStartupSeconds() * 1000, t.getExpectedOutput(),
                listener);
    }

    //
    // ========================================================================================
    //

    public void startRemote(RunModel runModel, boolean nativeSSH, NativeCommand.Listener listener) throws Throwable {

        String host = runModel.getContainerRemoteHost();
        String username = runModel.getContainerRemoteAccount();
        String password = runModel.getContainerRemotePassword();

        String toolname = "Container_startRemote_"
                + runModel.getContainerType();

        
        // c++ container: special case
        if (runModel.getContainerType().equals("cpp")
                && !runModel.getContainerAgainstInterfaceRepository()
                        .equals("")) {
            toolname = "Container_startRemote_cpp_intrep";
        }

        
        Tool t = ToolManager.getBuiltinTool(toolname);
        String command = ToolManager.generateCommand(t, runModel);

        log.finer("generated remote command: " + command);

        Executor.remote(nativeSSH, username, password, command, t.getExpectedOutput(),
                listener, host);
    }

    public void stopRemote(RunModel runModel, boolean nativeSSH, NativeCommand.Listener listener) throws Throwable {

        String host = runModel.getContainerRemoteHost();
        String username = runModel.getContainerRemoteAccount();
        String password = runModel.getContainerRemotePassword();

        Tool t = ToolManager.getBuiltinTool("Container_stopRemote_"
                + runModel.getContainerType());
        String command = ToolManager.generateCommand(t, runModel);

        Executor.remote(nativeSSH, username, password, command, t.getExpectedOutput(),
                listener, host);
    }

    
    //
    // ========================================================================================
    //
    
 	/**
 	 * @return any exception while starting the Container, or {@code null} if all went well.
 	 */
    public Exception startRemoteDemonic (RunModel runModel, NativeCommand.Listener listener) {

   	 String   contHost = runModel.getContainerRemoteHost();
   	 String   contName = runModel.getContainerName();
   	 String   contType = runModel.getContainerType();
   	 String[] contMods = runModel.getContainerTypeModifiers();
       int instance = MiscUtils.parseInt(runModel.getContainerScriptBase());

       String mgrHost = runModel.getContainerAgainstManagerHost();
       String mgrPort = runModel.getContainerAgainstManagerPort();
       String mgrLoc  = AcsLocations.convertToManagerLocation(mgrHost, mgrPort);
       String cmdFlags = "-m "+mgrLoc;

       String contHeap = runModel.getContainerHeapSize();
       if (contHeap != null)
      	 cmdFlags += " --passthroughProcessStart=\"-maxHeapSize="+contHeap+"m\"";

       boolean startStop = true;

		// (2008-08-05): we keep this for backward-compatibility (see COMP-1316 and COMP-1996)
		if ("archive".equals(contType)) {
			contType = "java";
			contMods = new String[]{"archiveContainer"};
		}

		return Executor.remoteDaemonForContainers(contHost, instance, startStop, contName, contType, contMods, cmdFlags, listener);
	}

	public void stopRemoteDemonic (RunModel runModel, NativeCommand.Listener listener) {

   	 String   contHost = runModel.getContainerRemoteHost();
   	 String   contName = runModel.getContainerName();
   	 String   contType = runModel.getContainerType();
   	 String[] contMods = runModel.getContainerTypeModifiers();
       int instance = MiscUtils.parseInt(runModel.getContainerScriptBase());
		
       String mgrHost = runModel.getContainerAgainstManagerHost();
       String mgrPort = runModel.getContainerAgainstManagerPort();
       String mgrLoc  = AcsLocations.convertToManagerLocation(mgrHost, mgrPort);
       String cmdFlags = "-m "+mgrLoc;
		 
       boolean startStop = false;

		 Executor.remoteDaemonForContainers(contHost, instance, startStop, contName, contType, contMods, cmdFlags, listener);
	}


}

