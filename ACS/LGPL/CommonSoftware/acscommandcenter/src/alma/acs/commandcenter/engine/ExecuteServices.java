/*
 * Created on Oct 21, 2003 by mschilli
 */
package alma.acs.commandcenter.engine;

import java.util.StringTokenizer;

import alma.acs.commandcenter.util.PreparedString;
import alma.entity.xmlbinding.acscommandcentertools.Tool;

/**
 *
 * @author mschilli
 */
public class ExecuteServices {

   protected RunModel runModel;

   public ExecuteServices(RunModel runModel) {
      this.runModel = runModel;
   }

   //
   // ========================================================================================
   //

   protected com.cosylab.cdb.jdal.Server cdbServer;
   
   public String startLocalJavaPexpect = "ready and waiting ...";

   public PreparedString startLocalJavaArgs = new PreparedString("-n -jacorb -root ? -OAport ?");

   public void startLocalJava(NativeCommand.Listener listener) {

      // insert dynamic values into command line args
      String argLine = startLocalJavaArgs.toString(new String[]{runModel.getServicesLocalJavaRoot(), runModel.getServicesLocalJavaPort()});
      // split command line args
      StringTokenizer toky = new StringTokenizer(argLine);
      final String[] args = new String[toky.countTokens()];
      for (int i = 0; toky.hasMoreTokens(); i++)
         args[i] = toky.nextToken();

      DefaultProperties props = new DefaultProperties();

/*
 msc 2009-03: commented out all abeans-related stuff from Acs 8.0.1 on

      // msc(2005-07): we want the abeans framework to use the following tmp-dir 
      // (the abeans framework would create the dir if it doesn't exist, but we need
      // to do this ourselves since we're responsible for also removing the directory afterwards)
      String acsInstance = runModel.getScriptBase(); 
      props.setProperty("ACS.tmp", props.getProperty("ACS.data")+"/tmp/ACS_INSTANCE."+acsInstance);
*/
      /*
       (Apr 29, 2004) msc: 
       On Linux we have ORBInitRef.NameService= set by acsStartJava,
       it is the reason for "[ ERROR: Could not read from URL ]".
       On windows this property is not set
       */
      System.getProperties().remove("ORBInitRef.NameService");


      Executor.localInProc(props, startLocalJavaPexpect, listener, new Executor.RunMain() {
         public void runMain() throws Exception {
            cdbServer = new com.cosylab.cdb.jdal.Server();
            cdbServer.run(args);
         }
      });
   }

   public void stopLocalJava() {
      Executor.local(new Executor.RunMain() {
         public void runMain() {
            if (cdbServer != null) {
               cdbServer.shutdown();
            }
            //PENDING(msc): remove this explicit wait
            //the local java in proc is where we have no output available
            // (unless we redirect the output stream of the current vm)
            // thus we can't decide, whether the cdb has shut down.
            try {
               Thread.sleep(1*1000);
            } catch (InterruptedException e) {}
         }
      });
   }

   //
   // ========================================================================================
   //

   public void startLocalScript(NativeCommand.Listener listener) throws Throwable {

      Tool t = ToolManager.getBuiltinTool("Services_startLocalScript"); 
      String command = ToolManager.generateCommand(t, runModel);
      Executor.localOutProc(command, true, t.getMaxStartupSeconds() * 1000, t.getExpectedOutput(), listener);
   }

   public void stopLocalScript(NativeCommand.Listener listener) throws Throwable {
       
      Tool t = ToolManager.getBuiltinTool("Services_stopLocalScript");
      String command = ToolManager.generateCommand(t, runModel);
      Executor.localOutProc(command, true, t.getMaxStartupSeconds() * 1000, t.getExpectedOutput(), listener);
   }

   //
   // ========================================================================================
   //

   public void startRemote(boolean nativeSSH, NativeCommand.Listener listener) throws Throwable {

      String host = runModel.getRemoteHost();
      String username = runModel.getRemoteAccount();
      String password = runModel.getRemotePassword();

      Tool t = ToolManager.getBuiltinTool("Services_startRemote");
      String command = ToolManager.generateCommand(t, runModel);
      Executor.remote(nativeSSH, username, password, command, t.getExpectedOutput(), listener, host);
   }

   public void stopRemote(boolean nativeSSH, NativeCommand.Listener listener) throws Throwable {

      String host = runModel.getRemoteHost();
      String username = runModel.getRemoteAccount();
      String password = runModel.getRemotePassword();

      Tool t = ToolManager.getBuiltinTool("Services_stopRemote");
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
