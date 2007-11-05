/*
 * Created on Oct 21, 2003 by mschilli
 */
package alma.acs.commandcenter.engine;

import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.net.SocketException;
import java.util.Enumeration;
import java.util.Properties;
import java.util.Vector;
import java.util.logging.Logger;

import alma.ACSErrTypeCommon.BadParameterEx;
import alma.acs.commandcenter.meta.Firestarter;
import alma.acs.commandcenter.meta.Firestarter.OrbInitException;
import alma.acs.commandcenter.trace.Flow;
import alma.acs.commandcenter.util.MiscUtils;
import alma.acs.commandcenter.util.PreparedString;
import alma.acs.commandcenter.util.StringRingBuffer;
import alma.acs.util.ACSPorts;
import alma.acs.util.AcsLocations;
import alma.acsdaemon.ContainerDaemon;
import alma.acsdaemon.ContainerDaemonHelper;
import alma.acsdaemon.ServicesDaemon;
import alma.acsdaemon.ServicesDaemonHelper;
import alma.acsdaemonErrType.FailedToStartAcsEx;
import alma.acsdaemonErrType.FailedToStartContainerEx;
import alma.acsdaemonErrType.FailedToStopAcsEx;
import alma.acsdaemonErrType.FailedToStopContainerEx;
import de.mud.ssh.SshWrapper;

/**
 * @author mschilli
 */
public class Executor {

   public static boolean disableRemote = false;
   
   public static final String SYSPROP_USE_NATIVE_SSH = "AcsCommandCenter.useNativeSSH";
   public static final String SYSPROP_KILL_NATIVE_SSH = "AcsCommandCenter.killNativeSSH";
   public static final String SYSPROP_COMMAND_NATIVE_SSH = "AcsCommandCenter.commandNativeSSH";
   
   public static boolean useNativeSSH() {return Boolean.getBoolean(SYSPROP_USE_NATIVE_SSH);}
   public static boolean killNativeSSH() {return Boolean.getBoolean(SYSPROP_KILL_NATIVE_SSH);}
   

   public static RemoteFlow remoteFlow = new RemoteFlow();
   public static LocalInProcFlow localInProcFlow = new LocalInProcFlow();
   public static LocalOutProcFlow localOutProcFlow = new LocalOutProcFlow();
   public static SingleStepFlow singleStepFlow = new SingleStepFlow();
   public static RemoteServicesDaemonFlow remoteServicesDaemonFlow = new RemoteServicesDaemonFlow();
   public static RemoteContainerDaemonFlow remoteContainerDaemonFlow = new RemoteContainerDaemonFlow();

   private static Logger log = MiscUtils.getPackageLogger(Executor.class);
   
   // ==========================================================================

   static public void remote(String username, String password, String command, String endMark, NativeCommand.Listener listener, String host) throws Throwable {
   	if (useNativeSSH()) {
   		remoteNative(username, password, command, endMark, listener, host);
   	} else {
   		int ticket = remotePortable(username, password, command, endMark, listener, host);
   	}
   }
   
   static public void remoteDownAll() {
   	if (useNativeSSH()) {
   		remoteDownAllNative();
   	} else {
   		remoteDownAllPortable();
   	}
   }
   
   //static public void remoteDown(int ticket) throws IOException {}
   
   
   ////////////////////////////////////////////////////////////////////
   // ------------------ Remote with SSH library ------------------- //
   ////////////////////////////////////////////////////////////////////
   
   
   static protected Vector<SshClient> clients = new Vector<SshClient>();

   /**
    * @return a ticket that can later be passed to remoteDown()
    * @throws IOException
    */
   static private int remotePortable(String username, String password, String command, String endMark, NativeCommand.Listener listener, String host) throws IOException {

      if (disableRemote)
         return 0;

      if (listener == null) {
         listener = new NativeCommand.ListenerAdapter(); // normalization: use a do-nothing implementation
      }
      
      
      try {
         remoteFlow.reset();

         int port = 22;
         SshClient client = new SshClient();

         int ticket = clients.size();
         clients.add(client);

         // int port = 23;
         // TelnetWrapper ssh = new TelnetWrapper();

         client.connect(host, port);
         remoteFlow.success(RemoteFlow.CONNECT);

         client.login(username, password);
         // there's no feedback at this point whether login succeeded

         

         //TODO(msc) if done this way, output will not be delivered until task ends. use a thread instead.
			String output = client.waitfor(new String[] { "$ ", "> " });
         remoteFlow.success(RemoteFlow.LOG_IN);

			if (output != null) {
				listener.stdoutWritten((NativeCommand) null, output);
			}
         log.finest("STDOUT:"+output);

         client.setPrompt(null); // make sure send() returns instantly

         log.fine("Now sending: '" + command + "'");

         client.send(command);
         remoteFlow.success(RemoteFlow.SEND_COMMAND);

         // ----
         String[] any = new String[100];
         for (int i = 0; i < 100; i++) {
            any[i] = new String(new char[] {(char) i });
         }
         // ----
         
         StringRingBuffer searchBuff = null;
         if (endMark != null) {
         	searchBuff = new StringRingBuffer(endMark.length());
         }
         
         process : for (;;) {
            output = null;
            try{
          //String out = client.waitfor(new String[] { normal, abnorm, runnin, "Starting Manager");
            output = client.waitfor(any);
            }catch(SocketException exc) {
               // a socket exception occurs when the client disconnects
               // in response to a call to its disconnect() method.
               // in that case, the exception is nothing bad: it's just annoying.
               if (client.disconnectRequested)
               	return ticket;
               else
               // in any other case, the exception is unexpected.
                  throw exc;
            }
            
            log.finest("STDOUT:"+output);
            
				// detect false alarm: some ssh-servers send null-output
				if (output != null) {
				
					listener.stdoutWritten((NativeCommand) null, output);

	            // don't know which output would identify the error case
	            //            if (out.endsWith(abnorm)) {
	            //               remoteFlow.failure("abnormal termination");
	            //               break;
	            //            }
	
	            if (endMark != null) {
			            for (int i = 0; i < output.length(); i++) {
			               searchBuff.add(output.charAt(i));
		               if (searchBuff.equals(endMark)) {
		                  remoteFlow.success(RemoteFlow.COMPLETE);
		                  break process;
		               }
		            }
	            }
				}

         }

         return ticket;
      } catch (IOException exc) {
         remoteFlow.failure(exc);
         throw exc;
      }
   }

   /**
    * Shuts down all ssh-clients that may still be active
    */
   static private void remoteDownAllPortable() {
       if (disableRemote)
           return;

       for (int i=0; i<clients.size(); i++) {
           try {
               remoteDownPortable(i);
           }catch(IOException exc) {
               log.info("could not disconnect "+clients.get(i));
           }
       }
    }
   
   /**
    * Shuts down the specified ssh-client.
    * @param ticket a ticket as returned by method <code>remote()</code>
    * @throws IOException   if disconnect fails
    */
   static private void remoteDownPortable(int ticket) throws IOException {
      if (disableRemote)
         return;

      SshWrapper client = null;
      client = (SshWrapper) clients.get(ticket);
      client.disconnect();
      log.info("disconnected "+client);
   }


   
   /**
    * Trivial extension of class SshWrapper with a more verbose toString() implementation.
    * Moreover it allows to distinguish whether a Socket exception in method waitFor() is
    * something serious or just an ordinary consequence of calling disconnect().
    */
   protected static class SshClient extends SshWrapper {
       protected String host;
       protected int port;
       
       @Override
		public void connect(String host, int port) throws IOException {
           this.disconnectRequested = false;
           this.host = host;
           this.port = port;
           super.connect(host, port);
       }

       @Override
		public String toString() {
           return "SSH Client on host '"+host+"', port '"+port+"'";
       }

       /** used to distinct the case where a socket exception thrown
        * in the client.waitFor() method (which indicates there's no
        * connection) should be ignored since disconnection was indeed wanted.
        */
       protected boolean disconnectRequested;
       
       @Override
		public void disconnect() throws IOException {
         this.disconnectRequested = true;
         super.disconnect();
      }
      
   }


   static class RemoteFlow extends Flow {
      static final String CONNECT = "Connect";
      static final String LOG_IN = "Log In";
      static final String SEND_COMMAND = "Send the Command";
      static final String COMPLETE = "Command Completion";
      {
         consistsOf(null, new String[] { CONNECT, LOG_IN, SEND_COMMAND, COMPLETE });
      }
   }
   
   ////////////////////////////////////////////////////////////////////
   // ---------------- Remote through native SSH ------------------- //
   ////////////////////////////////////////////////////////////////////
   
   
   static private Vector<NativeCommand> remoteNativeTasks = new Vector<NativeCommand>();
   
   // PENDING(msc): return the task, so shutdown can do task.sendCommand("exit") ? 
   static private void remoteNative(String username, final String password, final String command, String endMark, NativeCommand.Listener listener, String host) throws Throwable {
      
   	if (disableRemote)
         return;

      remoteFlow.reset();

      if (listener == null) {
         listener = new NativeCommand.ListenerAdapter(); // normalization: use a do-nothing implementation
      }
      
      // --- set up task

      // ssh-command can be changed at any time through a system property if necessary
      // msc (2006-04-28): added "-X" for X-forwarding, as done for OMC in Socorro in Jan'06
      String sshPatternDefault = "ssh -X -t -l ? ? /bin/bash --login -c \"?\"";
      String sshPattern = System.getProperty(SYSPROP_COMMAND_NATIVE_SSH, sshPatternDefault);
      PreparedString prep = new PreparedString(sshPattern);
      String sshCommand = prep.toString(new String[]{username, host, command});
      /*String sshCommand = "ssh -t -l "+username+" "+host+" /bin/bash --login -c \""+command+"\"";*/
      
      boolean foreground = true;
      long maxExecutionTime = -1;
      endMark = ".*" + endMark + ".*"; // make a regular expression
      NativeCommand task = new NativeCommand(sshCommand, foreground, maxExecutionTime, endMark);

      remoteNativeTasks.add(task);
      
      task.addListener(listener);
      
      task.addListener(new NativeCommand.ListenerAdapter() {
         @Override
			public void statusChanged(NativeCommand task, String oldStatus) {
         	if (task.getStatus() == NativeCommand.RUNNING) {
         		remoteFlow.success(RemoteFlow.CONNECT);
         	}
         }
      });

      
      // --- run task
      
      // the current thread will block if "foreground" is true
      task.run();

      
      // --- examine results

      Throwable latestExc = task.getLatestException();

      // if we get here and the process-status is still RUNNING,
      // it simply means the expected output has occurred but the
      // process itself is still running

     	System.out.println("Process invocation returns. Process is now: " + task.getStatus() + ", exitvalue: " + 
     			( task.getExitValue()!=null? task.getExitValue().toString() : "none (yet)" ) + ", command was '" + command + "'"
		);

      log.fine("Process invocation returns. Process is now: " + task.getStatus() + ", exitvalue: " + 
     			( task.getExitValue()!=null? task.getExitValue().toString() : "none (yet)" ) + ", command was '" + command + "'"
		);

      if (latestExc != null) {
      	log.fine("During process invocation (at least) one error occurred: " + latestExc);

      	remoteFlow.failure(latestExc);
         throw latestExc;
      } else {

      	remoteFlow.success(RemoteFlow.COMPLETE);
      }
         
   }
   
   static private void remoteDownAllNative() {
   	if (killNativeSSH()) {
	   	for (Enumeration en = remoteNativeTasks.elements(); en.hasMoreElements();) {
	   		NativeCommand t = null;
				try {
					t = (NativeCommand) en.nextElement();
					t.process.destroy();
				} catch (Exception e) {
		      	log.finest("Failed to destroy native-ssh task " + t);
				}
			}
   	}
   }

   
   ////////////////////////////////////////////////////////////////////
   // --------------------- Local In-Process ----------------------- //
   ////////////////////////////////////////////////////////////////////
   
   
   
   //   static protected Vector threads = new Vector();
   
   /**
    * @param properties the properties to insert (and override) into the system properties
    * @param pexpect as soon as the process writes this to stdout, this method returns
    * @param listener a proprietary listener if the caller wants to hear about the process' output
    * @param runMain a callback that will be invoked by the newly spawned thread
    */
   static public Thread localInProc(Properties properties, String pexpect, NativeCommand.Listener listener, final RunMain runMain) {
      
      if (listener == null) {
         listener = new NativeCommand.ListenerAdapter(); // normalization: use a do-nothing implementation
      }
      
      localInProcFlow.reset();

      // insert given properties into system properties
      System.getProperties().putAll(properties);
      //CommandCenter.dbg_printProps(System.getProperties(), "sysprops");
      
      // --- use our own search-enabled stream as stdout
      LocalInProcStream outstream = new LocalInProcStream();
      outstream.setListener(listener);
      outstream.setConsumer(Thread.currentThread(), pexpect);
      System.setOut(new PrintStream(outstream));

      Thread t = new Thread() {
         @Override
			public void run() {
               //localInProcFlow.trying(LocalInProcFlow.ALIVE);
            try {
               runMain.runMain();
            } catch (Throwable exc) {
               localInProcFlow.failure(exc);
            }
         }
      };
      t.start();

      localInProcFlow.success(LocalInProcFlow.START);

      try {
         Thread.sleep(Long.MAX_VALUE);
      } catch (InterruptedException exc) {}
      // it is safe to leave subthread t alone now
      // it can now do what it wants, the main thread goes on

      localInProcFlow.success(LocalInProcFlow.ALIVE);
      //CommandCenter.dbg_printProps(System.getProperties(), "sysprops");
      return t;
   }

   

   static protected class LocalInProcStream extends OutputStream {

      String search;
      Thread consumer;
      StringRingBuffer searchBuff;

      NativeCommand.Listener listener;

      void setListener(NativeCommand.Listener listener) {
         this.listener = listener;
      }

      void setConsumer(Thread consumer, String search) {
         this.consumer = consumer;
         this.search = search;
         if (search != null) {
         	this.searchBuff = new StringRingBuffer(search.length());
         }
         	
      }

      StringBuffer lineBuffer = new StringBuffer(512);

      @Override
		public void write(int b) throws IOException {
         char c = (char) b;
         // risk of infinite loop: 
         // if above we do System.setOut(), we must not do System.out.println() here
         // System.err.print(c); 

         // deal with Task.Listener if it exists
         if (listener != null) {
            lineBuffer.append(c);

            if (c == '\n') {
               listener.stdoutWritten(null, lineBuffer.toString());
               lineBuffer.setLength(0);
            }
         }

         // deal with search expression if it exists
         if (search != null && searchBuff != null) {
	         searchBuff.add(c);
	
	         if (searchBuff.equals(search)) {
	            // System.err.println("\n" + search + " occured\n");
	            consumer.interrupt();
	         }
         }
      }
   }

   //   static public void localDown(int ticket) {
   //      Thread t = (Thread)threads.get(ticket);
   //      t.
   //   }
   
   

   /**
    * The only sense of this is to have a flow for normal java instructions
    * that may take a while.
    * @param runMain the java instructions to perform
    */
   static public void local(final RunMain runMain) {
      singleStepFlow.reset();
      try {
         runMain.runMain();

      } catch (Throwable exc) {
         singleStepFlow.failure(exc);
      }
      singleStepFlow.success(SingleStepFlow.DONE);
   }
   
   
   static public interface RunMain {
      public void runMain() throws Throwable;
   }
   
   
   
   static class LocalInProcFlow extends Flow {
      static final String START = "Thread Start";
      static final String ALIVE = "Delegate Up";
      {
         consistsOf(null, new String[] { START, ALIVE });
      }
   }
   
   
   static class SingleStepFlow extends Flow {
      static final String DONE = "Task completion";
      {
         consistsOf(null, new String[] { DONE });
      }
   }
   
   
   
   ////////////////////////////////////////////////////////////////////
   // ------------------- Local Out-of-Process --------------------- //
   ////////////////////////////////////////////////////////////////////

   
   /**
    *  
    */
   static public void localOutProc(String command, boolean foreground, long maxExecutionTime, String endMark, NativeCommand.Listener listener) throws Throwable {

      if (listener == null) {
         listener = new NativeCommand.ListenerAdapter(); // normalization: use a do-nothing implementation
      }
      
      localOutProcFlow.reset();
      log.fine("Now executing: '" + command + "'");

      // --- set up task

      // we make the endMark a regular expression by enclosing it with ".*"
      NativeCommand task = new NativeCommand(command, foreground, maxExecutionTime, ".*" + endMark + ".*");
      task.addListener(listener);
      
      task.addListener(new NativeCommand.ListenerAdapter() {
         @Override
			public void statusChanged(NativeCommand task, String oldStatus) {
         	if (task.getStatus() == NativeCommand.RUNNING)
               localOutProcFlow.success(LocalOutProcFlow.RUN);
         }
      });

      
      // --- run task
      
      // the current thread will 
      // block if "foreground" is true
      task.run();

      
      // --- examine results

      Throwable latestExc = task.getLatestException();

      // if we get here and the process-status is still RUNNING,
      // it simply means the expected output has occurred but the
      // process itself is still running

     	System.out.println("Process invocation returns. Process is now: " + task.getStatus() + ", exitvalue: " + 
     			( task.getExitValue()!=null? task.getExitValue().toString() : "none (yet)" ) + ", command was '" + command + "'"
		);

      log.fine("Process invocation returns. Process is now: " + task.getStatus() + ", exitvalue: " + 
     			( task.getExitValue()!=null? task.getExitValue().toString() : "none (yet)" ) + ", command was '" + command + "'"
		);

      if (latestExc != null) {
      	log.fine("During process invocation (at least) one error occurred: " + latestExc);

      	localOutProcFlow.failure(latestExc);
         throw latestExc;
      } else {

      	localOutProcFlow.success(LocalOutProcFlow.COMPLETE);
      }
   }


   static class LocalOutProcFlow extends Flow {
      static final String RUN = "Process Launch";
      static final String COMPLETE = "Process Completion";
      {
         consistsOf(null, new String[] { RUN, COMPLETE });
      }
   }


   ////////////////////////////////////////////////////////////////////
   // ------------------- Remote using Daemons --------------------- //
   ////////////////////////////////////////////////////////////////////

   static private Firestarter firestarter;
   
   static public void remoteDaemonEnable (Firestarter fs) {
   	firestarter = fs;
   }
   
   static public void remoteDaemonForServices (String host, int instance, boolean startStop, String cmdFlags, NativeCommand.Listener listener) {
   	if (listener != null) {
   		listener.stdoutWritten(null, "\nIn daemon mode, output cannot be displayed.\n" +
   				"See logs in <daemon-owner>/.acs/commandcenter on host "+host+"\n");
   	}
   	
   	remoteServicesDaemonFlow.reset();

   	ACSPorts ports = ACSPorts.globalInstance(instance);
		String daemonLoc = AcsLocations.convertToServicesDaemonLocation(host, ports.giveServicesDaemonPort());

		org.omg.CORBA.ORB orb;
		try {
			orb = firestarter.giveOrb();
		} catch (OrbInitException exc) {
			remoteServicesDaemonFlow.failure(exc);
			return;
		}
		remoteServicesDaemonFlow.success(RemoteServicesDaemonFlow.INIT_CORBA);

		
		ServicesDaemon daemon;
		try {
			org.omg.CORBA.Object object = orb.string_to_object(daemonLoc);
			daemon = ServicesDaemonHelper.narrow(object);
			if (daemon == null)
				throw new NullPointerException("received null trying to retrieve acsdaemon on "+host);
		} catch (RuntimeException exc) {
			remoteServicesDaemonFlow.failure(exc);
			return;
		}
		remoteServicesDaemonFlow.success(RemoteServicesDaemonFlow.CONNECT_DAEMON);
		
		
		try {
			if (startStop == true) {
				daemon.start_acs((short)instance, cmdFlags);
			} else {
				daemon.stop_acs((short)instance, cmdFlags);
			}
		} catch (Exception exc) {
			remoteServicesDaemonFlow.failure(exc);
			return;
		}
		remoteServicesDaemonFlow.success(RemoteServicesDaemonFlow.SEND_COMMAND);


		boolean isManagerThere;
		String managerLoc = AcsLocations.convertToManagerLocation(host, ports.giveManagerPort());
		int wait = 60;
		int every = 3;
		for (int i = 0; i < wait; i += every) {
			try {
				Thread.sleep(every * 1000);
			} catch (InterruptedException exc) {
				break; // who interrupted us? anyway, stop probing.
			}
			try {
				org.omg.CORBA.Object obj = orb.string_to_object(managerLoc);
				isManagerThere = !obj._non_existent(); // simple check if obj is null wouldn't work
			} catch (RuntimeException exc) {
				isManagerThere = false;
			}
			if (startStop == isManagerThere) {
				remoteServicesDaemonFlow.success(RemoteServicesDaemonFlow.AWAIT_RESPONSE);
				return;
			}
		}
		remoteServicesDaemonFlow.failure("Couldn't verify daemon executed successfully");

   }

   static class RemoteServicesDaemonFlow extends Flow {
   	static final String INIT_CORBA = "Assert corba connectivity";
   	static final String CONNECT_DAEMON = "Connect to acsservicesdaemon";
   	static final String SEND_COMMAND = "Send command to daemon";
   	static final String AWAIT_RESPONSE = "Receive remote response";
   	{
   		consistsOf (null, new String[]{INIT_CORBA, CONNECT_DAEMON, SEND_COMMAND, AWAIT_RESPONSE});
   	}
   }
   
   /**
    * @param contType - only needed for starting (i.e. startStop==true)
    */
   static public void remoteDaemonForContainers (String host, int instance, boolean startStop, String contName, String contType, String cmdFlags, NativeCommand.Listener listener) {
   	if (listener != null) {
   		listener.stdoutWritten(null, "\nIn daemon mode, output cannot be displayed.\n" +
   				"See logs in <daemon-owner>/.acs/commandcenter on host "+host+"\n");
   	}

   	remoteContainerDaemonFlow.reset();

   	ACSPorts ports = ACSPorts.globalInstance(instance);
		String daemonLoc = AcsLocations.convertToContainerDaemonLocation(host, ports.giveContainerDaemonPort());

		org.omg.CORBA.ORB orb = null;
		try {
			orb = firestarter.giveOrb();
		} catch (OrbInitException exc) {
			remoteContainerDaemonFlow.failure(exc);
			return;
		}
		remoteContainerDaemonFlow.success(RemoteContainerDaemonFlow.INIT_CORBA);
		
		
		ContainerDaemon daemon;
		try {
			org.omg.CORBA.Object object = orb.string_to_object(daemonLoc);
			daemon = ContainerDaemonHelper.narrow(object);
			if (daemon == null)
				throw new NullPointerException("received null trying to retrieve acsdaemon on "+host);
		} catch (RuntimeException exc) {
			remoteContainerDaemonFlow.failure(exc);
			return;
		}
		remoteContainerDaemonFlow.success(RemoteContainerDaemonFlow.CONNECT_DAEMON);

		
		
		try {
			if (startStop == true) {
				daemon.start_container(contType, contName, (short)instance, cmdFlags);
			} else {
				daemon.stop_container(contName, (short)instance, cmdFlags);
			}
		} catch (Exception exc) {
			remoteContainerDaemonFlow.failure(exc);
			return;
		}

		// i'm adding this possibility to sleep a little simply because 
		// it gives the user a feeling of "something is happening".
		// this can be reconfigured, however, if desired (omc will do so).
		try {
			Thread.sleep(remoteDaemonForContainersCompletionDelay);
		} catch (InterruptedException exc) {}
		
		remoteContainerDaemonFlow.success(RemoteContainerDaemonFlow.SEND_COMMAND);

   }

   static public int remoteDaemonForContainersCompletionDelay = 2500;
   
   static class RemoteContainerDaemonFlow extends Flow {
   	static final String INIT_CORBA = "Assert corba connectivity";
   	static final String CONNECT_DAEMON = "Connect to acscontainerdaemon";
   	static final String SEND_COMMAND = "Send command to daemon";
   	{
   		consistsOf (null, new String[]{INIT_CORBA, CONNECT_DAEMON, SEND_COMMAND});
   	}
   }

   
   

}


