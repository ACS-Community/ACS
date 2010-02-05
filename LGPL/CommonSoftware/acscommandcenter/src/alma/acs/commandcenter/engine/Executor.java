/*
 * Created on Oct 21, 2003 by mschilli
 */
package alma.acs.commandcenter.engine;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.Enumeration;
import java.util.Properties;
import java.util.Vector;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import alma.ACSErr.Completion;
import alma.acs.commandcenter.meta.Firestarter;
import alma.acs.commandcenter.meta.Firestarter.OrbInitException;
import alma.acs.commandcenter.trace.Flow;
import alma.acs.commandcenter.util.MiscUtils;
import alma.acs.commandcenter.util.PreparedString;
import alma.acs.commandcenter.util.StringRingBuffer;
import alma.acs.container.corba.AcsCorba;
import alma.acs.exceptions.AcsJCompletion;
import alma.acs.exceptions.AcsJException;
import alma.acs.util.AcsLocations;
import alma.acsdaemon.ContainerDaemon;
import alma.acsdaemon.ContainerDaemonHelper;
import alma.acsdaemon.DaemonSequenceCallback;
import alma.acsdaemon.DaemonSequenceCallbackHelper;
import alma.acsdaemon.DaemonSequenceCallbackPOA;
import alma.acsdaemon.ServicesDaemon;
import alma.acsdaemon.ServicesDaemonHelper;

import com.trilead.ssh2.ChannelCondition;
import com.trilead.ssh2.Connection;
import com.trilead.ssh2.Session;

/**
 * @author mschilli
 */
public class Executor {

   public static boolean disableRemote = false;
   
   public static final String SYSPROP_COMMAND_NATIVE_SSH = "AcsCommandCenter.commandNativeSSH";

   public static RemoteFlow remoteFlow = new RemoteFlow();
   public static LocalInProcFlow localInProcFlow = new LocalInProcFlow();
   public static LocalOutProcFlow localOutProcFlow = new LocalOutProcFlow();
   public static SingleStepFlow singleStepFlow = new SingleStepFlow();
   public static RemoteServicesDaemonFlow remoteServicesDaemonFlow = new RemoteServicesDaemonFlow();
   public static RemoteContainerDaemonFlow remoteContainerDaemonFlow = new RemoteContainerDaemonFlow();

   private static Logger log = MiscUtils.getPackageLogger(Executor.class);
   
   // ==========================================================================

   /**
    * @return false - if this failed gracefully
    * @throws IOException - if this failed severely
    */
   static public boolean remote(boolean nativeSSH, String username, String password, String command, String endMark, NativeCommand.Listener listener, String host) throws Throwable {
   	if (nativeSSH)
   		return remoteNative(username, password, command, endMark, listener, host);
   	else
   		return remotePortable(username, password, command, endMark, listener, host);
   	
   }
   
   static public void remoteDownAll() {
   	// msc 2009-04: CommandCenterLogic uses this on shutdown. this method would
   	// need a "nativeSSH" flag, too, but i would like to avoid that. it then
   	// occurred to me that it shouldn't hurt (maybe it's even a good idea) to
   	// try and bring down both types of remote processes in any case.
		remoteDownAllNative();
		remoteDownAllPortable();
   }
   
 
   ////////////////////////////////////////////////////////////////////
   // ------------------ Remote with SSH library ------------------- //
   ////////////////////////////////////////////////////////////////////


   static protected Vector<Connection> connections = new Vector<Connection>();
   static protected Vector<Session> sessions = new Vector<Session>();

   /**
    * @return false - if this failed gracefully
    * @throws IOException - if this failed severely
    */
   static private boolean remotePortable(String username, String password, String command, String endMark, NativeCommand.Listener listener, String host) throws IOException {

      if (listener == null) {
         listener = new NativeCommand.ListenerAdapter(); // normalization: use a do-nothing implementation
      }
      
      
      try {
         remoteFlow.reset(null);

         // connect
         // --------
			Connection conn = new Connection(host);
			connections.add(conn);
			conn.connect();
         remoteFlow.success(RemoteFlow.CONNECT);

         // login
         // ------
         boolean isAuthenticated = conn.authenticateWithPassword(username, password);
			if (isAuthenticated == false)
				throw new IOException("Authentication failed");
         remoteFlow.success(RemoteFlow.LOG_IN);

         // send command
         // -------------
         Session sess = conn.openSession();
         sessions.add(sess);

         /* msc 2008-11:
          * We're passing the command as an argument to ssh, just like typing
          * > ssh 127.0.0.1 "env" 
          * on the commandline. This opens a non-login shell on the remote host
          * which (thank you bash) won't parse the .bash_profile but only the .bashrc!
          * Now unfortunately the usual setup for accounts on Alma machines is to
          * have the ACS settings defined in .bash_profile. The optimal way around
          * this would be to run a real login shell here by allocating a terminal,
          * and then deal with all the input output/stuff ourselves. I'm trying here
          * to get away with something cheaper: Explicitly source the .bash_profile
          * before running the command.
          */
         command = ". ~/.bash_profile ; " + command;

         log.info("Now sending: '" + command + "'");
         sess.execCommand(command);
         remoteFlow.success(RemoteFlow.SEND_COMMAND);

         // read output, scan for endmark
         // ----------------------------
         SearchBuffer searchStdout = null;
         SearchBuffer searchStderr = null;
         if (endMark != null) {
         	searchStdout = new SearchBuffer(endMark);
         	searchStderr = new SearchBuffer(endMark);
         }

			InputStream stdout = sess.getStdout();
			InputStream stderr = sess.getStderr();
			byte[] buffer = new byte[8192];

			while (true) {
				if (stdout.available() == 0 && stderr.available() == 0) {
					/* Even though currently there is no data available, it may be that new data arrives
					 * and the session's underlying channel is closed before we call waitForCondition().
					 * This means that EOF and STDOUT_DATA (or STDERR_DATA, or both) may be set together. */
					
					int conditions = sess.waitForCondition(
							ChannelCondition.STDOUT_DATA | ChannelCondition.STDERR_DATA | ChannelCondition.EOF, //
							10*1000); // allow several seconds

					if ((conditions & ChannelCondition.TIMEOUT) != 0) {
						throw new IOException("Timeout while waiting for data from peer");
					}

					if ((conditions & ChannelCondition.EOF) != 0) {
						/* The remote side won't send us further data ... */
						if ((conditions & (ChannelCondition.STDOUT_DATA | ChannelCondition.STDERR_DATA)) == 0) {
							/* ... and we have consumed all data in the local arrival window. */
							break;
						}
					}

					/* At this point, either STDOUT_DATA or STDERR_DATA, (or both) is set. */
				}

				/* If you below use "if" instead of "while", then the way the output appears on the local
				 * stdout and stder streams is more "balanced". Addtionally reducing the buffer size
				 * will also improve the interleaving, but performance will slightly suffer.
				 * OKOK, that all matters only if you get HUGE amounts of stdout and stderr data =)
				 */
				if (stdout.available() > 0) {
					int len = stdout.read(buffer);
					listener.stdoutWritten(null, new String(buffer, 0, len));

					if (searchStdout != null)
						if (searchStdout.add(buffer, 0, len)) {
							remoteFlow.success(RemoteFlow.COMPLETE);
							break;
						}
				}

				if (stderr.available() > 0) {
					int len = stderr.read(buffer);
					// msc 2008-11: porting to a different ssh library. this should of course
					// call stderrWritten() but i don't want to change the original behavior.
					listener.stdoutWritten(null, new String(buffer, 0, len));
					
					if (searchStderr != null)
						if (searchStderr.add(buffer, 0, len)) {
							remoteFlow.success(RemoteFlow.COMPLETE);
							break;
						}
				}

			}
			
			return true;

      } catch (IOException exc) {
         remoteFlow.failure(exc);
         // we kind of expect IOExceptions and it should be enough 
         // to show them in the flow dialog. so we don't rethrow them.
         /* throw exc; */
         return false;
      }
   }

   /**
	 * Shuts down all ssh-sessions and -connections that may still be active.
	 */
	static private void remoteDownAllPortable () {

		for (Session sess : sessions) {
			try {
				sess.close();
				log.fine("closed " + sess);

			} catch (Exception exc) {
				log.fine("could not close " + sess);
			}
		}

		for (Connection conn : connections) {
			try {
				conn.close();
				log.fine("closed " + conn);

			} catch (Exception exc) {
				log.fine("could not close " + conn);
			}
		}
	}

   


   public static class RemoteFlow extends Flow {
      static final String CONNECT = "Connect";
      static final String LOG_IN = "Log In";
      static final String SEND_COMMAND = "Send Command";
      static final String COMPLETE = "Command Completion";
      {
         consistsOf(null, new String[] { CONNECT, LOG_IN, SEND_COMMAND, COMPLETE });
      }
   }
   
   ////////////////////////////////////////////////////////////////////
   // ---------------- Remote through native SSH ------------------- //
   ////////////////////////////////////////////////////////////////////
   
   
   static private Vector<NativeCommand> remoteNativeTasks = new Vector<NativeCommand>();
   
   /**
    * @return false - if this failed gracefully
    * @throws Throwable - if this failed severely
    */
   static private boolean remoteNative(String username, final String password, final String command, String endMark, NativeCommand.Listener listener, String host) throws Throwable {
      
   	localOutProcFlow.reset(null);

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
         		localOutProcFlow.success(LocalOutProcFlow.RUN);
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

      log.info("Process invoked. Process is now: " + task.getStatus() + ". Exitcode: " + 
     			( task.getExitValue()!=null? task.getExitValue().toString() : "none (yet)" ) + ". Command was '" + command + "'"
		);

      if (latestExc != null) {
      	log.fine("During process invocation (at least) one error occurred: " + latestExc);

      	remoteFlow.failure(latestExc);
         throw latestExc;
      } else {

      	localOutProcFlow.success(LocalOutProcFlow.COMPLETE);
      	return true;
      }
         
   }
   
   static private void remoteDownAllNative() {
   	for (Enumeration<NativeCommand> en = remoteNativeTasks.elements(); en.hasMoreElements();) {
   		NativeCommand t = null;
			try {
				t = (NativeCommand) en.nextElement();
				t.process.destroy();
			} catch (Exception e) {
	      	log.finest("Failed to destroy native-ssh task " + t);
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
   static public void localInProc(Properties properties, String pexpect, NativeCommand.Listener listener, final RunMain runMain) {
      
      if (listener == null) {
         listener = new NativeCommand.ListenerAdapter(); // normalization: use a do-nothing implementation
      }
      
      localInProcFlow.reset(null);

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

   
   

   /**
    * The only sense of this is to have a flow for normal java instructions
    * that may take a while.
    * @param runMain the java instructions to perform
    */
   static public void local(final RunMain runMain) {
      singleStepFlow.reset(null);
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
   
   
   
   public static class LocalInProcFlow extends Flow {
      static final String START = "Thread Start";
      static final String ALIVE = "Delegate Up";
      {
         consistsOf(null, new String[] { START, ALIVE });
      }
   }
   
   
   public static class SingleStepFlow extends Flow {
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
      
      localOutProcFlow.reset(null);
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

     	log.info("Process invoked. Process is now: " + task.getStatus() + ". Exitcode: " + 
     			( task.getExitValue()!=null? task.getExitValue().toString() : "none (yet)" ) + ". Command was '" + command + "'"
		);

      if (latestExc != null) {
      	log.fine("During process invocation (at least) one error occurred: " + latestExc);

      	localOutProcFlow.failure(latestExc);
         throw latestExc;
      } else {

      	localOutProcFlow.success(LocalOutProcFlow.COMPLETE);
      }
   }


   public static class LocalOutProcFlow extends Flow {
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

	static public void remoteDaemonEnable(Firestarter fs) {
		firestarter = fs;
	}

	/**
	 * Starts or stops ACS via the ACS services daemon. 
	 * This call returns only when the action has completed.
	 * Exceptions will be returned instead of thrown.
	 * @return any exception that occurs underways
	 */
	/* msc 2009-12: this method has never thrown exceptions, instead they can be detected through
	 * Flow listening, and as of today also by looking at the return value. Starting to throw exceptions
	 * would be too big a change that I don't want to risk. I have no time to verify it doesn't harm. */
	static public Exception remoteDaemonForServices(String host, int instance, boolean startStop, String cmdFlags,
			NativeCommand.Listener listener) {
		if (listener != null) {
			listener.stdoutWritten(null, "\nIn daemon mode, output cannot be displayed.\n"
					+ "See logs in <daemon-owner>/.acs/commandcenter on host " + host + "\n");
		}

		String info = ((startStop) ? "Starting" : "Stopping") + " Acs Suite on host '" + host + "' (instance "+ instance + ")";
		remoteServicesDaemonFlow.reset(info);

		String daemonLoc = AcsLocations.convertToServicesDaemonLocation(host);

		org.omg.CORBA.ORB orb;
		AcsCorba acsCorba = null;
		remoteServicesDaemonFlow.trying(RemoteServicesDaemonFlow.INIT_CORBA);
		try {
			acsCorba = firestarter.giveAcsCorba();			
			orb = acsCorba.getORB();
		} catch (OrbInitException exc) {
			remoteServicesDaemonFlow.failure(exc);
			return new Exception(RemoteServicesDaemonFlow.INIT_CORBA+": "+exc.getMessage(), exc);
		}
		remoteServicesDaemonFlow.success(RemoteServicesDaemonFlow.INIT_CORBA);


		ServicesDaemon daemon;
		remoteServicesDaemonFlow.trying(RemoteServicesDaemonFlow.CONNECT_DAEMON);
		try {
			org.omg.CORBA.Object object = orb.string_to_object(daemonLoc);
			daemon = ServicesDaemonHelper.narrow(object);
			if (daemon == null)
				throw new NullPointerException("received null trying to retrieve acsdaemon on "+host);
			if (daemon._non_existent()) // this may be superfluous with daemons but shouldn't hurt either
				throw new RuntimeException("acsdaemon not existing on "+host);
		} catch (RuntimeException exc) {
			remoteServicesDaemonFlow.failure(exc);
			return new Exception(RemoteServicesDaemonFlow.CONNECT_DAEMON+": "+exc.getMessage(), exc);
		}
		remoteServicesDaemonFlow.success(RemoteServicesDaemonFlow.CONNECT_DAEMON);

		try {

			remoteServicesDaemonFlow.trying(RemoteServicesDaemonFlow.SEND_COMMAND);
			final BlockingQueue<Completion> sync = new ArrayBlockingQueue<Completion>(1);
			DaemonSequenceCallbackPOA daemonCallbackImpl = new DaemonSequenceCallbackPOA() {
				public void done(Completion comp) {sync.add(comp);}
				public void working(String service, String host, short instance_number, Completion comp) {}
			};
			DaemonSequenceCallback daemonCallback = DaemonSequenceCallbackHelper.narrow(acsCorba.activateOffShoot(daemonCallbackImpl, acsCorba.getRootPOA()) );

			if (startStop == true)
				daemon.start_acs(daemonCallback, (short)instance, cmdFlags);
			else
				daemon.stop_acs(daemonCallback, (short)instance, cmdFlags);
			remoteServicesDaemonFlow.success(RemoteServicesDaemonFlow.SEND_COMMAND);


			// The services daemon's start/stop methods are implemented asynchronously,
			// which means we need to wait for the callback notification.
			// @TODO: Perhaps a 10 minute timeout is too much though?
			remoteServicesDaemonFlow.trying(RemoteServicesDaemonFlow.AWAIT_RESPONSE);
			long timeout = 10; TimeUnit timeoutUnit = TimeUnit.MINUTES;
			Completion daemonReplyRaw = sync.poll(timeout, timeoutUnit);
			if (daemonReplyRaw != null) {
				AcsJCompletion daemonReply = AcsJCompletion.fromCorbaCompletion(daemonReplyRaw);
				if (daemonReply.isError()) {
					AcsJException exc = daemonReply.getAcsJException();
					remoteServicesDaemonFlow.failure(exc);
					return new Exception(RemoteServicesDaemonFlow.AWAIT_RESPONSE+": "+exc.getMessage(), exc);
				}
			}
			else {
				// Timeout while waiting for callback from the daemon
				RuntimeException exc = new RuntimeException("Timeout: Acs daemon did not "+(startStop?"start":"stop")+" Acs within "+timeout+" "+timeoutUnit.toString());
				remoteServicesDaemonFlow.failure(exc);
				return new Exception(RemoteServicesDaemonFlow.AWAIT_RESPONSE+": "+exc.getMessage(), exc);
			}

			remoteServicesDaemonFlow.success(RemoteServicesDaemonFlow.AWAIT_RESPONSE);
			return null;


		} catch (Exception exc) {
			remoteServicesDaemonFlow.failure(exc);
			return new Exception(remoteServicesDaemonFlow.current()+": "+exc.getMessage(), exc);
		}
	}

   public static class RemoteServicesDaemonFlow extends Flow {
   	static final String INIT_CORBA = "Assert corba connectivity";
   	static final String CONNECT_DAEMON = "Connect to acsservicesdaemon";
   	static final String SEND_COMMAND = "Send command to daemon";
   	static final String AWAIT_RESPONSE = "Receive remote response";
   	{
   		consistsOf (null, new String[]{INIT_CORBA, CONNECT_DAEMON, SEND_COMMAND, AWAIT_RESPONSE});
   	}
   }

   /**
    * @param startStop - if true, the daemon starts the container, otherwise stops it. @todo rethink how useful this overloading is.
    * @param contType - only needed for starting (i.e. startStop==true)
    * @param contTypeMods - only needed for starting (i.e. startStop==true)
    * @see http://www.eso.org/projects/alma/develop/acs/OnlineDocs/ACS_docs/schemas/urn_schemas-cosylab-com_Container_1.0/complexType/DeployInfo.html 
    */
	/* msc 2010-02: this method has never thrown exceptions, instead they can be detected through
	 * Flow listening, and as of today also by looking at the return value. Starting to throw exceptions
	 * would be too big a change that I don't want to risk. I have no time to verify it doesn't harm. */
   static public Exception remoteDaemonForContainers (String host, int instance, boolean startStop, String contName, String contType, String[] contTypeMods, String cmdFlags, NativeCommand.Listener listener) {
   	if (listener != null) {
   		listener.stdoutWritten(null, "\nIn daemon mode, output cannot be displayed.\n" +
   				"See logs in <daemon-owner>/.acs/commandcenter on host "+host+"\n");
   	}

   	String info = ((startStop)? "Starting" : "Stopping") + " container "+contName+" on host '"+host+"' (instance "+instance+")";
   	remoteContainerDaemonFlow.reset(info);

		String daemonLoc = AcsLocations.convertToContainerDaemonLocation(host);

		org.omg.CORBA.ORB orb = null;
		remoteContainerDaemonFlow.trying(RemoteContainerDaemonFlow.INIT_CORBA);
		try {
			orb = firestarter.giveOrb();
		} catch (OrbInitException exc) {
			remoteContainerDaemonFlow.failure(exc);
			return new Exception(RemoteContainerDaemonFlow.INIT_CORBA+": "+exc.getMessage());
		}
		remoteContainerDaemonFlow.success(RemoteContainerDaemonFlow.INIT_CORBA);
		
		
		ContainerDaemon daemon;
		remoteContainerDaemonFlow.trying(RemoteContainerDaemonFlow.CONNECT_DAEMON);
		try {
			org.omg.CORBA.Object object = orb.string_to_object(daemonLoc);
			daemon = ContainerDaemonHelper.narrow(object);
			if (daemon == null)
				throw new NullPointerException("received null trying to retrieve acsdaemon on "+host);
			if (daemon._non_existent()) // this may be superfluous with daemons but shouldn't hurt either
				throw new RuntimeException("acsdaemon not existing on "+host);
		} catch (RuntimeException exc) {
			remoteContainerDaemonFlow.failure(exc);
			return new Exception(RemoteContainerDaemonFlow.CONNECT_DAEMON+": "+exc.getMessage());
		}
		remoteContainerDaemonFlow.success(RemoteContainerDaemonFlow.CONNECT_DAEMON);

		
		remoteContainerDaemonFlow.trying(RemoteContainerDaemonFlow.SEND_COMMAND);
		try {
			if (startStop == true) {
				daemon.start_container(contType, contName, (short)instance, contTypeMods, cmdFlags);
			} else {
				daemon.stop_container(contName, (short)instance, cmdFlags);
			}
		} catch (Exception exc) {
			remoteContainerDaemonFlow.failure(exc);
			return new Exception(RemoteContainerDaemonFlow.SEND_COMMAND+": "+exc.getMessage());
		}

		// i'm adding this possibility to sleep a little simply because 
		// it gives the user a feeling of "something is happening".
		// this can be reconfigured, however, if desired (omc will do so).
		try {
			Thread.sleep(remoteDaemonForContainersCompletionDelay);
		} catch (InterruptedException exc) {}
		
		remoteContainerDaemonFlow.success(RemoteContainerDaemonFlow.SEND_COMMAND);
		return null;
   }

   static public int remoteDaemonForContainersCompletionDelay = 2500;
   
   public static class RemoteContainerDaemonFlow extends Flow {
   	static final String INIT_CORBA = "Assert corba connectivity";
   	static final String CONNECT_DAEMON = "Connect to acscontainerdaemon";
   	static final String SEND_COMMAND = "Send command to daemon";
   	{
   		consistsOf (null, new String[]{INIT_CORBA, CONNECT_DAEMON, SEND_COMMAND});
   	}
   }


   /**
    * Helper class for stream parsing, this doesn't bear the overhead
    * of util class StringRingBuffer.
    */
   static class SearchBuffer {

   	byte[] search;
		byte[] data;
		int next = 0;
		boolean isFillingUp = true;

		SearchBuffer (String searched) {
			search = searched.getBytes();
			data = new byte[search.length];
		}

		/**
		 * Convenience method for feeding bytes into this buffer. 
		 * @return whether the search-expression was found
		 */
		boolean add (byte[] bytes, final int off, final int len) {
			for (int i=off; i<off+len; i++) {
				if (add (bytes[i]))
					return true;
			}
			return false;
		}

		/**
		 * @param b - a newly incoming byte
		 * @return whether the byte made the search-expression complete
		 */
		boolean add (byte b) {
			
			// fill new character into this
			// -----------------------------
			data[next] = b;

			next += 1;
			if (next == data.length)
				next = 0;

			if (isFillingUp && next == 0)
				isFillingUp = false;


			// compare this with searched
			// ---------------------------
			if (isFillingUp)
				return false;

			int idx;
			int otherIdx = 0;

			idx = next;
			while (idx < data.length) {
				if (data[idx] != search[otherIdx])
					return false;
				idx++;
				otherIdx++;
			}

			idx = 0;
			while (idx < next) {
				if (data[idx] != search[otherIdx])
					return false;
				idx++;
				otherIdx++;
			}
			return true;
		}

   }


}


