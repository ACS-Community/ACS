/*
 * Created on Jul 16, 2003 by mschilli
 *
 */
package alma.acs.commandcenter.engine;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.Enumeration;
import java.util.Timer;
import java.util.TimerTask;
import java.util.Vector;
import java.util.concurrent.ThreadFactory;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Pattern;

import alma.acs.commandcenter.util.MiscUtils;


/**
 * Describes and encapsulates a native command. A native command
 * triggers a process, and provides various bells and whistles
 * around the pure process object as provided by the Java Runtime.
 * 
 * The native command can be run
 * a) within its own thread (as it implements Runnable), or 
 * b) directly via a call to its run() method.
 * 
 * @author mschilli 
 */
public class NativeCommand implements Runnable {

	//
	// ============= Constants ===============
	//

   // status constants
   public final static String NEW = "new";
   public final static String RUNNING = "running";
   public final static String TERMINATED = "terminated";
   public final static String CANNOTRUN = "unable to run";
   public final static String TIMEOUT = "timed out";

   // maxExecutionTime constant 
   public final static long NO_TIMEOUT = -1;

   
	//
	// ============= Members ===============
	//
   
   // time watcher sleeps between its iterations
   static public long DEFAULT_WATCHER_INTERVAL = 1000; //msecs

   // time before watcher threads start
   static public long DEFAULT_WATCHER_DELAY = 500; // msecs

   // there is 1 watcher for each command
   protected static Timer watchers;
   
   // keep a reference to the stdin of the process
   protected OutputStreamWriter stdin;

   // there are N listeners for each command
   protected Vector<NativeCommand.Listener> listeners = new Vector<NativeCommand.Listener>();

   // the actual description of a native command
   protected String command;
   protected Process process;
   protected long interval;
   protected long delay;
   protected long maxExecutionTime;
   protected String status;
   protected boolean foreground;
   protected String endMark;

   // results of execution
   protected Throwable latestException;
   protected Integer exitValue = null; // assigned if process terminates

   // logger
   protected Logger log;
   
   
   //
   // ================= Constructors ====================
   //

   public NativeCommand(String command, boolean foreground) {
      this(command, foreground, NO_TIMEOUT);
   }

   public NativeCommand(String command, boolean foreground, long maxExecutionTime) {
      this(command, foreground, maxExecutionTime, null);
   }

   public NativeCommand(String command, boolean foreground, long maxExecutionTime, String endMark) {
      this(command, foreground, maxExecutionTime, endMark, DEFAULT_WATCHER_INTERVAL, DEFAULT_WATCHER_DELAY);
   }

   public NativeCommand(String command, boolean foreground, long maxExecutionTime, String endMark, long interval, long delay) {
      this.command = command;
      this.foreground = foreground;
      this.maxExecutionTime = maxExecutionTime;
      this.endMark = endMark;
      this.interval = interval;
      this.delay = delay;

      this.status = NEW;
      
      this.log = MiscUtils.getPackageLogger(this);
   }

   
   
   //
   // ================= API ====================
   //

   /**
    * The default thread factory for background actions.
    */
   protected ThreadFactory threadFactoryDefault = new ThreadFactory(){
		public Thread newThread (Runnable r) {
			return new Thread(r);
		}
	};

   protected ThreadFactory threadFactory = threadFactoryDefault;

   /** 
    * This class executes various actions concurrently.
    * With this setter, clients can control the threads to be used.
    * @param threads - null for default
    */
   public void setThreadFactory (ThreadFactory threads) {
   	threadFactory = (threads == null)? threadFactoryDefault : threads;
   }


   
   public void addListener(NativeCommand.Listener po) {
      listeners.add(po);
   }

   public void removeListener(NativeCommand.Listener po) {
      listeners.remove(po);
   }

   public String getStatus() {
      return status;
   }

   /**
    * @return null    if no exit value due to ungraceful process death
    */
   public Integer getExitValue() {
      return exitValue;
   }

   /**
    * Returns the most recent occured error. Note that
    * the internal exception cache is reset by this method,
    * thus it can only be called once for each error.
    * @return
    */
   public Throwable getLatestException() {
      Throwable ret = latestException;
      latestException = null;
      return ret;
   }

   
   /**
    * Writes the given text to the <i>STDIN<i> of the process.
    * @param text the input to send to the process
    */
   public void send(String text) {
		try {
			// TODO(msc): don't log passwords !!!
			log.finer("writing command '"+text+"' to stdin of "+process);

			stdin.write(text+"\n");
			stdin.flush();
		
		} catch (IOException exc) {
			log.log(Level.FINE, "failed to write command '"+command+"' to stdin of "+process, exc);
		}
   }
   
   
   /**
    *  We use four delegates: <ol>
    *  <li> One to start a process
    *  <li> One to watch its progress
    *  <li> Two to read its output (out and err)
    * </ol>
    * 
    *  The delegates give feedback to the main thread by provoking InterruptedExceptions on it.
    */
   public void run() {

      Spawner deleg1 = new Spawner();
      deleg1.run();

      // nothing more to do in this case
      if (status.equals(CANNOTRUN)) {
         return;
      }

      // start a watcher for the process
      Watcher deleg2 = new Watcher();
      if (watchers == null)
         watchers = new Timer();
      watchers.schedule(deleg2, delay, interval);

      // start a delegate to read the process's output stream
      Reader deleg3 = new Reader(process.getInputStream());
      threadFactory.newThread(deleg3).start();

      // start a delegate to read the process's error stream
      Reader deleg4 = new Reader(process.getErrorStream());
      threadFactory.newThread(deleg4).start();

      // store a reference to the process's input stream
      stdin = new OutputStreamWriter(process.getOutputStream());
      /*stdin = new BufferedWriter(new OutputStreamWriter(process.getOutputStream()));*/
      
      
      // See, if we should suspend the current thread till the process terminates
      if (foreground) {

         // now send the current thread to sleep until something interesting happens
         // but order a wake up call before
         try {

            // Besides process termination, an expected piece of output can wake this thread.
            if (endMark != null) {
               deleg3.interruptThreadOnExpectedOutput(Thread.currentThread(), endMark);
               deleg4.interruptThreadOnExpectedOutput(Thread.currentThread(), endMark);
            }

            // Variant A
            // obviously, the process output is not yet completely flushed
            // at the time this call returns - so it's of limited use for us.
            // process.waitFor();

            // Variant B
            // thread.suspend() is deprecated, so we use this construct
            while (true) {
               deleg2.interruptThreadOnTaskTermination(Thread.currentThread());
               Thread.sleep(Long.MAX_VALUE);
            }

         } catch (InterruptedException e) {
            log.finer("Native command interrupted by InterruptedException");
         }
      }

   }

   //
   // ================= Internal ====================
   //
   
   /**
    */
   protected void changeStatus(String newStatus) {

   	// close the IO streams and thereby all three OS pipes
   	// ----------------------------------------------------
   	// http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4784692
   	// http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4523660
   	if (TERMINATED==newStatus) {
   		try {process.getOutputStream().close();} catch (Exception exc) {}
   		try {process.getErrorStream().close();} catch (Exception exc) {}
   		try {process.getInputStream().close();} catch (Exception exc) {}
   	}

   	String oldStatus = this.status;
      this.status = newStatus;
      
      fireStatusChanged (oldStatus, newStatus);
   }

   
   /**
    */
   protected void fireStatusChanged (String oldStatus, String newStatus) {

   	for (Enumeration<Listener> en = listeners.elements(); en.hasMoreElements();) {
         NativeCommand.Listener po = (NativeCommand.Listener) en.nextElement();
         po.statusChanged(this, oldStatus);
      }
   }
   
   
   /**
    */
   protected void fireOutputWritten (InputStream sourceStream, String additionalOutput) {
		if (additionalOutput == null || "".equals(additionalOutput)) {
			return; // ignore
		}
      boolean isOutputOnStdout = (sourceStream == process.getInputStream());

      for (Enumeration<Listener> en = listeners.elements(); en.hasMoreElements();) {
         NativeCommand.Listener po = (NativeCommand.Listener) en.nextElement();

         if (isOutputOnStdout)
            po.stdoutWritten(this, additionalOutput);
         else
            po.stderrWritten(this, additionalOutput);

      }
   }

   //
   // ================= Inner Types ====================
   //

   /**
    * Interested in Processes? Be a NativeCommand Listener today!
    */
   static public interface Listener {
      /**  */
      public void statusChanged(NativeCommand command, String oldStatus);
      
      /** @param additionalOutput    one or more lines of new output (last line will not contain a line terminator) */
      public void stdoutWritten(NativeCommand command, String additionalOutput);

      /** @param additionalOutput    one or more lines of new output (last line will not contain a line terminator) */
      public void stderrWritten(NativeCommand command, String additionalOutput);
   }

  /**
   * An empty implementation of the Listener interface.
   */ 
   static public class ListenerAdapter implements Listener {
      public void statusChanged(NativeCommand command, String oldStatus) {}
      public void stdoutWritten(NativeCommand command, String additionalOutput) {}
      public void stderrWritten(NativeCommand command, String additionalOutput) {}
   }
   
   /**
    * Runs a process. Very little code, but it's
    * prettier to have a dedicated class for it.
    */
   protected class Spawner {

      public void run() {
         try {

            process = Runtime.getRuntime().exec(command);
            changeStatus(RUNNING);

         } catch (IOException e) {
            latestException = e;
            changeStatus(CANNOTRUN);
         }
      }

   }

   
   /**
    * Reads a process's streams.
    * Can notify a suspended thread if a specified piece of output
    * (as regex) occurs.
    */
   protected class Reader implements Runnable {

   	protected Thread interruptableThread;
   	protected Pattern expectedOutput;

   	protected InputStream sourceStream;

   	protected Reader(InputStream sourceStream) {
         this.sourceStream = sourceStream;
      }

      /** @param thread  this parameter is actually redundant but makes things clearer */
      public void interruptThreadOnExpectedOutput(Thread thread, String expectedRegex) {
         this.interruptableThread = thread;
         this.expectedOutput = Pattern.compile(expectedRegex);
      }

      public void run() {
         try {
            BufferedReader sourceReader = new BufferedReader(new InputStreamReader(sourceStream));
            String line;

            // this will sleep until data becomes available
            while ((line = sourceReader.readLine()) != null) {
					// readLine() is smart: it detects any of "\n", "\r", "\r\n".
					// It will also strip off any of them. Thus we add
					// the unix line terminator again.
               fireOutputWritten(sourceStream, line+"\n");
               
               if (expectedOutput != null && expectedOutput.matcher(line).matches()) 
                  interruptableThread.interrupt();
            }

            // TODO(msc) read out standard error 

         } catch (IOException exc) {
         	// seems to happen when the process is willingly destroy()-ed
            log.fine("will stop reading from process output stream, an I/O error occurred: "+exc);
         }
      }

   }

   /**
    * Polls the process behavior and sends events
    * to Listeners if something interesting happens.
    */
   protected class Watcher extends TimerTask {

   	protected long startTime;
   	protected Thread interruptableThread;

      /** @param thread  actually redundant but makes things clearer */
      public void interruptThreadOnTaskTermination(Thread thread) {
         this.interruptableThread = thread;
      }

      @Override
		public void run() {

         // nothing more to observe in these cases
         if (status.equals(TERMINATED) || status.equals(CANNOTRUN)) {
         	this.cancel();
         }

         // save time of very first run
         if (startTime == 0)
            startTime = System.currentTimeMillis();

         // check if process return value is readable...
         try {
            int x = process.exitValue();

            // yes, now spread the news
            changeStatus(TERMINATED);
            exitValue = new Integer(x);
            if (interruptableThread != null)
               interruptableThread.interrupt();

         } catch (IllegalThreadStateException exc) {
            // ... if not, the process has not ended yet

            // if maxExecutionTime exists and
            // the process runs too long already, send event
            if (maxExecutionTime > NO_TIMEOUT && System.currentTimeMillis() - startTime > maxExecutionTime) {
               changeStatus(TIMEOUT);
            }
         }
      }
   }

}
