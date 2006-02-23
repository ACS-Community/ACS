/*
 * Created on 25.10.2003
 *  
 */
package alma.acs.commandcenter;

import java.awt.Rectangle;
import java.io.File;
import java.io.PrintStream;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.logging.Logger;

import alma.acs.commandcenter.app.CommandCenterLogic;
import alma.acs.commandcenter.app.CommandCenterLogic.StartupOptions;
import alma.acs.commandcenter.engine.Executor;
import alma.acs.commandcenter.util.MiscUtils;

/**
 * Launches AcsCommandCenter.
 * 
 * @author mschilli
 *  
 */
public class CommandCenter {

	private static Logger log = MiscUtils.getPackageLogger(CommandCenter.class);

   private static CommandCenterLogic commandCenterLogic;
   
	public static void main(String[] args) {

		// --- parse the command line
      
      StartupOptions startupOptions = new StartupOptions();

      for (int i=0; i<args.length; i++) {
      	//System.out.println(i+": "+args[i]);
      	try {

				if (equalsOneOf(args[i], new String[]{"-r", "-retrieve", "--retrieve"})) {
					startupOptions.project = new File(args[++i]);
					continue;
				}

				if (equalsOneOf(args[i], new String[]{"-g", "-geometry", "--geometry"})) {
			   	StringTokenizer toky = new StringTokenizer(args[++i], "x+");
			   	int w = Integer.parseInt(toky.nextToken());
			   	int h = Integer.parseInt(toky.nextToken());
			   	int x = Integer.parseInt(toky.nextToken());
			   	int y = Integer.parseInt(toky.nextToken());
			   	startupOptions.geometry = new Rectangle(x, y, w, h);
			   	continue;
				}

				if (equalsOneOf(args[i], new String[]{"-x", "-noexit", "--noexit"})) {
			   	startupOptions.doExitOnClose = false;
			   	continue;
				}
				
				if (equalsOneOf(args[i], new String[]{"-useNativeSSH", "--useNativeSSH"})) {
			   	startupOptions.useNativeSSH = true;
			   	continue;
				}

				if (equalsOneOf(args[i], new String[]{"-killNativeSSH", "--killNativeSSH"})) {
			   	startupOptions.killNativeSSH = true;
			   	continue;
				}

				// msc (Oct 24, 2005): stand-alone argument should be considered a project name
				startupOptions.project = new File(args[i]);
				
      	} catch (Exception exc) { // ArrayIndexOutOfBounds, NumberFormat, ...
				// since we don't see what exactly failed,
				// we ignore the complete command line 
      		startupOptions = new StartupOptions();
            log.warning("command line argument(s) invalid, all arguments will be ignored: "+Arrays.asList(args));
            printUsage(System.err);
	      }
      }

      
      // --- evaluate some pieces of the options

      if (startupOptions.useNativeSSH) {
			System.setProperty(Executor.SYSPROP_USE_NATIVE_SSH, Boolean.toString(true));
      }

      if (startupOptions.killNativeSSH) {
			System.setProperty(Executor.SYSPROP_KILL_NATIVE_SSH, Boolean.toString(true));
      }
      
      
      // --- instantiate appropriate logic

      // A) normal Gui operations

      commandCenterLogic = new CommandCenterLogic();
      commandCenterLogic.prepare(startupOptions);

      if (startupOptions.project != null) {
         commandCenterLogic.loadProject(startupOptions.project);
      }

      commandCenterLogic.go();

      // B) something else... ;)

	}

	/**
	 * Whether <code>a</code> equals one of the elements of <code>b</code>.
	 * Comparison is case-INsensitive.
	 */
	private static boolean equalsOneOf(String a, String[] bb) {
		for (int i=0; i < bb.length; i++)
			if (bb[i].equalsIgnoreCase(a))
				return true;
		return false;
	}
	
	
   private static void printUsage(PrintStream s) {
   	String msg = "" +
            "Usage: (this) [OPTIONS]\n" +
            "Options:\n" +
            "-r | -retrieve | --retrieve PROJ                      load a project file on startup\n" +
   			"-g | -geometry | --geometry WIDTHxHEIGHT+XPOS+YPOS    size and location of window\n" +
   			"-x | -noexit   | --noexit                             don't exit the JVM when quitting\n";

      s.println(msg);
   }
	

	/**
	 * System.exit() can be prevented by setting the boolean flag to false through
	 * the corresponding command line switch.
	 */
	public static void exit(final int code) {
		log.fine("requested to exit with exit code '" + code + "'");
      
		if (commandCenterLogic.startupOptions.doExitOnClose) {

			// trying to tear down a hanging in-process Acs
         // can freeze the whole application.
         // thus, we use a watchdog to ensure termination.
         new Thread(){public void run(){
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
   //
   // =============================
   //
   
   public static void dbg_printProps(Properties p) {
      dbg_printProps(p, System.err);
   }

   public static void dbg_printProps(Properties p, String filesuffix) {
   	PrintStream s = null;
      try {
         java.io.File f = java.io.File.createTempFile(filesuffix, ".tmp");
      	s = new java.io.PrintStream(new java.io.FileOutputStream(f));
         System.err.println("CommandCenter.dbg_printProps: writing props to "+f.getAbsolutePath());
      }catch (java.io.IOException exc) {
      	System.err.println("CommandCenter.dbg_printProps: could not open print stream to a tmp file; writing to stderr");
      	s = System.err;
      }
      dbg_printProps(p, s);
      s.close();
   }
   
   public static void dbg_printProps(Properties p, PrintStream s) {
      
      Set e1 = p.entrySet();
      Object[] e2 = e1.toArray();
      Comparator c = new Comparator() {
         public int compare(Object a, Object b) {
            return ((String)((Map.Entry)a).getKey()).compareTo((String)((Map.Entry)b).getKey());
         }
      };
      Arrays.sort(e2, c);

      
      s.println("------------ (time now is: "+System.currentTimeMillis()+")");
      for (int i=0; i<e2.length; i++) {
         Map.Entry next = (Map.Entry)e2[i];
         s.println(next.getKey()+"="+next.getValue());
      }
      s.println("------------");
      s.flush();
   }
   
   
   
}



