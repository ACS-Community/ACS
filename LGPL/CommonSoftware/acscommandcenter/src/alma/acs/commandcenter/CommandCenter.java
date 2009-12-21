/*
 * Created on 25.10.2003
 *  
 */
package alma.acs.commandcenter;

import java.awt.Rectangle;
import java.io.File;
import java.io.PrintStream;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Date;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.logging.ConsoleHandler;
import java.util.logging.Formatter;
import java.util.logging.Handler;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import alma.acs.commandcenter.app.CommandCenterLogic;
import alma.acs.commandcenter.app.CommandCenterLogic.StartupOptions;
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

		// --- tweak console log format when started from command line 
		
		for (Handler h: Logger.getLogger("").getHandlers()) {
			if (h instanceof ConsoleHandler) {
				h.setFormatter(new Formatter(){
					DateFormat df = new SimpleDateFormat("HH:mm:ss ");
					@Override public String format (LogRecord record) {
						String s = df.format(new Date(record.getMillis()));
						s += record.getMessage() + "\n";
						return s;
					}
				});
			}
		}
		
		
		
		// --- parse the command line
      
      StartupOptions startupOptions = new StartupOptions();

      for (int i=0; i<args.length; i++) {

      	if (i==0 && equalsOneOf(args[i], new String[]{"-h", "-help", "--help"})) {
      		printUsage(System.out);
      		return;
      	}
      	
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
					log.warning("command line option '-useNativeSSH' no longer supported.");
			   	continue;
				}

				if (equalsOneOf(args[i], new String[]{"-killNativeSSH", "--killNativeSSH"})) {
					log.warning("command line option '-killNativeSSH' no longer supported.");
			   	continue;
				}

				// msc (Oct 24, 2005): stand-alone argument should be considered a project name
				// msc (Apr 28, 2006): alternatively it could be a manager location
				String standalone = args[i];
				
				if (standalone.length()>8 && standalone.substring(0, 8).equalsIgnoreCase("corbaloc")) {
					startupOptions.manager = standalone;
					continue;
				}
				String mgrArg = MiscUtils.convertShortNotationToCorbaloc(standalone);
				if (mgrArg != null) {
					startupOptions.manager = mgrArg;
					continue;
				}
				startupOptions.project = new File(standalone);

				
      	} catch (Exception exc) { // ArrayIndexOutOfBounds, NumberFormat, ...
      		startupOptions = new StartupOptions();
            log.warning("command line argument(s) invalid, some arguments may be ignored: "+Arrays.asList(args));
            printUsage(System.err);
	      }
      }

      
      
      // --- instantiate appropriate logic

      commandCenterLogic = new CommandCenterLogic();
      commandCenterLogic.prepare(startupOptions);

      if (startupOptions.project != null) {
         commandCenterLogic.loadProject(startupOptions.project);
      }
      
      commandCenterLogic.go();

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
		      "Usage: (this) [ARGUMENTS] [OPTIONS]\n" +
		      "Arguments:\n" +
		      " CORBALOC                                              instantly connect to existing Acs\n" +
		      " HOST:INSTANCE                                         instantly connect to existing Acs\n" +
		      " PROJ                                                  load a project file on startup\n" +
            "Options:\n" +
            " -r | -retrieve | --retrieve PROJ                      load a project file on startup\n" +
   			" -g | -geometry | --geometry WIDTHxHEIGHT+XPOS+YPOS    size and location of window\n" +
   			" -x | -noexit   | --noexit                             don't exit the JVM when quitting\n";

      s.println(msg);
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
   
   
   @SuppressWarnings("unchecked")
	public static void dbg_printProps(Properties p, PrintStream s) {
      
      Set<Map.Entry<Object, Object>> e1 = p.entrySet();
      Map.Entry[] e2 = e1.toArray(new Map.Entry[e1.size()]);
      Comparator<Map.Entry> c = new Comparator<Map.Entry>() {
         public int compare(Map.Entry a, Map.Entry b) {
            return ((String)a.getKey()).compareTo((String)b.getKey());
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



