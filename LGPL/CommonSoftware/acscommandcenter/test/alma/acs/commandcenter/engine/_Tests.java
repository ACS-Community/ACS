/*
 * Created by mschilli on Dec 20, 2004
 */
package alma.acs.commandcenter.engine;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import junit.framework.AssertionFailedError;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import alma.acs.commandcenter.AccTests;
import alma.acs.util.AcsLocations;
import alma.entity.xmlbinding.acscommandcentertools.AcsCommandCenterTools;
import alma.entity.xmlbinding.acscommandcentertools.Tool;

/**
 * A TestSuite holding 
 * <ul>
 * <li>various UnitTests 
 * <li>other TestSuites
 * <li>Utilities for those tests
 * </ul>
 * 
 * @author mschilli
 */
public class _Tests {



	public static Test suite() {
		TestSuite ret = new TestSuite("Test for " + _Tests.class.getPackage().getName());

		if (AccTests.hasNativeAcs()) {
			ret.addTestSuite(AcsScriptsTest.class);

			if (AccTests.hasHead()) {
				ret.addTestSuite(ExternalToolsTest.class);
			}
		}
		
		ret.addTestSuite(ExecutorTest.class);
		
		return ret;
	}

	
	
	// ===================================================================
	// =======================  Test Utilities  ==========================
	// ===================================================================
	
	
	
	static protected void assertExistsInstance (String acsInstance) throws Throwable {
		String output = _Tests.runAcsList(acsInstance);
		if (output.equals("")) {
			throw new AssertionFailedError("Acs instance directory " + acsInstance + " doesn't exist but should");
		}
	}

	static protected void assertNotExistsInstance (String acsInstance) throws Throwable {
		/* PENDING (msc): Use other expected-output strings for ascStopX-commands?
		 * 
		 * Checking for existence of the directory instantly after one
		 * of the acsStopXXX-commands returns is unreliable. Sometimes
		 * the directory removal still takes a bit after the expected
		 * output (e.g. for acsStop: "Freeing...") comes
		 */
		try {
			Thread.sleep(1000);
		} catch (InterruptedException e) {}
	
		String output = _Tests.runAcsList(acsInstance);
		if (!output.equals("")) {
			throw new AssertionFailedError("Acs instance directory " + acsInstance + " exists but should not. acsList's output was: "+output);
		}
	}

	static protected String runAcsList (String acsInstance) throws Throwable {
		final String[] output = new String[]{""};
		NativeCommand.Listener listener = new NativeCommand.ListenerAdapter() {
	
			@Override
			public void stdoutWritten (NativeCommand task, String additionalOutput) {
				output[0] += additionalOutput;
			}
		};
		Executor.localOutProc("acsList -b " + acsInstance, true, -1L, null, listener);
		return output[0];
	}

	static protected Tool giveTool(String name) throws Exception {
		
		AcsCommandCenterTools tools = ToolManager.getExtraTools();
		// --- loop over all "tool" definitions
		for (int i = 0; i < tools.getToolCount(); i++) {
			Tool tool = tools.getTool(i);
			if (tool.getCaption().equalsIgnoreCase(name))
				return tool;
		}
		throw new AssertionFailedError("Extra Tool '"+name+"' is not contained in Tool config file");
	}

	static protected void enter (TestCase x) {
		String testclass = x.getClass().getName();
		testclass = testclass.substring(testclass.lastIndexOf('.')+1);
		String testmethod = x.getName();
		System.err.println("\n=== " + testmethod + " ("+testclass+") ===");
	}

	static protected void sleep (int msecs) {
		try {
			Thread.sleep(msecs);
		} catch (InterruptedException exc) {
		}
	}
	
	static protected ExecuteServices createExecuteServices (final String acsInstance) {
	
		RunModel m = new RunModelAdapter() {
	
			@Override
			public String getScriptBase () {
				return acsInstance;
			}
		};
	
		return new ExecuteServices(m);
	}

	static protected ExecuteManager createExecuteManager (final String acsInstance) {
	
		RunModel m = new RunModelAdapter() {
	
			@Override
			public String getScriptBase () {
				return acsInstance;
			}
		};
		return new ExecuteManager(m);
	}

	static protected ExecuteAcs createExecuteAcs (final String acsInstance) {
	
		RunModel m = new RunModelAdapter() {
	
			@Override
			public String getServicesLocalJavaRoot () {
				return null;
			}

			@Override
			public String getScriptBase () {
				return acsInstance;
			}
		};
		return new ExecuteAcs(m);
	}

	static protected RunModel createContainerRunModel (final String acsInstance, final String type, final String name) {
	
		RunModel m = new RunModelAdapter() {
	
			@Override
			public String getContainerType () {
				return type;
			}
	
			@Override
			public String getContainerScriptBase () {
				return acsInstance;
			}
	
			@Override
			public String getContainerName () {
				return name;
			}
	
		};
		return m;
	}

	static protected ExecuteTools createExecuteTools (final String acsInstance, final String acsHost, final String mgrPort, final String irPort, final String nsPort) {
	
		RunModel m = new RunModelAdapter() {
	
			@Override
			public String getScriptBase () {
				return acsInstance;
			}
			
			@Override
			public String getToolAgainstManagerHost () {
				return acsHost;
			}
	
			@Override
			public String getToolAgainstManagerPort () {
				return mgrPort;
			}
			
			@Override
			public String getToolAgainstInterfaceRepository () {
				return AcsLocations.convertToInterfaceRepositoryLocation(acsHost, irPort);
			}
	
			@Override
			public String getToolAgainstNameService () {
				return AcsLocations.convertToNameServiceLocation(acsHost, nsPort);
			}
	
		};
		return new ExecuteTools(m);
	}


   public static class LogWriter extends NativeCommand.ListenerAdapter {
   	
   }
	
   public static void main (String[] args) {
   	System.out.println(getScriptlogFile("testA").getAbsolutePath());
   	System.out.println(scriptlogDirectory);
   	System.out.println(scriptlogCount);
   	System.out.println(getScriptlogFile("testB").getAbsolutePath());
   	System.out.println(scriptlogDirectory);
   	System.out.println(scriptlogCount);
   }
   
   protected static File scriptlogDirectory;
   protected static int scriptlogCount = 0;
   
   private static File getScriptlogFile(String testname) {
   	
   	File ret = null;

   	if (scriptlogDirectory == null) {
			File scriptlogRoot = new File("./scriptlogs");
			
			int i=1;
			for (; i<100; i++) {
				File tryout = new File (scriptlogRoot, "run"+formatIndex(i)); 
				if (tryout.exists())
					continue;
				
				tryout.mkdir();
				scriptlogDirectory = tryout;
				break;
			}
			if (scriptlogDirectory == null) {
				throw new RuntimeException("couldn't create scriptlog/runXXX directory. if 100 scriptlogs/run directories already exist, clean up!");
			}
   	}
   	
   	scriptlogCount +=1 ;
		ret = new File(scriptlogDirectory, formatIndex(scriptlogCount) + "_" + testname); 
		
		return ret;
   }

   private static String formatIndex (int index) {
   	String idx = String.valueOf(index);
   	return "00".substring(0, 2 - idx.length()) + idx;
   }
   
	/**
	 * @return
	 */
	protected static LogWriter giveTaskListener (String testname) {

		
		// --- create requested listener
		
		try {
			File f = getScriptlogFile(testname);
			System.err.println(" see "+f.getName());
			final FileWriter writer = new FileWriter(f);
			return new LogWriter() {

				@Override
				public void stdoutWritten (NativeCommand task, String additionalOutput) {
					try {
						if (! (additionalOutput.endsWith("\r") || additionalOutput.endsWith("\n") )) {
							additionalOutput += "\n"; 
						}
						writer.write(additionalOutput);
						writer.flush();
					} catch (IOException exc) {}
				}

				@Override
				public void stderrWritten (NativeCommand task, String additionalOutput) {
					try {
						if (! (additionalOutput.endsWith("\r") || additionalOutput.endsWith("\n") )) {
							additionalOutput += "\n"; 
						}
						writer.write(additionalOutput);
						writer.flush();
					} catch (IOException exc) {}
				}
			};

		} catch (IOException exc) {
			return null;
		}
	}

}






