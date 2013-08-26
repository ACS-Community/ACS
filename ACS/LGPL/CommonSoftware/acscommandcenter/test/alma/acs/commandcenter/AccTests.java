/*
 * Created by mschilli on Dec 20, 2004
 */
package alma.acs.commandcenter;

import java.awt.GraphicsEnvironment;

import junit.framework.AssertionFailedError;
import junit.framework.Test;
import junit.framework.TestSuite;

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
public class AccTests {

	/* $Id: AccTests.java,v 1.5 2007/10/22 14:13:37 mschilli Exp $ */


	public static Test suite() {
		TestSuite ret = new TestSuite("Test for " + AccTests.class.getPackage().getName());

		ret.addTest(alma.acs.commandcenter.app._Tests.suite());
		ret.addTest(alma.acs.commandcenter.engine._Tests.suite());
		ret.addTest(alma.acs.commandcenter.util._Tests.suite());
		
		return ret;
	}
	
	
	// ===================================================================
	// =======================  Test Utilities  ==========================
	// ===================================================================
	

	/**
	 * @return Whether graphical features can be tested.
	 */
	static public boolean hasHead() {
		return !GraphicsEnvironment.isHeadless();
	}

	/**
	 * @return Whether native Acs scripts can be tested.
	 */
	static public boolean hasNativeAcs() {
		return ( System.getProperty("os.name").indexOf("Windows") == -1);
	}
	
	
	/**
	 * A wrapper for a task.
	 */
	static public abstract class AssertableTask extends Thread {
	
		String desc;
		Throwable err;
	
		public AssertableTask(String description) {
			this.desc = description;
		}
	
		@Override
		public void run () {
			try {
				System.err.println(" + '" + desc + "' about to execute");
				action();
				System.err.println(" - '" + desc + "' completed, no exception was thrown");
			} catch (Throwable t) {
				this.err = t;
			}
		}
	
		public abstract void action () throws Throwable;
	}


	
	/*
	 * Forwards to its companion of the same name, with a 
	 * pre-defined timeout. See there for more info.  
	 *
	static public void assertCompletion (AssertableTask t) throws Throwable {
		assertCompletion (30, t);
	} 
	 */
	
	/**
	 * Asserts that <code>t</code> terminates without errors within the given time limit.
	 * <p>
	 * This runs <code>t</code>. If <code>t</code> takes longer than <code>secs</code>,
	 * an <code>Exception</code> will be thrown. If <code>t</code> terminates with an
	 * error, it will be rethrown.
	 * </p>
	 */
	static public void assertCompletion (int secs, AssertableTask t) throws Throwable {
		long start = System.currentTimeMillis();
		t.start();
	
		do {
			try {
				t.join(secs * 1000);
			} catch (InterruptedException e) {}
		} while (t.isAlive() && (System.currentTimeMillis() - start) / 1000 < secs);
	
		if (t.isAlive()) {
			Error e = new AssertionFailedError("'" + t.desc + "' has not finished within " + secs + " seconds");
			System.err.println("  (failure... "+e.getMessage()+")");
			throw e; 
		}
			
		if (t.err != null) {
			System.err.println("  (exception... "+t.err.getMessage()+")");
			throw t.err;
		}
	}

}