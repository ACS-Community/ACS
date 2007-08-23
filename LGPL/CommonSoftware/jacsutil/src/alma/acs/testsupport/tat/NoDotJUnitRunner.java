package alma.acs.testsupport.tat;

import junit.framework.Test;
import junit.framework.TestResult;
import junit.textui.ResultPrinter;
import junit.textui.TestRunner;

/**
 * Variant of {@link junit.textui.TestRunner} that does not print a "." for every test.
 * Using this class can be useful in conjunction with TAT, because these dots sometimes
 * show up in the output and sometimes they don't (unknown hidden parameters of TAT??),
 * which forces us to change the ref files unnecessarily.
 * 
 * @author hsommer
 */
public class NoDotJUnitRunner extends TestRunner {

	public NoDotJUnitRunner() {
		super();
		setPrinter(createResultPrinter());
	}	
	
	/**
	 * Factory method for result printer. 
	 * Could be overridden by a test that needs a more specialized custom ResultPrinter 
	 */
	protected ResultPrinter createResultPrinter() {
		return new ResultPrinter(System.out) {
			public void startTest(Test test) {
				// do not print the damn "." !
			}			
		};
	}
	
	/**
	 * Copied from base class TestRunner, except that a NoDotJUnitRunner gets created.
	 */
	public static void main(String args[]) {
		NoDotJUnitRunner aTestRunner= new NoDotJUnitRunner();
		try {
			TestResult r= aTestRunner.start(args);
			if (!r.wasSuccessful()) 
				System.exit(FAILURE_EXIT);
			System.exit(SUCCESS_EXIT);
		} catch(Exception e) {
			System.err.println(e.getMessage());
			System.exit(EXCEPTION_EXIT);
		}
	}

}
