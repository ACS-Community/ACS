/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
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
