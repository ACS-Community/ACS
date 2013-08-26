/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory,
 * 2002 Copyright by ESO (in the framework of the ALMA collaboration), All
 * rights reserved
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation; either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
package alma.test.corbareftest;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.component.client.ComponentClient;
import alma.CorbaRefTest.HelloWorld;
import alma.CorbaRefTest.HelloWorldHelper;

/**
 * Client application that accesses the HelloWorld components.  Shamelessly copied
 * from the HelloDemoClient code.
 * 
 * @author hsommer Nov 21, 2002 5:53:05 PM
 */
public class JavaClient extends ComponentClient
{
	private HashMap<String,HelloWorld> m_helloComps;

	/**
	 * @param logger
	 * @param managerLoc
	 * @param clientName
	 * @throws Exception
	 */
	public JavaClient(Logger logger, String managerLoc, String clientName)
			throws Exception {
		super(logger, managerLoc, clientName);
		m_helloComps = new HashMap<String,HelloWorld>();
	}

// 	/**
// 	 * Calls sayHello() on the hello component.
// 	 * @throws AcsJContainerServicesEx 
// 	 */
// 	public void doSomeStuff() throws AcsJContainerServicesEx {
//         m_helloComp = HelloWorldHelper.narrow(getContainerServices().getComponent("HELLODEMO1"));
        
// 		String helloRet = m_helloComp.sayHello();
// 		m_logger.info("got string from component's sayHello method: "
// 				+ helloRet);
// 	}
	/**
	 * Client processing operation
	 */
	public void doOperations(){
	    String instr;
	    BufferedReader stdin = new BufferedReader(new InputStreamReader(System.in));
	    boolean done = false;

	    try {
	    	System.out.println("Ready");
	    	while (!done) {  
	    		instr = stdin.readLine();
	    		String[] words = instr.split(" ");
//	    		for (String w : words)
//	    			System.out.println(w);
	    		if (words[0].equals("Done")) {
	    			done = true;
	    		}
	    		else if (words[0].equals("Load")) {
	    			String lgood = new String("");
	    			String lbad = new String("");
	    			
	    			String[] compnames = new String[words.length-1];
	    			System.arraycopy(words, 1, compnames, 0,words.length-1);
		    		for (String w : compnames) {
		    			try {
		    				m_helloComps.put(w,HelloWorldHelper.narrow(getContainerServices().getComponent(w)));
		    				lgood += w;
		    				lgood += " ";
		    			} catch (Exception e) {
		                    e.printStackTrace(System.err);
		    				lbad += w;
		    				lbad += " ";
		    			}
		    		}
		    		System.out.print("Load Complete: ");
		    		System.out.print(lgood);
		    		System.out.print(" Failed with Exception: ");
		    		System.out.println(lbad);
	    		}
	    		else if (words[0].equals("Call")) {
	    			String cgood = new String("");
	    			String cbad = new String("");
	    			
		    		for (String w : m_helloComps.keySet()) {
		    			try {
		    				m_helloComps.get(w).displayMessage();
		    				cgood += w;
		    				cgood += " ";
		    			} catch (Exception e) {
		                    e.printStackTrace(System.err);
		    				cbad += w;
		    				cbad += " ";
		    			}
		    		}
		    		System.out.print("Call Success: ");
		    		System.out.print(cgood);
		    		System.out.print(" Failed with Exception: ");
		    		System.out.println(cbad);	    		}
	    	}
			System.out.println("Bye");
		} catch (java.io.IOException e) {
	        System.out.println("Error in IO");
	    }
		return;
	}
	
	/**
	 * Checks whether the Java property 'ACS.manager' is set and calls the
	 * other methods from this class.
	 */
	public static void main(String[] args) {
 		String managerLoc = System.getProperty("ACS.manager");
 		if (managerLoc == null) {
 			System.out.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
 			System.exit(-1);
 		}
 		String clientName = "JavaClient";
 		JavaClient jc = null;
 		try {
 			jc = new JavaClient(null, managerLoc, clientName);
 			jc.doOperations();
 		}
 		catch (Exception e) {
             try {
                 Logger logger = jc.getContainerServices().getLogger();
                 logger.log(Level.SEVERE, "Client application failure", e);
             } catch (Exception e2) {
                 e.printStackTrace(System.err);
             }
 		}
 		finally {
 			if (jc != null) {
 				try {
 					jc.tearDown();
 				}
 				catch (Exception e3) {
 					// bad luck
                     e3.printStackTrace();
 				}
 			}
 		}
	}
}
