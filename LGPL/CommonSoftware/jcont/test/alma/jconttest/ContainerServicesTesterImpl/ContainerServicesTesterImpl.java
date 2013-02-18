/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.jconttest.ContainerServicesTesterImpl;

import java.util.Random;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;

import org.omg.CORBA.StringHolder;

import alma.ACS.ACSComponentHelper;
import alma.ACS.stringSeqHolder;
import alma.ACSErrTypeCommon.CouldntPerformActionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.acs.component.ComponentDescriptor;
import alma.acs.component.ComponentImplBase;
import alma.acs.component.ComponentQueryDescriptor;
import alma.acs.component.ComponentStateManager;
import alma.acs.util.StopWatch;
import alma.jconttest.ContainerServicesTesterOperations;
import alma.jconttest.DummyComponent;
import alma.jconttest.DummyComponentImpl.DummyComponentHelper;



/**
 * A component whose methods can be used to test the behavior of the <code>ContainerServices</code> 
 * inside a container.
 *  
 * @author hsommer
 * created Nov 12, 2003 6:16:17 PM
 */
public class ContainerServicesTesterImpl extends ComponentImplBase implements ContainerServicesTesterOperations
{

	public boolean testComponentName(StringHolder compName)
	{
		String name = m_containerServices.getName();
		if (name != null) {
			compName.value = name;
			m_logger.info("this instance of " + ContainerServicesTesterImpl.class.getName() + 
					" runs under the name " + name );
			return true;
		}
		else {
			m_logger.warning("failed to retrieve instance name from ContainerServices!"); 
			compName.value = "<null>";
			return false;
		}
	}

	public boolean testStateManager(StringHolder currStateNameHolder)
	{
		ComponentStateManager compMgr = m_containerServices.getComponentStateManager();
		currStateNameHolder.value = compMgr.getName(compMgr.getCurrentState());
		return true;
	}

	/**
	 * @see alma.jconttest.ContainerServicesTesterOperations#testGetDynamicDummyComponent(org.omg.CORBA.StringHolder)
	 */
	public boolean testGetDynamicDummyComponent(StringHolder compName)
	{
		boolean ret = false;
		try
		{
			ComponentQueryDescriptor compQueryDesc = new ComponentQueryDescriptor();
			
//			compQueryDesc.setComponentName(ComponentQueryDescriptor.ANY);
			compQueryDesc.setComponentType(DummyComponentHelper.DUMMYCOMPONENT_CORBATYPE);
			
			org.omg.CORBA.Object compObj = m_containerServices.getDynamicComponent(compQueryDesc, false);
			DummyComponent dummyComp = alma.jconttest.DummyComponentHelper.narrow(compObj);
			
			// return the name that was dynamically assigned
			compName.value = dummyComp.name();			
			m_logger.info("received dynamic instance of " + DummyComponentHelper.DUMMYCOMPONENT_CORBATYPE);

			// now check the component descriptor
			ComponentDescriptor compDesc = m_containerServices.getComponentDescriptor(compName.value);
			m_logger.info("Component data: " + compDesc.toString());
			
			m_containerServices.releaseComponent(dummyComp.name());
			ret = true;
		}
		catch (Exception e)
		{
			// todo: IDL ex
			m_logger.warning(e.toString());
			
			// prevent CORBA marshalling ex
			compName.value = "!!!failed!!!";
		}
		return ret;
	}

	/**
	 * @see alma.jconttest.ContainerServicesTesterOperations#testGetReferenceWithCustomClientSideTimeout(String)
	 */
	public boolean testGetReferenceWithCustomClientSideTimeout() {
		boolean ret = true;
		try {
			String compName = "DefaultDummyComp";
			org.omg.CORBA.Object compObj = m_containerServices.getComponent(compName);
			DummyComponent comp = alma.jconttest.DummyComponentHelper.narrow(compObj);
			int waitTime = 18 ; //secs. (should be at most 10 s longer than 'timeout' 
								// because component deactivation blocks only 10 seconds on a still running operation. 
			int timeout = 10; // secs.
			// first let's try to call the IF without a timeout
			try {
				comp.callThatTakesSomeTime(waitTime * 1000);
			} catch (org.omg.CORBA.TIMEOUT e) {
				m_logger.log(Level.WARNING, "No TIMEOUT exception expected for a 20 second call, without applying a special client-side timeout.");
				ret = false;
			}

			// then we call the ContainerServices to assign a timeout of 10 seconds
			org.omg.CORBA.Object compObjWithTimeout = m_containerServices.getReferenceWithCustomClientSideTimeout(compObj, timeout);
			DummyComponent compWithTimeout = alma.jconttest.DummyComponentHelper.narrow(compObjWithTimeout);

			// we call again the IF
			StopWatch sw = new StopWatch(m_logger);
			try {
				compWithTimeout.callThatTakesSomeTime(waitTime * 1000);
				m_logger.log(Level.WARNING, "TIMEOUT exception expected after " + timeout + " seconds.");
				ret = false;
			} catch (org.omg.CORBA.TIMEOUT e) {
				m_logger.info("Good: got a TIMEOUT exception from DefaultDummyComp#callThatTakesSomeTime");
			}

			int elapsedTime = (int) sw.getLapTimeMillis() / 1000;
			if (Math.abs(elapsedTime - timeout) > 2) {
				ret = false;
				m_logger.log(Level.WARNING, "TIMEOUT exception caught as expected, but after " + elapsedTime + 
						" seconds instead of the expected " + timeout + " seconds.");
			}
			// the release call should block for the remaining 8 s that 'callThatTakesSomeTime' keeps running after our client timeout.
			m_containerServices.releaseComponent(compName);
		} catch (Exception ex) {
			m_logger.log(Level.WARNING, "Failure in testGetReferenceWithCustomClientSideTimeout, will return 'false'.", ex);
			ret = false;
		}
		return ret;
	}

	
	/**
	 * @see alma.jconttest.ContainerServicesTesterOperations#testGetDefaultIdentifierArchive(org.omg.CORBA.StringHolder)
	 */
	public boolean testGetDefaultIdentifierArchive(StringHolder compName)
	{
		compName.value = "not yet implemented...";
		return false;
	}

    /**
     * Tests the well-behaved usage of {@link alma.acs.container.ContainerServices#getThreadFactory()}. 
     * @return true if all was good.
     * @see alma.acs.container.CleaningThreadFactoryTest
     */
    public boolean testGetThreadFactory(int numThreads, final int busyLoopCount, final boolean randomize) {
        boolean ret = true;
        try {
            ThreadFactory threadFactory = m_containerServices.getThreadFactory();
            ExecutorService exec = Executors.newFixedThreadPool(numThreads, threadFactory);
            final Random random = new Random(System.currentTimeMillis());
            
            class testCmd implements Runnable {
                public long y;
                public void run() {
                    int max = randomize ? random.nextInt(busyLoopCount) : busyLoopCount;
                    for (int i=0; i < max; i++) {
                        y = i * (i-1);
                    }
                    m_logger.fine("Test thread '" + Thread.currentThread().getName() + "' done.");
                }        
            }
            // run up to maxThreadNumber new threads
            numThreads = randomize ? random.nextInt(numThreads) + 1 : numThreads;
            m_logger.fine("Will use the thread factory to run " + numThreads + " threads.");
            for (int i=0; i < numThreads; i++) {
                exec.execute(new testCmd());
            }
            exec.shutdown();
            try {
                ret = exec.awaitTermination(Long.MAX_VALUE, TimeUnit.NANOSECONDS);
            } catch (InterruptedException e) {
            	m_logger.log(Level.WARNING, "interrupted", e);
            }
        } catch (Exception e) {
            ret = false;
        }
        
        return ret;
    }
    

	public void testFindDummyComponentsByType(stringSeqHolder curls) throws alma.ACSErrTypeCommon.CouldntPerformActionEx {
		try {
			curls.value = m_containerServices.findComponents(null, "IDL:alma/jconttest/DummyComponent:1.0");
		} catch (Exception ex) {
			AcsJCouldntPerformActionEx ex2 = new AcsJCouldntPerformActionEx(ex);
			throw ex2.toCouldntPerformActionEx();
		}
	}
	
	public void testGetCollocatedComponent(String curl, String compType, String targetCurl) throws CouldntPerformActionEx {
		String msg = "component '" + curl + "' of type '" + compType +"' collocated with '" + targetCurl + "'";
		m_logger.info("Received call to testGetCollocatedComponent for " + msg);
		try {
			ComponentQueryDescriptor cqd = new ComponentQueryDescriptor(curl, compType);			
			org.omg.CORBA.Object compObj = m_containerServices.getCollocatedComponent(cqd, false, targetCurl);
			if (compObj == null) {
				throw new NullPointerException("Got null reference for " + msg);
			}
			// curl could be given as "*" or null for dynamic name assignment, therefore we ask the name() from the component
			m_containerServices.releaseComponent(ACSComponentHelper.narrow(compObj).name());
		} catch (Throwable thr) {
			throw (new AcsJCouldntPerformActionEx("testGetCollocatedComponent failed for " + msg, thr)).toCouldntPerformActionEx();
		}
	}

	
	public void testGetComponentNonSticky(String curl, boolean release) throws CouldntPerformActionEx {
		String msg = "component '" + curl + "'";
		m_logger.info("Received call to testGetComponentNonSticky for " + msg);
		try {
			org.omg.CORBA.Object compObj = m_containerServices.getComponentNonSticky(curl);
			if (compObj == null) {
				throw new NullPointerException("Got null reference for " + msg);
			}
			if (release) {
				// releasing a non-sticky component ref should be a no-op 
				m_containerServices.releaseComponent(curl);
			}
		} catch (Throwable thr) {
			throw (new AcsJCouldntPerformActionEx("testGetComponentNonSticky failed for " + msg, thr)).toCouldntPerformActionEx();
		}		
	}

}
