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
import java.util.logging.Level;

import org.omg.CORBA.StringHolder;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;

import alma.ACSErrTypeCommon.CouldntPerformActionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.acs.component.ComponentDescriptor;
import alma.acs.component.ComponentImplBase;
import alma.acs.component.ComponentQueryDescriptor;
import alma.acs.component.ComponentStateManager;
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
    

	public void testGetCollocatedComponent(String curl, String targetCurl) throws CouldntPerformActionEx {
		String msg = "component '" + curl + "' collocated with '" + targetCurl + "'";
		m_logger.info("Received call to testGetCollocatedComponent for " + msg);
		try {
			org.omg.CORBA.Object compObj = m_containerServices.getCollocatedComponent(curl, targetCurl);
			if (compObj == null) {
				throw new NullPointerException("Got null reference for " + msg);
			}
			m_containerServices.releaseComponent(curl);
		} catch (Throwable thr) {
			throw (new AcsJCouldntPerformActionEx("testGetCollocatedComponent failed for " + msg, thr)).toCouldntPerformActionEx();
		}		
	}

	
	public void testGetComponentNonSticky(String curl) throws CouldntPerformActionEx {
		String msg = "component '" + curl + "'";
		m_logger.info("Received call to testGetComponentNonSticky for " + msg);
		try {
			org.omg.CORBA.Object compObj = m_containerServices.getComponentNonSticky(curl);
			if (compObj == null) {
				throw new NullPointerException("Got null reference for " + msg);
			}
			m_containerServices.releaseComponent(curl);
		} catch (Throwable thr) {
			throw (new AcsJCouldntPerformActionEx("testGetCollocatedComponent failed for " + msg, thr)).toCouldntPerformActionEx();
		}		
	}

	
	public void testForceReleaseComponent(String curl, boolean getOwnRef) throws CouldntPerformActionEx {
		try {
			if (getOwnRef) {
				m_containerServices.getComponent(curl);
			}
			m_containerServices.getAdvancedContainerServices().forceReleaseComponent(curl); 
		} catch (Throwable thr) {
			m_logger.log(Level.WARNING, "testForceReleaseComponent failed", thr);
			throw (new AcsJCouldntPerformActionEx("testForceReleaseComponent failed", thr)).toCouldntPerformActionEx();
		}
	}


}
