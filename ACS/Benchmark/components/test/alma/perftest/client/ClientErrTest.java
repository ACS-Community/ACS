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
package alma.perftest.client;

import java.util.logging.Logger;
import alma.acs.component.client.ComponentClient;
import alma.acs.time.Profiler;

import alma.perftest.ErrTestComponent;
import alma.perftest.ErrTestComponentHelper;


public class ClientErrTest extends ComponentClient
{   
    private Profiler m_profiler = null;
    
    private String m_compName = null;
    private long m_numInvocations = 0;
    private String m_msg = "";
    private int m_depth = 0;
    private boolean m_err = false;
    
    public ClientErrTest(Logger logger, String managerLoc)
	throws Exception 
	{
	    super(logger, managerLoc, "no client name");
	    m_profiler = new Profiler();
	}
    
    public void doSomeStuff() 
	throws Exception
	{
	    ErrTestComponent tComp = ErrTestComponentHelper.narrow(getContainerServices().getComponent(m_compName));

	    for (long i=0; i<m_numInvocations; i++)
		{
		m_profiler.start();
		tComp.testExceptions(m_depth, m_err);
		m_profiler.stop();
		}

	    m_profiler.fullDescription(m_msg);
	}
    
    public static void main(String[] args) 
	{
	    String managerLoc = System.getProperty("ACS.manager");
	    ClientErrTest hlc = null;
	    
	    
	    try 
		{
	    	hlc = new ClientErrTest(null, managerLoc);

		//Setup needed stuff
	    	hlc.m_compName = args[0];
		hlc.m_numInvocations = Long.parseLong(args[1]);
		hlc.m_depth = Integer.parseInt(args[2]);
		hlc.m_err = Boolean.valueOf(args[3]).booleanValue();
		for(int i = 4; i < args.length; i++)
		    {
		    if (i!=4)
			{
			hlc.m_msg = hlc.m_msg + " " + args[i];
			}
		    else
			{
			hlc.m_msg = args[i];
			}
		    }

	    	hlc.doSomeStuff();
		}
	    catch (Exception e) 
		{
		e.printStackTrace(System.err);
		}
	    finally 
		{
		if (hlc != null) 
		    {
		    try 
			{
			hlc.tearDown();
			}
		    catch (Exception e1) 
			{
			// bad luck
			}
		    }
		}
	}
}
