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
package alma.demo.client;

import java.util.logging.Logger;

import alma.acs.component.client.ComponentClient;

import alma.acstime.Timer;
import alma.acstime.Clock;
import alma.acstime.TimeoutHandlerPOA;
import alma.ACSErr.CompletionHolder;

public class TimeoutHandlerClient extends ComponentClient
{
    /**
     * Client application that creates a timeout using the ACS Time System.
     * This is done by subclassing the TimeoutHandler (IDL interface) and registering
     * an instance of this class with a Timer component.
     *
     * @author dfugate Mar 9, 2004 5:53:05 PM
     */
    private class TimeoutHandlerImpl extends TimeoutHandlerPOA
    {
	/**
	 * Invoked whenever the actual timeout occurs.
	 * @param e The current time(out).
	 */
	public void handleTimeout(alma.acstime.Epoch e)
	    {
		System.out.println("The current time is: " + e.value);
	    }
    }
    
    /**
     * Timer object is responsbile for scheduling timeouts.
     */
    private Timer m_timer = null;
    /**
     * Clock object gives the current time.
     */
    private Clock m_clock = null;

    /**
     * @param logger
     * @param managerLoc
     * @param clientName
     * @throws Exception
     */
    public TimeoutHandlerClient(Logger logger, String managerLoc, String clientName)
	throws Exception
	{
	    super(logger, managerLoc, clientName);
	    //Get the standard Clock component
	    m_clock = alma.acstime.ClockHelper.narrow(getContainerServices().getComponent("CLOCK1"));
	    //Get the standard Timer component
	    m_timer = alma.acstime.TimerHelper.narrow(getContainerServices().getComponent("TIMER1"));
	}
    
    public void doSomeStuff()
	throws Exception
	{
	    //Schedule the timeout for now + three seconds.
	    alma.ACS.ROuLongLong now = m_clock.now();
	    CompletionHolder completionHolder = new CompletionHolder();
	    alma.acstime.Epoch start = new alma.acstime.Epoch(now.get_sync(completionHolder));
	    start.value = start.value + 3000000;
	    
	    //Timeout only occurs once
	    alma.acstime.Duration period = new alma.acstime.Duration(0);
	    
	    //Create timeout handler and schedule its timeout
	    TimeoutHandlerImpl myHandler = new TimeoutHandlerImpl();
	    //The TimeoutHandler IDL interface is derived from ACS::OffShoot so 
	    //the container services can be used to activate it as a CORBA object
	    alma.acstime.TimeoutHandler handlerCORBARef = alma.acstime.TimeoutHandlerHelper.narrow(getContainerServices().activateOffShoot(myHandler));
	    //Schedule the timeout with the Timer device. Although we save
	    //the timeout ID in a variable, there is really no point since
	    //this is a one-shot timeout that should not be cancelled.
	    int id1 = m_timer.schedule(handlerCORBARef, 
				       start, 
				       period);
	}
	
	/**
	 * Checks whether the Java property 'ACS.manager' is set and calls the other methods from this class.
	 */
	public static void main(String[] args)
	    throws Exception
	{
	    String managerLoc = System.getProperty("ACS.manager");
	    if (managerLoc == null)
		{
		System.out.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
		System.exit(-1);
		}
	    
	    String clientName = "TimeoutHandlerClient";
	    
	    
	    final TimeoutHandlerClient lcbcl = new TimeoutHandlerClient(null, managerLoc, clientName);
	    //Schedule the timeout
	    lcbcl.doSomeStuff();
	    //Give it 20 seconds to occur (should only take 3 seconds).
	    Thread.sleep(20000);
	    System.out.println("20 sec waiting over, will tear down the show...");
	    lcbcl.tearDown();
	}
}
