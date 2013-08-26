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
package alma.demo.LampCallbackImpl;

import java.util.logging.Level;

import alma.ACS.CBDescIn;
import alma.ACS.CBdouble;
import alma.ACS.CBdoubleHelper;
import alma.ACS.CBdoubleOperations;
import alma.ACSErr.CompletionHolder;
import alma.ACSErr.ErrorTrace;
import alma.ACS.Monitordouble;
import alma.ACS.RWdouble;
import alma.ACSErr.ACSException;
import alma.ACSErrTypeJavaNative.wrappers.AcsJJavaLangEx;
import alma.acs.component.ComponentImplBase;
import alma.acs.exceptions.AcsJException;
import alma.acsexmplLamp.Lamp;
import alma.acsexmplLamp.LampHelper;
import alma.demo.LampCallbackOperations;
import alma.demo.LampUnavailable;

/**
 * @author rgeorgie
 */
public class LampCallbackImpl extends ComponentImplBase implements LampCallbackOperations
{
	private Lamp m_lamp;

	private RWdouble m_brightness;

	private CBdoubleLampCallback m_cb;
	private CBDescIn m_desc;

	private Monitordouble m_monitor;

	private static final String m_lampCurl = "LAMP1";


	/////////////////////////////////////////////////////////////
	// Implementation of ComponentLifecycle
	/////////////////////////////////////////////////////////////

	public void cleanUp()
	{
		m_logger.info("cleanUp() called...");
		m_containerServices.releaseComponent(m_lampCurl, null);
	}

	/////////////////////////////////////////////////////////////
	// Implementation of LampCallbackOperations
	/////////////////////////////////////////////////////////////

	/**
	 * Attaches a monitor to the brightness object of the lamp component. 
	 * The component uses a callback when the interval set to the timer expires.
	 * @return double
	 * @see LampCallbackOperations#monitorLampBrightness()
	 * @throws alma.demo.LampUnavailable
	 */
	public double monitorLampBrightness() throws LampUnavailable
	{
		if (m_cb == null)
		{
			m_cb = new CBdoubleLampCallback(m_logger);
			m_logger.finer("CBdoubleLampCallback instantiated...");
		}

		CBdouble cbdouble;
		try
		{
			//test
//			CBdoubleHelper.narrow(m_containerServices.activateOffShoot(m_cb));
//			m_containerServices.deactivateOffShoot(m_cb);
			
			// note that m_cb may go through a cycle of activation/deactivation, see stopMonitor()
			cbdouble = CBdoubleHelper.narrow(m_containerServices.activateOffShoot(m_cb, CBdoubleOperations.class));
		}
		catch (Exception e)
		{
			m_logger.log(Level.SEVERE, "failed to obtain the callback offshoot object.", e);
			throw new LampUnavailable(e.getMessage());
		}

		if (m_desc == null)
		{
			m_desc = new CBDescIn();
			m_logger.info("m_desc instantiated...");
		}
		
		double brightness = 0;
		try
		{
			getLampBrightnessObject();
			
			CompletionHolder completionHolder = new CompletionHolder();
			brightness = m_brightness.get_sync(completionHolder);

			m_monitor = m_brightness.create_monitor(cbdouble, m_desc);
			m_logger.info("monitor instantiated...");

			// call every 10th second		
			m_logger.info("prepares to trigger brightness every 10th second...");
			m_monitor.set_timer_trigger(100000000);

			m_logger.info("ready to trigger brightness...");

		}
		catch (Exception ex)
		{
			m_logger.log(Level.SEVERE, "ex in monitorLampBrightness impl", ex);
			throw new LampUnavailable(ex.getMessage());
		}

		return brightness;
	}
	
	
	/**
	 * Stops the monitor upon request.
	 * @see alma.demo.LampCallbackOperations#stopMonitor()
	 * @throws alma.demo.LampUnavailable
	 */
	public void stopMonitor() throws LampUnavailable
	{
		if (m_monitor != null)
		{
			try
			{
				m_monitor.destroy();

				m_containerServices.deactivateOffShoot(m_cb);
				m_logger.finer("deactivated CBdoubleLampCallback offshoot CORBA object.");
}
			catch (Exception ex)
			{
				m_logger.log(Level.SEVERE, "ex in stopMonitor impl", ex);
				throw new LampUnavailable(ex.getMessage());
			}
		}
	}



	/**
	 * Throws a plain CORBA exception,
	 * that is, one that does not collaborate with the ACS error system.
	 * 
	 * @see alma.demo.LampCallbackOperations#exceptionMethod()
	 */
	public void exceptionMethod() throws LampUnavailable
	{
		throw new LampUnavailable("use me for container tests...");
	}



	public void acsExceptionMethodVoid() throws ACSException
	{
		try
		{
			// imagine here's some impl code that throws AcsJExceptions
			acsJExceptionMethod();
		}
		catch (AcsJException e)
		{
			// to the outside (CORBA) we must convert it
			ErrorTrace et = e.getErrorTrace();
			ACSException acsEx = new ACSException(et);
			throw acsEx;
		}
	}


	public double acsExceptionMethodDouble() throws ACSException
	{
		// this will throw the ex
		acsExceptionMethodVoid();
		
		// will never get here
		return -1.0;
	}


	private void acsJExceptionMethod() throws AcsJException
	{
		try
		{
			// imagine here's some code that throws exceptions
			Exception npe = new NullPointerException("mean NPE");
			throw npe;
		}
		catch (Exception ex)
		{
			// wrap that npe with an acs ex and throw it on
			AcsJException acsEx = new AcsJJavaLangEx("low level ex", ex);
			throw acsEx;
		}
	}


	/////////////////////////////////////////////////////////////
	// other
	/////////////////////////////////////////////////////////////	

	/**
	 * Getts the lamp component LAMP1.
	 * @return Lamp
	 * @throws java.lang.Exception
	 */
	public Lamp getLamp() throws LampUnavailable
	{
		if (m_lamp == null)
		{
			try
			{
				m_lamp = LampHelper.narrow(m_containerServices.getComponent(m_lampCurl));
			}
			catch (Exception e)
			{
				String msg = "failed to obtain the component " + m_lampCurl;
				m_logger.log(Level.SEVERE, msg, e);
				throw new LampUnavailable(msg);
			}
		}
		return m_lamp;
	}


	/**
	 * Gets the lamp component's brightness CORBA object as RWdouble.
	 * @return RWdouble
	 * @throws java.lang.Exception
	 */
	public RWdouble getLampBrightnessObject() throws LampUnavailable
	{
		if (m_brightness == null)
		{
			getLamp();
			try
			{
				m_brightness = m_lamp.brightness();
			}
			catch (Exception e)
			{
				String msg = "failed to obtain the lamp's brightness object";
				m_logger.log(Level.SEVERE, msg, e);
				throw new LampUnavailable(msg);
			}
		}
		return m_brightness;
	}
}

