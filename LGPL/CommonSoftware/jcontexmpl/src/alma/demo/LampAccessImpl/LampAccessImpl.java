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
package alma.demo.LampAccessImpl;

import java.util.logging.Level;

import alma.ACS.CBDescIn;
import alma.ACS.CBvoid;
import alma.ACSErr.CompletionHolder;
import alma.acs.container.ContainerServices;
import alma.ACS.RWdouble;
import alma.acs.component.ComponentImplBase;
import alma.acs.component.ComponentLifecycleException;
import alma.acsexmplLamp.Lamp;
import alma.acsexmplLamp.LampHelper;
import alma.demo.LampAccessOperations;
import alma.demo.LampUnavailable;


public class LampAccessImpl extends ComponentImplBase implements LampAccessOperations
{
	private RWdouble m_brightness;
	private CBvoidLampAccess m_cb;
	private CBvoid m_cbvoid;
	private CBDescIn m_desc;
	private static final String m_lampCurl = "LAMP1";

	/////////////////////////////////////////////////////////////
	// Implementation of ComponentLifecycle
	/////////////////////////////////////////////////////////////

	public void initialize(ContainerServices containerServices) throws ComponentLifecycleException
	{
                super.initialize(containerServices);
		m_logger.finer("initialize() called...");

		try
		{
			getLampBrightnessProperty();
		}
		catch (Exception e)
		{
			throw new ComponentLifecycleException(
			"failed to get reference to 'brightness' " + 
			"property of the lamp component.", e);
		}
	}

	/**
	 * Releases the lamp component.
	 * @see alma.acs.component.ComponentLifecycle#cleanUp()
	 */
	public void cleanUp()
	{
		m_logger.info("cleanUp() called...");
		m_containerServices.releaseComponent(m_lampCurl);
	}

	/////////////////////////////////////////////////////////////
	// Implementation of LampAccessOperations
	/////////////////////////////////////////////////////////////
	
	/**
	 * Passes a value to the brightness property of the lamp component. 
	 * @param brightness
	 * @see alma.demo.LampAccessOperations#setLampBrightness(double)
	 * @throws LampUnavailable
	 */
	public void setLampBrightness(double brightness) throws LampUnavailable
	{
		m_logger.finer("LampAccess:setLampBrightness(" + brightness + ") called...");
		
		if (m_brightness == null)
		{
			m_brightness = getLampBrightnessProperty();
			m_logger.info("m_brightness initialized...");
		}
		
		if (m_cb == null)
		{
			try
			{
				m_cb = new CBvoidLampAccess(m_logger);
				m_logger.info("CBvoidLampAccess instantiated...");
				m_cbvoid = alma.ACS.CBvoidHelper.narrow(m_containerServices.activateOffShoot(m_cb));

				//				m_logger.warning("doof test: second activation...");
				//				CBvoidLampAccess cb2 = new CBvoidLampAccess(m_logger);
				//				m_containerServices.activateOffShoot(cb2);
			}
			catch (Exception e)
			{
				m_logger.log(Level.SEVERE, "failed to obtain the callback offshoot object.", e);
				throw new LampUnavailable(e.getMessage());
			}

		}

		if (m_desc == null)
		{
			m_desc = new CBDescIn();
			m_logger.info("m_desc instantiated...");
		}
		try
		{
			m_logger.info("before callback done...");
			getLampBrightnessProperty().set_async(brightness, m_cbvoid, m_desc);
			m_logger.finer("now callback done...");
		}
		catch (Exception ex)
		{
			m_logger.log(Level.SEVERE, "ex in setLampBrightness impl", ex);
			throw new LampUnavailable(ex.getMessage());
		}
	}

	/**
	 * Returns the magnitude of the lamp component's brightness. 
	 * @return double
	 * @see alma.demo.LampAccessOperations#getLampBrightness()
	 * @throws LampUnavailable
	 */
	public double getLampBrightness() throws LampUnavailable
	{
		m_logger.finer("LampAccess:getLampBrightness() called...");
		
		try
		{
			m_brightness = getLampBrightnessProperty();
			CompletionHolder compHolder = new CompletionHolder();
			return m_brightness.get_sync(compHolder);

		}
		catch (Exception ex)
		{
			throw new LampUnavailable(ex.getMessage());
		}
	}


	/////////////////////////////////////////////////////////////
	// other
	/////////////////////////////////////////////////////////////	

	/**
	 * Gets the lamp component's (LAMP1) brightness as RWdouble.
	 * @return RWdouble
	 * @throws LampUnavailable
	 */
	RWdouble getLampBrightnessProperty() throws LampUnavailable
	{
		if (m_brightness == null)
		{
			org.omg.CORBA.Object cmp = null;
			try
			{
				cmp = m_containerServices.getComponent(m_lampCurl);
				Lamp lamp = LampHelper.narrow(cmp);
				m_brightness = lamp.brightness();
			}
			catch (Exception ex)
			{
				throw new LampUnavailable(ex.getMessage());
			}

			if (m_brightness == null)
			{
				throw new LampUnavailable("failed to obtain the lamp component's brightness property (NULL).");
			}
		}
		return m_brightness;
	}
	
//	/**
//	 * Throws an exception that is of type LampAccessAcsJEx.
//	 * @throws ACSException
//	 */
//	public void brightLampExceptionMethod() throws ACSException
//	{
//		try
//		{
//			throw new LampAccessAcsJEx("mean bright lamp exception");
//		}
//		catch (AcsJException ex)
//		{
//			AcsJException acsEx = new LampAccessAcsJEx("caught exception", ex);
//			ACSException e = ex.getACSException();
//			throw e;
//		}
//	}
}

