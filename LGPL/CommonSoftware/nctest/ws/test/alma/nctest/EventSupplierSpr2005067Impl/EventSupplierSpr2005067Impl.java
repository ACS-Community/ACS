/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) Associated Universities Inc., 2002
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
 *
 * ncTestCompImpl.java
 *
 * Created on April 11, 2003, 2:21 PM
 */
package alma.nctest.EventSupplierSpr2005067Impl;

import java.util.logging.Level;

import org.omg.CORBA.portable.IDLEntity;

import alma.ACSErrTypeCommon.wrappers.AcsJGenericErrorEx;
import alma.ACSErrTypeCommon.wrappers.AcsJUnexpectedExceptionEx;
import alma.SPR2005067.ControlSystemChangeOfStateEvent;
import alma.SPR2005067.ControlSystemChangeOfStateEvent2;
import alma.acs.component.ComponentImplBase;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.acs.exceptions.AcsJException;
import alma.acs.nc.AcsEventPublisher;
import alma.demo.SupplierCompOperations;

/** Class designed for testing event suppliers.
 * @author dfugate
 */
public class EventSupplierSpr2005067Impl 
    extends ComponentImplBase implements SupplierCompOperations
{

   private AcsEventPublisher<IDLEntity> m_supplier = null;
   
   /** Sends some events to an event channel.
    * @param param number of events to send
    */
   public void sendEvents(short param)
   {
       /* 
	* This is OK
	*/
       m_logger.info("Now sending ControlSystemChangeOfStateEvent2 events. This should always be OK");
       try
	   {
	   ControlSystemChangeOfStateEvent2 t_block = 
	       new ControlSystemChangeOfStateEvent2(alma.SPR2005067.SystemState.INACCESSIBLE,
						    alma.SPR2005067.SystemSubstate.STARTING_UP_PASS1,
						    false,
						    0
		   );
	   for(short i=0; i<param; i++)
	       {
	       m_supplier.publishEvent(t_block);
	       }
	   
	   }
       catch(AcsJException e)
	   {
	   /*
	    * Here I just log the error trace.
	    * It would be nicer to throw it to the caller,
	    * but the interface of sendEvents does not allow it.
	    * We should change that interface.
	    */
	   AcsJGenericErrorEx ex = new AcsJGenericErrorEx("This exception publishing events should have never occurred", e); 
	   ex.log(m_logger);
	   }
       catch (Throwable thr) 
	   {
	   AcsJUnexpectedExceptionEx ex = new AcsJUnexpectedExceptionEx("Got unexpected exception", thr);
	   /*
	    * Here I just log the error trace.
	    * It would be nicer to throw it to the caller,
	    * but the interface of sendEvents does not allow it.
	    * We should change that interface.
	    */
	   ex.log(m_logger);
	   }
       /*
	* This fails with JacORB (tested up to version 2.2.4)
	* See SPR 2005067
	*/
       m_logger.info("Now sending ControlSystemChangeOfStateEvent events. This FAILS now but should be OK as well. See SPR2005067");
       try
	   {
	   ControlSystemChangeOfStateEvent t_block = 
	       new ControlSystemChangeOfStateEvent(alma.SPR2005067.SystemState.INACCESSIBLE,
						   alma.SPR2005067.SystemSubstate.STARTING_UP_PASS1,
						   alma.SPR2005067.SystemState.INACCESSIBLE,
						   alma.SPR2005067.SystemSubstate.STARTING_UP_PASS1,
						   false,
						   0
		   );
	   for(short i=0; i<param; i++)
	       {
	       m_supplier.publishEvent(t_block);
	       }
	   
	   }
       catch(AcsJException e)
	   {
	   /*
	    * Here I just log the error trace.
	    * It would be nicer to throw it to the caller,
	    * but the interface of sendEvents does not allow it.
	    * We should change that interface.
	    */
	   AcsJGenericErrorEx ex = new AcsJGenericErrorEx("This exception is due to SPR2005067", e); 
	   ex.log(m_logger);
	   }
       catch (Throwable thr) 
	   {
	   /*
	    * The message int the exception logged is incomplete.
	    * Look at the println and compare to see the difference
	    */
           System.err.println(thr);
	   AcsJUnexpectedExceptionEx ex = new AcsJUnexpectedExceptionEx("Got unexpected exception", thr);
	   /*
	    * Here I just log the error trace.
	    * It would be nicer to throw it to the caller,
	    * but the interface of sendEvents does not allow it.
	    * We should change that interface.
	    */
	   ex.log(m_logger);
	   }
   }
    
   
   public void sendEventsSpecial(alma.FRIDGE.FridgeControlPackage.NestedFridgeEvent[] eventData) throws alma.ACSErrTypeCommon.CouldntPerformActionEx
   {
	   // todo
   }
   
   
	/** Disconnects the supplier. */
	public void cleanUp() {
		m_logger.info("cleanUp() called...");

		try {
			m_supplier.disconnect();
		} catch (Exception e) {
			ComponentLifecycleException acsex = new ComponentLifecycleException("failed to cleanup AcsEventPublisher", e);
			m_logger.log(Level.SEVERE, "Failed to cleanup", e);

			// Here I want to log the error stack.
			// But this is not an ACS exception, so I cannot do it.
		}
	}

	/**
	 * Sets up the AcsEventPublisher.
	 * 
	 * @param containerServices
	 *            Services to components.
	 * @throws ComponentLifecycleException
	 *             Not thrown.
	 */
	public void initialize(ContainerServices containerServices) throws ComponentLifecycleException {
		super.initialize(containerServices);
		m_logger.info("initialize() called...");

		try {
			// Instantiate our supplier
			m_supplier = containerServices.createNotificationChannelPublisher(
					alma.SPR2005067.CHANNELNAME_SPR2005067.value, // the channel's name
					IDLEntity.class); // common base type of the events we'll publish
		} catch (Exception e) {
			throw new ComponentLifecycleException("failed to create new AcsEventPublisher", e);
		}
	}
}

