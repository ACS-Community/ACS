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
/**
 *      @author Julio Araya (jaray[at]alumnos.inf.utfsm.cl) &
 *      Nicolas Troncoso (ntroncos[at]alumnos.inf.utfsm.cl)
 **/

package cl.utfsm.samplingSystemUI.core;
import alma.ACSErrTypeCommon.CouldntAccessComponentEx;
import alma.ACSErrTypeCommon.TypeNotSupportedEx;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.ACSErrTypeCommon.CouldntAccessPropertyEx;
import alma.ACSErrTypeCommon.CORBAProblemEx;
import alma.ACSErrTypeCommon.CouldntCreateObjectEx;
import alma.ACSErrTypeCommon.MemoryFaultEx;
import alma.ACSErrTypeCommon.OutOfBoundsEx;
import alma.acs.exceptions.AcsJException;
import alma.acssamp.SampObj;

/**
 * For each sampling detail one of this classes must be created to sample the
 * data. This class creates the notification channel, and starts the sampling.
 * This class also provides means of stoping and cleaning up the sampling.
 * Sampling is down in separated Thread, so the run member will return
 * imediatly.
 */ 
public class PropertySamp {
	SampDetail sampDetail=null;
	SamplingManager sManager =null;
	SampObj sampObject = null;
	NotificationChannelSuscription ncs = null;
	AcsInformation info = null;

	/**
	* Generates a SamplingObject (if none exist for the target sampDetail) and creates a channel for communication
	 * @throws CouldntAccessComponentEx 
	 * @throws TypeNotSupportedEx 
	 * @throws AcsJContainerServicesEx
	 * @throws CouldntAccessPropertyEx
	 * @throws CORBAProblemEx
	 * @throws CouldntCreateObjectEx
	 * @throws MemoryFaultEx
	 * @throws OutOfBoundsEx
	 * @throws AcsJException
	 * @throws SamplingManagerException 
	*/
	public void run() throws CouldntAccessComponentEx, TypeNotSupportedEx, 
                                  AcsJContainerServicesEx, CouldntAccessPropertyEx,
                                  CORBAProblemEx, CouldntCreateObjectEx,
                                  MemoryFaultEx, OutOfBoundsEx, AcsJException, SamplingManagerException {
		
		try {
			sampObject=sManager.getSamplingObj(sampDetail);
			ncs = new NotificationChannelSuscription(sampObject.getChannelName(),info.getContainerServices());
			ncs.consumerReady();
			sampObject.start();
		} catch (alma.ACSErrTypeCommon.CouldntAccessComponentEx ex) {
			throw ex;
		} catch (alma.ACSErrTypeCommon.TypeNotSupportedEx ex) {
			throw ex;
		} catch (SamplingManagerException ex) {
			throw ex;
		}
	}

	/**
	* Assigns a SamplingManager and an AcsInformation object to the sampDetail (each sampDetail should have it's own Manager)
	* @param sampDetail Instance of SampDetail to be Monitored by the Manager.
	*/
	public PropertySamp(SampDetail sampDetail){
		this.sampDetail=sampDetail;
		sManager=SamplingManager.getInstance();
		info = AcsInformation.getInstance();
	}

	/**
	* Closes the Notification Channel, stops sampling of the current SampDetail and destroys the Sampling Object
	*/
	public void stop(){
		ncs.disconnect();
		sampObject.suspend();
	}
}
