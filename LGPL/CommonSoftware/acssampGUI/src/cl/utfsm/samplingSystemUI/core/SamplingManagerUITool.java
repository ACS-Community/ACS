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
import java.util.Hashtable;

import alma.ACSErrTypeCommon.CouldntAccessComponentEx;
import alma.ACSErrTypeCommon.CouldntAccessPropertyEx;
import alma.ACSErrTypeCommon.TypeNotSupportedEx;
import alma.JavaContainerError.wrappers.AcsJContainerEx;


/**
 * The class is a base class for sampling tools. A tool must extend this class
 * and implement a static main. Usage of the sampling system must only be done
 * through the member provided by this class. 
 *
 */
public class SamplingManagerUITool {
	protected static AcsInformation info=null;
	private static Hashtable<SampDetail,PropertySamp> sampler = new Hashtable<SampDetail,PropertySamp>();
	/*container useage count*/

	/**
	 * Prepares the conection to the ACS infrastructure. This member creates
	 * two objects, an AcsInformation and a SamplingManager. This two are
	 * the links with the ACS infrastructure.
	 * 
	 * @param clientName string tha contains the name with which the tool
	 * will register with ACS.
	 * @param managerName string that contains the name of the sampling
	 * manager in the container. This manager is specified in the CDB.
	 * @throws AcsJContainerEx 
	 *
	 * @see AcsInformation
	 * @see SamplingManager
	 */
	protected static void spinUp(String clientName, String managerName) throws AcsInformationException, SamplingManagerException, AcsJContainerEx {
		info = AcsInformation.getInstance(clientName);
		SamplingManager.getInstance(managerName);
	}

	/**
	 * Start the sampling of a property. This member will start a
	 * PropertySamp object, which will run in a separeted Thread.
	 * 
	 * @param sDetail a SampDetail object that contains the specification
	 * for the sampling.
	 * @throws CouldntAccessComponentEx 
	 * @throws TypeNotSupportedEx 
	 * @throws CouldntAccessPropertyEx
	 * @throws SamplingManagerException
	 * @see SampDetail
	 * @see PropertySamp  
	 */
	protected static void startSample(SampDetail sDetail) throws 
                                  CouldntAccessComponentEx, TypeNotSupportedEx,
                                  CouldntAccessPropertyEx, SamplingManagerException {
		
		if(sampler.get(sDetail) == null)
			sampler.put(sDetail,new PropertySamp(sDetail));

		try {
			sampler.get(sDetail).run();
		} catch(CouldntAccessComponentEx e) {
			sampler.remove(sDetail);
			throw e;
		} catch(alma.ACSErrTypeCommon.TypeNotSupportedEx e) {
			sampler.remove(sDetail);
			throw e;
		} catch(CouldntAccessPropertyEx e) {
			sampler.remove(sDetail);
			throw e;
		} catch(Exception e) {
			sampler.remove(sDetail);
			throw new SamplingManagerException("Could not start sample",e);
		}
	}

	/**
	 * Stop the sampling of a property identified by a SampDetail. 
	 *
	 * @see SampDetail
	 */
	protected static void stopSample(SampDetail sDetail){
		sampler.get(sDetail).stop();
	}

	/**
	 * Stop and delete from the internal structures the sampling identified
	 * by a SampDetail.
	 *
	 * @see SampDetail
	 */
	protected static void deleteSample(SampDetail sDetail){
		stopSample(sDetail);
		sampler.remove(sDetail);
	}

	/**
	 * Shutdown all the sampling operation. Stop every sampling, and
	 * disconnects from the ACS manager.
	 *
	 * @see AcsInformation
	 */
	protected static void tearDown() throws Exception{
		for (PropertySamp item : sampler.values()) {
			item.stop();
			info.shutDown();
		}
	}

	protected static void hashPrint(){
		for (PropertySamp item : sampler.values()){
			System.out.println(item);	
		}
	}

	/**
	 * Wraper that checks if a component exists.
	 *
	 * @param componetName the name of the component.
	 */
	protected static boolean componentExists(String componentName){
		return info.componentExists(componentName);

	}

	/**
	 * Wraper that checks if a property exists given a component.
	 *
	 * @param componetName the name of the component.
	 * @param propertyName the name of the property. 
	 */
	protected static boolean propertyExists(String componentName, String propertyName){
		return info.propertyExists(componentName,propertyName);
	}
}

