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
import alma.ACSErr.NameValue;
import alma.ACSErrTypeCommon.CouldntAccessComponentEx;
import alma.ACSErrTypeCommon.TypeNotSupportedEx;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.ACSErrTypeCommon.CouldntAccessPropertyEx;
import alma.ACSErrTypeCommon.CORBAProblemEx;
import alma.ACSErrTypeCommon.CouldntCreateObjectEx;
import alma.ACSErrTypeCommon.MemoryFaultEx;
import alma.ACSErrTypeCommon.OutOfBoundsEx;
import alma.acssamp.*;

import java.util.HashMap;
import java.util.Hashtable;


/**
 * Probaly this class should have another name, but it does manage the sampling
 * system within this application. This class is a per-manager singleton. And holds
 * references for each ACS sampling object. Each one of this objects is
 * associated in hashTable with a sampling detail 
 * @see SampDetail
 * @see SampObj
 */ 
public class SamplingManager {
	Hashtable<SampDetail,SampObj> samplingObjects = null;
	AcsInformation info = null;
	private static HashMap<String,SamplingManager> _instances = null;
	private static SamplingManager _last_referenced = null;
	private String managerName = "";
	private Samp _acssamp_instance = null;

	static {
		_instances = new HashMap<String, SamplingManager>(); 
	}

	/**
	 * Private constructor. This may only be called from within this class.
	 * This is done beacause this class is a singleton, so just one
	 * instance should be allowed per sampling manager.
	 */ 
	private SamplingManager(String managerName) throws SamplingManagerException{
		try{
			info = AcsInformation.getInstance();
			if(!info.componentExists(managerName)){
				throw new SamplingManagerException("No reference to manager by name "+managerName);
			}
			samplingObjects = new Hashtable<SampDetail,SampObj>();
			this.managerName = managerName;
			_acssamp_instance = null;
		}catch(Exception e){
			throw new SamplingManagerException("No reference to manager by name "+managerName,e);
		}
	}

	/**
	 * Singleton creator. This member calls the contructor and verifies
	 * that only one instance is allowed for the given sampling manager.
	 * @param managerName The sampling managercomponent 
	 * @return A singleton for this sampling manager
	 * @throws SamplingManagerException If something goes wrong in the creation of the singleton
	 */
	public static synchronized SamplingManager getInstance(String managerName) throws SamplingManagerException{ 
		SamplingManager inst = _instances.get(managerName);
		if ( inst == null) {
			inst = new SamplingManager(managerName);
			_instances.put(managerName, inst);
		}
		_last_referenced = inst;
		return inst;
	}
	
	/**
	 * Returns the last sampling manager referenced by {@link #getInstance(String)}. 
	 * @return The last sampling manager singleton referenced by the system.
	 */
	public static synchronized SamplingManager getInstance() { 
		if (_last_referenced == null) {
			throw new IllegalStateException("Sampling manager instance was requested, but it does not exist."); 
		} 
		return _last_referenced;
	}

	/**
	 * Gets a reference to the sampling manager and returns it
	 * As of ACS-7.0 the following call throws AcsJContainerServicesEx instead of returnin null
	 * 
	 * @return alma.acssamp.Samp
	 * @throws SamplingManagerException 
	 */
	public Samp getSampReference() throws SamplingManagerException {
		
		if( _acssamp_instance == null ) {
			org.omg.CORBA.Object obj;
			try {
				obj = (info.getContainerServices()).getComponent(managerName);
				_acssamp_instance = alma.acssamp.SampHelper.narrow(obj);
			} catch (AcsJContainerServicesEx e) {
				
				throw new SamplingManagerException("Couldn't get reference to Sampling Manager '"
						+ managerName + "'",e);
			}
		} else {
			try {
				_acssamp_instance.componentState();
			} catch (Exception e) {
				info.getContainerServices().releaseComponent(managerName);
				_acssamp_instance = null;
				throw new SamplingManagerException("Sampling Manager '"
						+ managerName + "' reference has been destroyed");
			}
		}
		return _acssamp_instance;
	}
	
	/**
	 * Get or create a new Sampling object. This member will create a new sampling
	 * object for each distinc sampling detail. If the sampling detail already
	 * existes it will return the already created dampling object.
	 * @param managerDef Sampling detail requested.
	 * @return a sampling object SampObj.
	 * @throws CouldntAccessComponentEx 
	 * @throws TypeNotSupportedEx 
	 * @throws AcsJContainerServicesEx
	 * @throws CouldntAccessPropertyEx
	 * @throws CORBAProblemEx
	 * @throws CouldntCreateObjectEx
	 * @throws MemoryFaultEx
	 * @throws OutOfBoundsEx
	 * @throws SamplingManagerException 
	 */ 
	public synchronized SampObj getSamplingObj(SampDetail managerDef) throws CouldntAccessComponentEx, TypeNotSupportedEx, 
                                                                                 AcsJContainerServicesEx, CouldntAccessPropertyEx,
                                                                                 CORBAProblemEx, CouldntCreateObjectEx,
                                                                                 MemoryFaultEx, OutOfBoundsEx, SamplingManagerException {
		SampObj temp=null;
		if(samplingObjects.get(managerDef)!=null){
			info.getContainerServices().getLogger().info("Trying to add a duplicate samplingObj");
			return samplingObjects.get(managerDef);
		}
		try{
			temp = getSampReference().initSampObj(managerDef.getComponent(),managerDef.getProperty(),managerDef.getFrequency(),managerDef.getReportRate());
		} catch(alma.ACSErrTypeCommon.CouldntAccessComponentEx tmp) {
			info.getContainerServices().getLogger().warning("Sampling Manager could not access component " +  managerDef.getComponent());
			throw tmp;
		} catch(alma.ACSErrTypeCommon.TypeNotSupportedEx tmp) {
			String str = "";
			for( NameValue val : tmp.errorTrace.data)
				if(val.name.equals("Type Name")) {
					str = val.value;
					break;
				}
			info.getContainerServices().getLogger().warning("Unsopported type for component/property: " + managerDef.getComponent() + "/" + managerDef.getProperty() + ": " + str);
			throw tmp;
		} catch(SamplingManagerException e) {
			throw e;
		}
		samplingObjects.put(managerDef,temp);
		return temp;
	}
	/**
	 * Check for a sampling detail, and return true if it a sampling object
	 * has been created for it.
	 * @param managerDef ampling detail requested.
	 * @return true if it exists, or false if it does not.
	 */ 
	public synchronized Boolean checkObj(SampDetail managerDef){
		if(samplingObjects.get(managerDef)==null)
			return false;
		return true;
	}

	/**
	* Finalizes the usage of all Components taken over by the Manager
	*
	*/
	protected void finalize() {
		 info.getContainerServices().releaseComponent(managerName);
	}
}
