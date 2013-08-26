/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2011
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *                 and Cosylab
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
package alma.alarmsystem.corbaservice;

import org.omg.CORBA.Any;
import org.omg.CORBA.ORB;

import alma.acs.container.AdvancedContainerServices;
import alma.acs.logging.AcsLogger;

import si.ijs.maci.AdministratorOperations;

import alma.JavaContainerError.wrappers.AcsJContainerEx;

public class AlarmSystemAdvancedContainerServices implements AdvancedContainerServices {
	
	/**
	 * The logger
	 */
	private final AcsLogger logger;
	
	private final AlarmSystemContainerServices alSysContSvcs;
	
	public AlarmSystemAdvancedContainerServices(AlarmSystemContainerServices alSysContSvcs) {
		if (alSysContSvcs==null) {
			throw new IllegalArgumentException("The alarm system container services can't be null");
		}
		this.alSysContSvcs=alSysContSvcs;
		logger=alSysContSvcs.getLogger();
		if (logger==null) {
			throw new IllegalStateException("The logger can't be null");
		}
	}
	
	@Override
	public org.omg.CORBA.Object corbaObjectFromString(String strObjRef) {
		ORB orb = alSysContSvcs.getOrb();
		org.omg.CORBA.Object objRef = orb.string_to_object(strObjRef);
		logger.finer("converted corba object reference string " + strObjRef + 
						" back to a corba object reference.");
		return objRef;
	}
	
	@Override
	public String corbaObjectToString(org.omg.CORBA.Object objRef)
	{
		ORB orb = getORB(); 
		String str = orb.object_to_string(objRef);
		logger.finer("converted corba object reference of type " + objRef.getClass().getName() +
						" to the string " + str);
		return str;
	}

	@Override
	public Any getAny() {
		ORB orb = getORB();
    	org.omg.CORBA.Any any = orb.create_any();
    	if (any == null) {
    		// should never happen, but we check just in case, 
    		// since there is a difficult to verify NPE when Any is created by MC for sending alarms.
    		String msg = "Failed to create org.omg.CORBA.Any";
    		logger.warning(msg);
    		throw new NullPointerException(msg);
    	}
    	return any;
	}

	@Override
	public ORB getORB() {
		return alSysContSvcs.getOrb();
	}
	
	@Override
	public void disconnectManagerAdmin(AdministratorOperations adminOp) {
	}
	
	public void connectManagerAdmin(AdministratorOperations adminOp, boolean retryConnectOnFailure) throws AcsJContainerEx {
	}
}
