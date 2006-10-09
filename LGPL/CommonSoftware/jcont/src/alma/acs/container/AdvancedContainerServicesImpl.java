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
package alma.acs.container;

import java.util.logging.Logger;

import org.omg.CORBA.ORB;

import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;


/**
 * This class implements the more exotic methods from the container services interface,
 * which have been refactored out of <code>ContainerServices</code> into a separate interface.
 * 
 * @see alma.acs.container.ContainerServicesImpl  
 * @author hsommer 
 */
public class AdvancedContainerServicesImpl implements AdvancedContainerServices
{
    // the container services which this class logically belongs to 
	protected ContainerServicesImpl containerServicesImpl;

    // logger used by this class
	protected Logger logger;
    
    
	AdvancedContainerServicesImpl(ContainerServicesImpl containerServicesImpl, Logger logger) {
        this.containerServicesImpl = containerServicesImpl;
        this.logger = logger;
    }

    
	/* (non-Javadoc)
	 * @see alma.acs.container.ContainerServices#corbaObjectToString(org.omg.CORBA.Object)
	 */
	public String corbaObjectToString(org.omg.CORBA.Object objRef)
	{
		ORB orb = containerServicesImpl.getAcsCorba().getORB();
		String str = orb.object_to_string(objRef);
		logger.finer("converted corba object reference of type " + objRef.getClass().getName() +
						" to the string " + str);
		return str;
	}

	/* (non-Javadoc)
	 * @see alma.acs.container.ContainerServices#corbaObjectFromString(java.lang.String)
	 */
	public org.omg.CORBA.Object corbaObjectFromString(String strObjRef)
	{
		ORB orb = containerServicesImpl.getAcsCorba().getORB();
		org.omg.CORBA.Object objRef = orb.string_to_object(strObjRef);
		logger.finer("converted corba object reference string " + strObjRef + 
						" back to a corba object reference.");
		return objRef;
	}

	
    /**
     * Returns a reference to a new CORBA Any. In Java the only way to do 
     * this is through the ORB itself (i.e., the create_any method).
     * @return org.omg.CORBA.Any or null in case of error
     */
    public org.omg.CORBA.Any getAny() {
		//sanity check to ensure the container/client has setup an ORB properly
		if (containerServicesImpl.getAcsCorba().getORB() == null) {
			logger.warning("Failed to get a non-null ORB");
			return null;
		}
		return containerServicesImpl.getAcsCorba().getORB().create_any();
	}
    

    /* TODO: remove this method once weak component references are implemented.
     * @see alma.acs.container.AdvancedContainerServices#forceReleaseComponent(java.lang.String)
     */
    public void forceReleaseComponent(String curl) throws AcsJContainerServicesEx {
    	containerServicesImpl.releaseComponent(curl, true);
    }
}

