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
package alma.alarmsystem.corbaservice;

import org.omg.CORBA.ORB;

import alma.acs.alarmsystem.corbaservice.AlarmServiceUtils;
import alma.acs.container.ContainerServicesBase;
import alma.acs.logging.AcsLogger;
import alma.alarmsystem.AlarmService;
import alma.alarmsystem.CERNAlarmService;
import alma.alarmsystem.CERNAlarmServiceHelper;

/**
 * An helper class with a set of useful methods.
 * <P>
 * Some of the methods of this class can be used through a script.
 * <P>
 * AlarmServiceUtils needs a {@link ORB} that can be passed in the constructor
 * directly or through an instance of {@link ContainerServicesBase}.
 * <BR>
 * If the empty constructor is used, a new {@link ORB} is instantiated.
 * 
 * @author acaproni
 *
 */
public class CernAlarmServiceUtils extends AlarmServiceUtils {
	
	/**
	 * Constructor
	 * 
	 * @param orb The ORB
	 * @param theLogger The logger
	 */
	public CernAlarmServiceUtils(ORB orb, AcsLogger theLogger) {
		super(orb,theLogger);
	}
	
	/**
	 * Constructor
	 * 
	 * @param containerServices The container services
	 */
	public CernAlarmServiceUtils(ContainerServicesBase containerServices) {
		super(containerServices.getAdvancedContainerServices().getORB(), containerServices.getLogger());
	}
	
	/**
	 * Constructor
	 * 
	 * @param theLogger The logger; if <code>null</code> a new logger is instantiated
	 */
	public CernAlarmServiceUtils(AcsLogger theLogger) {
		super(theLogger);
	}
	
	/**
	 * Get a reference to the {@link CERNAlarmService}.
	 * 
	 * @return The {@link CERNAlarmService} 
	 */
	public CERNAlarmService getCernAlarmService() throws Exception {
		AlarmService alService=getAlarmService();
		if (alService.isACSAlarmService()) {
			throw new Exception("The ACS implementation of the alarm service is in use");
		}
		return CERNAlarmServiceHelper.narrow(alService);
	}
}

