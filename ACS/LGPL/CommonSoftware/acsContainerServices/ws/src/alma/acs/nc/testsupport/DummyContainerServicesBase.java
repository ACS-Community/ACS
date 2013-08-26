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
package alma.acs.nc.testsupport;

import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.logging.Logger;

import org.omg.PortableServer.Servant;

import com.cosylab.CDB.DAL;

import alma.ACS.OffShoot;
import alma.ACS.OffShootOperations;
import alma.JavaContainerError.ContainerServices;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.container.AdvancedContainerServices;
import alma.acs.container.ContainerServicesBase;
import alma.acs.logging.AcsLogger;
import alma.acs.logging.ClientLogManager;
import alma.acs.nc.AcsEventPublisher;
import alma.acs.nc.AcsEventSubscriber;

/**
 * Dummy container services to be used for unit testing without running ACS services.
 * <p>
 * Note that {@link ContainerServicesImpl} would drag in {@link AcsManagerProxy}, 
 * so that it seems cleaner to create a dummy class for which the unit test
 * can override a few methods. 
 * 
 * @author hsommer
 */
public class DummyContainerServicesBase implements ContainerServicesBase
{
	protected final String name;
	protected final AcsLogger logger;

	/**
	 * @param name
	 * @param logger A JDK logger will be wrapped to become an AcsLogger. If <code>null</code>, a new logger gets created.
	 */
	public DummyContainerServicesBase(String name, Logger logger) {
		this.name = name;
		if (logger == null) {
			this.logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(name, false);
		}
		else {
			this.logger = AcsLogger.fromJdkLogger(logger, null);
		}
	}

	///////////////////////////////////////
	
	@Override
	public String getName() {
		return name;
	}

	/**
	 * @throws RuntimeException, see {@link #check()}
	 * @see alma.acs.container.ContainerServicesBase#getLogger()
	 */
	@Override
	public AcsLogger getLogger() {
		return logger;
	}

	/**
	 * @see alma.acs.container.ContainerServicesBase#getCDB()
	 */
	@Override
	public DAL getCDB() throws AcsJContainerServicesEx {
		throw new AcsJContainerServicesEx();
	}

	/**
	 * @see alma.acs.container.ContainerServicesBase#activateOffShoot(org.omg.PortableServer.Servant)
	 */
	@Override
	public <T extends Servant & OffShootOperations> OffShoot activateOffShoot(T cbServant)
			throws AcsJContainerServicesEx {
		throw new AcsJContainerServicesEx();
	}

	/**
	 * @throws AcsJContainerServicesEx
	 * @see alma.acs.container.ContainerServicesBase#deactivateOffShoot(java.lang.Object)
	 */
	@Override
	public void deactivateOffShoot(java.lang.Object offshootImpl) throws AcsJContainerServicesEx {
		throw new AcsJContainerServicesEx();
	}

	/**
	 * @see alma.acs.container.ContainerServicesBase#getAdvancedContainerServices()
	 */
	@Override
	public AdvancedContainerServices getAdvancedContainerServices() {
		return null;
	}

	/**
	 * @see alma.acs.container.ContainerServicesBase#getThreadFactory()
	 */
	@Override
	public ThreadFactory getThreadFactory() {
		return Executors.defaultThreadFactory();
	}


	/**
	 * @see alma.acs.container.ContainerServices#createNotificationChannelSubscriber(String)
	 */
	
	public <T> AcsEventSubscriber<T> createNotificationChannelSubscriber(String channelName, Class<T> eventType) throws AcsJContainerServicesEx {
		throw new AcsJContainerServicesEx();
	}

	/**
	 * @see alma.acs.container.ContainerServices#createNotificationChannelSubscriber(String, String)
	 */
	public <T> AcsEventSubscriber<T> createNotificationChannelSubscriber(String channelName, String channelNotifyServiceDomainName, Class<T> eventType) throws AcsJContainerServicesEx {
		throw new AcsJContainerServicesEx();
	}

	/**
	 * @see ContainerServices#createNotificationChannelPublisher(String)
	 */
	@Override
	public <T> AcsEventPublisher<T> createNotificationChannelPublisher(String channelName, Class<T> eventType) throws AcsJContainerServicesEx {
		throw new AcsJContainerServicesEx();
	}

	/**
	 * @see ContainerServices#createNotificationChannelPublisher(String, String)
	 */
	@Override
	public <T> AcsEventPublisher<T> createNotificationChannelPublisher(String channelName, String channelNotifyServiceDomainName, Class<T> eventType) throws AcsJContainerServicesEx {
		throw new AcsJContainerServicesEx();
	}

}
