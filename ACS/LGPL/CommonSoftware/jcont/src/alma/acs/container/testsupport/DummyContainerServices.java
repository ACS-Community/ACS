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
package alma.acs.container.testsupport;

import java.util.Properties;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import org.omg.CORBA.Object;

import si.ijs.maci.ComponentSpec;

import alma.ACS.OffShoot;
import alma.ACS.OffShootOperations;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.alarmsystem.source.AlarmSource;
import alma.acs.component.ComponentDescriptor;
import alma.acs.component.ComponentQueryDescriptor;
import alma.acs.component.ComponentStateManager;
import alma.acs.container.AcsManagerProxy;
import alma.acs.container.ContainerServices;
import alma.acs.container.ContainerServicesImpl;
import alma.acs.nc.AcsEventPublisher;
import alma.acs.nc.AcsEventSubscriber;
import alma.acs.nc.testsupport.DummyContainerServicesBase;
import alma.entities.commonentity.EntityT;

/**
 * Dummy container services to be used for unit testing without running ACS services.
 * <p>
 * Note that {@link ContainerServicesImpl} would drag in {@link AcsManagerProxy}, 
 * so that it seems cleaner to create a dummy class for which the unit test
 * can override a few methods. 
 * 
 * @author hsommer
 */
public class DummyContainerServices extends DummyContainerServicesBase implements ContainerServices
{
	private AlarmSource alarmSource;

	/**
	 * @param name
	 * @param logger A JDK logger will be wrapped to become an AcsLogger. If <code>null</code>, a new logger gets created.
	 */
	public DummyContainerServices(String name, Logger logger) {
		super(name, logger);
		
		alarmSource = new AlarmSource() {
			
			@Override
			public void terminateAllAlarms() {
			}
			
			@Override
			public void tearDown() {
			}
			
			@Override
			public void start() {
			}
			
			@Override
			public void setAlarm(String faultFamily, String faultMember, int faultCode, Properties alarmProps, boolean active) {
			}
			
			@Override
			public void setAlarm(String faultFamily, String faultMember, int faultCode, boolean active) {
			}
			
			@Override
			public void raiseAlarm(String faultFamily, String faultMember, int faultCode, Properties properties) {
			}
			
			@Override
			public void raiseAlarm(String faultFamily, String faultMember, int faultCode) {
			}
			
			@Override
			public void queueAlarms(long delayTime, TimeUnit unit) {
			}
			
			@Override
			public void queueAlarms() {
			}
			
			@Override
			public void flushAlarms() {
			}
			
			@Override
			public void enableAlarms() {
			}
			
			@Override
			public void disableAlarms() {
			}
			
			@Override
			public void clearAlarm(String faultFamily, String faultMember, int faultCode) {
			}
		};
	}

	///////////////////////////////////////
	

	/**
	 * @see alma.acs.container.ContainerServices#activateOffShoot(alma.acs.container.OffShootOperations, java.lang.Class)
	 */
	@Override
	public <T extends OffShootOperations> OffShoot activateOffShoot(T offshootImpl, Class<T> idlOpInterface)
			throws AcsJContainerServicesEx {
		throw new AcsJContainerServicesEx();
	}


	/**
	 * @throws RuntimeException, see {@link #check()}
	 * @see alma.acs.container.ContainerServices#getComponentStateManager()
	 */
	@Override
	public ComponentStateManager getComponentStateManager() {
		return null;
	}

	/**
	 * @see alma.acs.container.ContainerServices#getComponent(java.lang.String)
	 */
	@Override
	public Object getComponent(String componentUrl) throws AcsJContainerServicesEx {
		throw new AcsJContainerServicesEx();
	}

	/**
	 * @see alma.acs.container.ContainerServices#getComponentNonSticky(java.lang.String)
	 */
	@Override
	public Object getComponentNonSticky(String curl) throws AcsJContainerServicesEx {
		throw new AcsJContainerServicesEx();
	}

	/**
	 * @see alma.acs.container.ContainerServices#getDefaultComponent(java.lang.String)
	 */
	@Override
	public Object getDefaultComponent(String componentIDLType) throws AcsJContainerServicesEx {
		throw new AcsJContainerServicesEx();
	}

	/**
	 * @see alma.acs.container.ContainerServices#getCollocatedComponent(java.lang.String, java.lang.String)
	 */
	@Override
	public Object getCollocatedComponent(String compUrl, String targetCompUrl) throws AcsJContainerServicesEx {
		throw new AcsJContainerServicesEx();
	}

	/**
	 * @see alma.acs.container.ContainerServices#getCollocatedComponent(alma.acs.component.ComponentQueryDescriptor, boolean, java.lang.String)
	 */
	@Override
	public Object getCollocatedComponent(ComponentQueryDescriptor compSpec, boolean markAsDefaul, String targetCompUrl)
			throws AcsJContainerServicesEx {
		throw new AcsJContainerServicesEx();
	}

	/**
	 * @see alma.acs.container.ContainerServices#getDynamicComponent(alma.acs.component.ComponentQueryDescriptor, boolean)
	 */
	@Override
	public Object getDynamicComponent(ComponentQueryDescriptor compSpec, boolean markAsDefault)
			throws AcsJContainerServicesEx {
		throw new AcsJContainerServicesEx();
	}

	/**
	 * @deprecated
	 * @see alma.acs.container.ContainerServices#getDynamicComponent(alma.acs.container.ComponentSpec, boolean)
	 */
	@Override
	public Object getDynamicComponent(ComponentSpec compSpec, boolean markAsDefault) throws AcsJContainerServicesEx {
		throw new AcsJContainerServicesEx();
	}

	/**
	 * @throws RuntimeException, see {@link #check()}
	 * @see alma.acs.container.ContainerServices#findComponents(java.lang.String, java.lang.String)
	 */
	@Override
	public String[] findComponents(String curlWildcard, String typeWildcard) throws AcsJContainerServicesEx {
		throw new AcsJContainerServicesEx();
	}

	/**
	 * @throws RuntimeException, see {@link #check()}
	 * @see alma.acs.container.ContainerServices#getComponentDescriptor(java.lang.String)
	 */
	@Override
	public ComponentDescriptor getComponentDescriptor(String componentUrl) throws AcsJContainerServicesEx {
		throw new AcsJContainerServicesEx();
	}

	/**
	 * @see alma.acs.container.ContainerServices#releaseComponent(java.lang.String)
	 */
	@Override
	public void releaseComponent(String componentUrl) {
	}

	/**
	 * @see alma.acs.container.ContainerServices#releaseComponent(String, ComponentRequestCallback)
	 */
	@Override
	public void releaseComponent(String componentUrl, ComponentReleaseCallback callback) {
	}

	/**
	 * @see alma.acs.container.ContainerServices#registerComponentListener(alma.acs.container.ContainerServices.ComponentListener)
	 */
	@Override
	public void registerComponentListener(ComponentListener listener) {
	}

	/**
	 * @see alma.acs.container.ContainerServices#getReferenceWithCustomClientSideTimeout(org.omg.CORBA.Object, double)
	 */
	@Override
	public Object getReferenceWithCustomClientSideTimeout(Object originalCorbaRef, double timeoutSeconds)
			throws AcsJContainerServicesEx {
		throw new AcsJContainerServicesEx();
	}

	/**
	 * @see alma.acs.container.ContainerServices#assignUniqueEntityId(alma.acs.container.EntityT)
	 */
	@Override
	public void assignUniqueEntityId(EntityT entity) throws AcsJContainerServicesEx {
		throw new AcsJContainerServicesEx();
	}

	/**
	 * @see alma.acs.container.ContainerServices#getTransparentXmlWrapper(java.lang.Class, java.lang.Object, java.lang.Class)
	 */
	@Override
	public <T, F> T getTransparentXmlWrapper(Class<T> transparentXmlIF, F objectReference, Class<F> flatXmlIF)
			throws AcsJContainerServicesEx {
		throw new AcsJContainerServicesEx();
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

	@Override
	public AlarmSource getAlarmSource() {
		return alarmSource;
	}

}
