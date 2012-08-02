/*
ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2012 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/
package alma.acs.alarmsystem.source;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import alma.acs.component.client.ComponentClient;
import alma.acs.container.ContainerServices;
import alma.acs.container.ContainerServicesBase;
import alma.acs.logging.AcsLogLevel;
import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;

/** 
 * <code>AlarmSourceFactory</code> is in charge of instantiating and releasing {@link AlarmSource} objects
 * avoid dependencies in the {@link ACSAlarmSystemInterfaceFactory}, {@link ComponentClient} and
 * {@link ContainerServices}.
 * <P>
 * <code>AlarmSourceFactory</code> should be used by {@link ComponentClient} and {@link ContainerServices}
 * to return a {@link AlarmSource} and to release all the {@link AlarmSource} objects before exiting.
 * <P>
 * The purpose of <code>AlarmSourceFactory</code> is to store and delivery <code>AlarmSource</code> objects to components,
 * being sure that a component instantiates at the most one and only one <code>AlarmSource</code>.
 * The <code>AlarmSourceFactory</code> keep a reference to all the instantiated <code>AlarmSource</code> objects to ensure, during the
 * shutdown of a component, to flush all the alarms and finally release all the resources used by those
 * objects.
 * <P>Life cycle: while there is no special initialization method, 
 * {@link AlarmSourceFactory#tearDown()} must be called when done with this object.
 * <P>Concurrency: We use a {@link ConcurrentHashMap} that reduces the number of lock outs in respect
 * of the traditional {@link HashMap} resulting in better performances, especially when the number of get's
 * is relevant as we expect during normal activities. To avoid using another lock for releasing
 * all the alarm sources, we preferred a volatile boolean to reject operations when the factory has been closed.
 * Under some circumstances, it could happen that the tearDown cleans the map when another method is still running.
 * This should not be a big deal because tearDown runs just before terminating the container (or
 * component client).
 * 
 * @author  acaproni
 * @version $Id: AlarmSourceFactory.java,v 1.1 2012/08/02 09:27:10 acaproni Exp $
 * @since ACS 10.2    
 */
public class AlarmSourceFactory {
	
	/**
	 * The ContainerServices
	 */
	private ContainerServicesBase contSvcs;
	
	/**
	 * The map of all the alarm sources created by the factory.
	 * <P>
	 * The key is the name of the component.
	 */
	private final Map<String, AlarmSource>alarmSources = new ConcurrentHashMap<String, AlarmSource>();
	
	/**
	 * The flag signals if the factory has been already closed to reject further
	 * incoming requests.
	 * <P>
	 * {@link ConcurrentHashMap} does not support transaction so we want to avoid accepting new
	 * requests especially during the shutdown where the factory must perform a series of operations.  
	 */
	private volatile boolean closed=false;
	
	/**
	 * Constructor
	 * 	
	 * @param logger The logger
	 * @param closed
	 */
	public AlarmSourceFactory(ContainerServicesBase contSvcs) {
		super();
		if (contSvcs==null) {
			throw new IllegalArgumentException("The ContainerServices can't be null");
		}
		this.contSvcs = contSvcs;
	}

	/**
	 * Return the {@link AlarmSource} for the component whose name is read from
	 * passed the {@link ContainerServicesBase}.
	 * <P>
	 * If a <code>AlarmSource</code> already exists for the component then a reference is returned,
	 * otherwise a new <code>AlarmSource</code> is built.
	 * 
	 * @param containerServices The not <code>null</code> {@link ContainerServicesBase}
	 * @return the {@link AlarmSource}
	 */
	public AlarmSource getAlarmSource(ContainerServicesBase containerServices) {
		if (containerServices==null) {
			throw new IllegalArgumentException("The ContainerServices can't be null");
		}
		return getAlarmSource(containerServices.getName());
	}
	
	/**
	 * Return the {@link AlarmSource} for the component with the passed name.
	 * <P>
	 * If a <code>AlarmSource</code> already exists for the component then a reference is returned,
	 * otherwise a new <code>AlarmSource</code> is built.
	 * <P>
	 * Note that the component name is used as a key to return the same {@link AlarmSource} 
	 * for the same component so in principle the parameter <code>componentName</code> 
	 * can be freely set. 
	 * 
	 * @param componentName The not <code>null</code> and not empty name of the component.
	 * @return the {@link AlarmSource}
	 */
	public AlarmSource getAlarmSource(String componentName) {
		if (closed) {
			throw new IllegalStateException("The AlarmSourceFactory has already been closed!");
		}
		if (componentName==null | componentName.isEmpty()) {
			throw new IllegalArgumentException("The name can't be null nor empty");
		}
		
		AlarmSource ret = alarmSources.get(componentName);
		if (ret==null) {
			contSvcs.getLogger().log(AcsLogLevel.DELOUSE, "Instantiating the AlarmSource for "+componentName);
			ret = new AlarmSourceImpl(contSvcs);
			alarmSources.put(componentName, ret);
			ret.start();
		} 
		return ret;
	}
	
	/**
	 * Release the {@link AlarmSource} object of the component whose name is read from
	 * passed the {@link ContainerServicesBase}.
	 * 
	 * @param containerServices
	 */
	public void releaseAlarmSource(ContainerServicesBase containerServices) {
		if (containerServices==null) {
			throw new IllegalArgumentException("The ContainerServices can't be null");
		}
		releaseAlarmSource(containerServices.getName());
	}
	
	/**
	 * Release the {@link AlarmSource} object of the component whose name is read from
	 * passed the {@link ContainerServicesBase}.
	 * 
	 * @param componentName The name of the component
	 */
	public void releaseAlarmSource(String componentName) {
		if (closed) {
			throw new IllegalStateException("The AlarmSourceFactory has already been closed!");
		}
		if (componentName==null | componentName.isEmpty()) {
			throw new IllegalArgumentException("The name can't be null nor empty");
		}
		
		AlarmSource alarmSrc=alarmSources.remove(componentName);
		if (alarmSrc!=null) {
			contSvcs.getLogger().log(AcsLogLevel.DELOUSE, "Releasing the AlarmSource of "+componentName);
			// Flush all the alarms.. frees resources like threads and so on
			alarmSrc.tearDown();
		}
	}
	
	public void tearDown() {
		if (closed) {
			contSvcs.getLogger().log(AcsLogLevel.WARNING, "AlarmSourceFactory already closed");
			return;
		}
		closed=true;
		contSvcs.getLogger().log(AcsLogLevel.DELOUSE, "Closing the AlarmSourceFactory");
		Set<String>sourceNames=alarmSources.keySet();
		for (String sourceName: sourceNames) {
			AlarmSource source=alarmSources.get(sourceName);
			if (source!=null) {
				contSvcs.getLogger().log(AcsLogLevel.DELOUSE, "Closing AlarmSource with name "+sourceName);
				source.tearDown();
			}
		}
		alarmSources.clear();
		contSvcs.getLogger().log(AcsLogLevel.DEBUG, "AlarmSourceFactory closed");
	}
}