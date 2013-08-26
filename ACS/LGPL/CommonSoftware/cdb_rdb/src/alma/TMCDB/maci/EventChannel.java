/*
 * ALMA - Atacama Large Millimeter Array
 * (c) European Southern Observatory, 2002
 * (c) Associated Universities Inc., 2002
 * Copyright by ESO (in the framework of the ALMA collaboration),
 * Copyright by AUI (in the framework of the ALMA collaboration),
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY, without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 * MA 02111-1307  USA
 * File Container.java
 */
package alma.TMCDB.maci;

import java.util.HashMap;
import java.util.Map;

import com.cosylab.cdb.jdal.hibernate.RootNameOverrideFeature;

@SuppressWarnings("serial")
public class EventChannel extends EventChannelNode implements RootNameOverrideFeature {

    @SuppressWarnings("unused")
	private int EventChannelId;
    @SuppressWarnings("unused")
	private int ConfigurationId;

    public String Name;
    
    private boolean IntegrationLogs = false;
    private int MaxQueueLength = 0;
    private int MaxConsumers = 0;
    private int MaxSuppliers = 0;
    private boolean RejectNewEvents = false;
    private String DiscardPolicy = "AnyOrder";
    private String EventReliability = "BestEffort";
    private String ConnectionReliability = "BestEffort";
    private short Priority = 0;
    private int Timeout = 0;
    private String OrderPolicy = "AnyOrder";
    private boolean StartTimeSupported = false;
    private boolean StopTimeSupported = false;
    private int MaxEventsPerConsumer = 0;
    
    private Map<String, Event> Events = new HashMap<String, Event>();
    
    // hierarchical support
    // must be public to be accessible, but should not have getter to be come visible as node
    public String Path;

    /* (non-Javadoc)
	 * @see com.cosylab.cdb.jdal.hibernate.RootNameOverrideFeature#getRootNameOverride()
	 */
	public String getRootNameOverride() {
		return "EventChannel";
	}

	/**
     * Default Constructor for Component.  Setter methods must be used to insert data.
     */
    public EventChannel () {
    }

	/**
	 * @return the name
	 */
	public String getName() {
		return Name;
	}

	/**
	 * @param name the name to set
	 */
	public void setName(String name) {
		Name = name;
	}

	/**
	 * @return the integrationLogs
	 */
	public boolean isIntegrationLogs() {
		return IntegrationLogs;
	}

	/**
	 * @param integrationLogs the integrationLogs to set
	 */
	public void setIntegrationLogs(boolean integrationLogs) {
		IntegrationLogs = integrationLogs;
	}

	/**
	 * @return the maxQueueLength
	 */
	public int getMaxQueueLength() {
		return MaxQueueLength;
	}

	/**
	 * @param maxQueueLength the maxQueueLength to set
	 */
	public void setMaxQueueLength(int maxQueueLength) {
		MaxQueueLength = maxQueueLength;
	}

	/**
	 * @return the maxConsumers
	 */
	public int getMaxConsumers() {
		return MaxConsumers;
	}

	/**
	 * @param maxConsumers the maxConsumers to set
	 */
	public void setMaxConsumers(int maxConsumers) {
		MaxConsumers = maxConsumers;
	}

	/**
	 * @return the maxSuppliers
	 */
	public int getMaxSuppliers() {
		return MaxSuppliers;
	}

	/**
	 * @param maxSuppliers the maxSuppliers to set
	 */
	public void setMaxSuppliers(int maxSuppliers) {
		MaxSuppliers = maxSuppliers;
	}

	/**
	 * @return the rejectNewEvents
	 */
	public boolean isRejectNewEvents() {
		return RejectNewEvents;
	}

	/**
	 * @param rejectNewEvents the rejectNewEvents to set
	 */
	public void setRejectNewEvents(boolean rejectNewEvents) {
		RejectNewEvents = rejectNewEvents;
	}

	/**
	 * @return the discardPolicy
	 */
	public String getDiscardPolicy() {
		return DiscardPolicy;
	}

	/**
	 * @param discardPolicy the discardPolicy to set
	 */
	public void setDiscardPolicy(String discardPolicy) {
		DiscardPolicy = discardPolicy;
	}

	/**
	 * @return the eventReliability
	 */
	public String getEventReliability() {
		return EventReliability;
	}

	/**
	 * @param eventReliability the eventReliability to set
	 */
	public void setEventReliability(String eventReliability) {
		EventReliability = eventReliability;
	}

	/**
	 * @return the connectionReliability
	 */
	public String getConnectionReliability() {
		return ConnectionReliability;
	}

	/**
	 * @param connectionReliability the connectionReliability to set
	 */
	public void setConnectionReliability(String connectionReliability) {
		ConnectionReliability = connectionReliability;
	}

	/**
	 * @return the priority
	 */
	public short getPriority() {
		return Priority;
	}

	/**
	 * @param priority the priority to set
	 */
	public void setPriority(short priority) {
		Priority = priority;
	}

	/**
	 * @return the timeout
	 */
	public int getTimeout() {
		return Timeout;
	}

	/**
	 * @param timeout the timeout to set
	 */
	public void setTimeout(int timeout) {
		Timeout = timeout;
	}

	/**
	 * @return the orderPolicy
	 */
	public String getOrderPolicy() {
		return OrderPolicy;
	}

	/**
	 * @param orderPolicy the orderPolicy to set
	 */
	public void setOrderPolicy(String orderPolicy) {
		OrderPolicy = orderPolicy;
	}

	/**
	 * @return the startTimeSupported
	 */
	public boolean isStartTimeSupported() {
		return StartTimeSupported;
	}

	/**
	 * @param startTimeSupported the startTimeSupported to set
	 */
	public void setStartTimeSupported(boolean startTimeSupported) {
		StartTimeSupported = startTimeSupported;
	}

	/**
	 * @return the stopTimeSupported
	 */
	public boolean isStopTimeSupported() {
		return StopTimeSupported;
	}

	/**
	 * @param stopTimeSupported the stopTimeSupported to set
	 */
	public void setStopTimeSupported(boolean stopTimeSupported) {
		StopTimeSupported = stopTimeSupported;
	}

	/**
	 * @return the maxEventsPerConsumer
	 */
	public int getMaxEventsPerConsumer() {
		return MaxEventsPerConsumer;
	}

	/**
	 * @param maxEventsPerConsumer the maxEventsPerConsumer to set
	 */
	public void setMaxEventsPerConsumer(int maxEventsPerConsumer) {
		MaxEventsPerConsumer = maxEventsPerConsumer;
	}


	/**
	 * @return the events
	 */
	public Map<String, Event> getEvents() {
		return Events;
	}

	/**
	 * @param events the events to set
	 */
	public void setEvents(Map<String, Event> events) {
		Events = events;
	}

    
}

