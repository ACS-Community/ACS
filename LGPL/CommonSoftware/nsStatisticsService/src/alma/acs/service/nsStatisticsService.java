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
package alma.acs.service;

import gov.sandia.CosNotification.NotificationServiceMonitorControl;
import gov.sandia.CosNotification.NotificationServiceMonitorControlPackage.InvalidName;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.BlockingQueue;
import java.util.logging.Logger;

import alma.acs.exceptions.AcsJException;
import alma.acs.logging.AcsLogLevel;
import alma.acs.nsstatistics.ChannelData;
import alma.acs.nsstatistics.EventData;
import alma.acs.nsstatistics.EventModel;
import alma.acs.nsstatistics.MCStatistics;
import alma.acs.nsstatistics.NotifyServiceData;
import alma.acs.nsstatistics.NotifyServices;




/**
 * @author pcolomer
 *
 * $Id: ncStatisticsService.java,v 1.39 2014/11/17 15:36:44 pcolomer Exp $
 */
public class nsStatisticsService extends Thread {
	
	/**
	 * Possible status of services and channels
	 */
	protected enum Status {
		ENABLED, 
		DISABLED, 
		UNKNOWN
	}
	
	
	class ChannelInfo {
		public Status status;
		
		public ChannelInfo() {
			status = Status.UNKNOWN;
		}
	}
	
	class ServiceInfo {
		public Status status;
		public HashMap<String,ChannelInfo> channels;
		
		public ServiceInfo() {
			status = Status.UNKNOWN;
			channels = new HashMap<String,ChannelInfo>();
		}
		
		public ServiceInfo(Status status) {
			this.status = status;
			channels = new HashMap<String,ChannelInfo>();
		}
	}

	/**
	 * Tool name used to identify log messages
	 */
	static final String TOOL_NAME = "nsStatisticsService";
	
	/**
	 * Event model used to get services & channels statistics
	 */
	protected EventModel eventModel;
	
	/**
	 * Command line parameters
	 */
	protected ServiceParameters params;
	
	/**
	 * Logger
	 */
	protected Logger logger;
	
	/**
	 * Attribute used to decide when to stop the execution of this tool
	 */
	protected boolean stop;
	
	/**
	 * Status of services and channels
	 */
	protected HashMap<String,ServiceInfo> status;
	
	/**
	 * Constructor
	 * @param eventModel Event model used to get statistics of services and channels
	 * @param params Command line parameters
	 */
	public nsStatisticsService(EventModel eventModel,ServiceParameters params) {
		this.params = params;
		this.eventModel = eventModel;
		this.logger = eventModel.getLogger();
		this.stop = false;
		initStatus();
	}
	
	protected void initStatus() {
		status = new HashMap<String,ServiceInfo>();
		HashMap<String,String[]> data = params.getSelectedServicesChannels();
		Set<String> services = data.keySet();
		for(Iterator<String> it = services.iterator();it.hasNext();) {
			String service = it.next();
			String [] channels = data.get(service);
			ServiceInfo serviceInfo = new ServiceInfo();
			if(channels != null) {
				for(int i = 0;i < channels.length;++i) {
					serviceInfo.channels.put(channels[i], new ChannelInfo());
				}
			}
			status.put(service, serviceInfo);
		}
	}
	
	/**
	 * Set the status passed as parameter to all services
	 * @param status
	 */
	protected void setServicesStatus(Status status) {
		Set<String> registeredServices = this.status.keySet();
		for(Iterator<String> it = registeredServices.iterator();it.hasNext();) {
			String serviceName = it.next();
			ServiceInfo serviceInfo = this.status.get(serviceName);
			if(serviceInfo.status != status) {
				serviceInfo.status = status;
				if(Status.ENABLED == status) {
					logger.warning("Notify Service '" + serviceName + "' is running");
				} else if(Status.DISABLED == status) {
					logger.warning("Notify Service '" + serviceName + "' has been stopped");
				} else if(Status.UNKNOWN == status) {
					logger.warning("Notify Service '" + serviceName + "' status is unknown");
				}
			}
		}
	}
	
	/**
	 * Update status of services
	 * @param services
	 */
	protected void updateServicesStatus(List<NotifyServiceData> services) {
		NotifyServiceData service = null;
		
		// Iterate services got from the EventModel
		for(Iterator<NotifyServiceData> it = services.iterator();it.hasNext();) {				
			service = it.next();
			
			// Status of the current service already exists
			if(status.containsKey(service.getName())) {
				ServiceInfo serviceInfo = status.get(service.getName());
				
				if(Status.DISABLED == serviceInfo.status) {
					logger.warning("Notify Service '"+service.getName()+"' has been restarted");
					serviceInfo.status = Status.ENABLED;
				} else if(Status.UNKNOWN == serviceInfo.status) {
					logger.warning("Notify Service '"+service.getName()+"' is running");
					serviceInfo.status = Status.ENABLED;
				}
			// Status of the current service didn't exist so we create it
			} else {
				status.put(service.getName(), new ServiceInfo(Status.ENABLED));
				logger.warning("Notify Service '"+service.getName()+"' is running");
			}
		}
		
		// Iterate services already registered
		Set<String> registeredServices = status.keySet();
		for(Iterator<String> it = registeredServices.iterator();it.hasNext();) {
			String serviceName = it.next();
			ServiceInfo serviceInfo = status.get(serviceName);
			boolean found = false;
			
			for(Iterator<NotifyServiceData> it2 = services.iterator();it2.hasNext() && false == found;) {
				service = it2.next();
				
				//logger.info("----------------------? " + service.getName() + " == " + serviceName);
				if(service.getName().equals(serviceName)) {
					found = true;
				}
			}
			
			if(false == found && Status.ENABLED == serviceInfo.status) {
				logger.warning("Notify Service '" + serviceName + "' has been stopped");
				serviceInfo.status = Status.DISABLED;
			}
		}
		
		// Log status of all services 
		/*
		for(Iterator<String> it = registeredServices.iterator();it.hasNext();) {
			String serviceName = it.next();
			ServiceInfo serviceInfo = status.get(serviceName);
			String status = "";
			if(serviceInfo.status == Status.ENABLED) {
				status = "ENABLED";
			} else if(serviceInfo.status == Status.DISABLED) {
				status = "DISABLED";
			} else if(serviceInfo.status == Status.UNKNOWN) {
				status = "UNKNOWN";
			}
			logger.info("SERVICE " + serviceName + " STATUS: " + status);
		}*/
	}
	
	/**
	 * Should statistics of channel channelName be logged?  
	 * @param serviceName Name of service which owns the channel channelName
	 * @param channelName Name of channel
	 * @return 
	 */
	protected boolean shouldLogChannel(String serviceName,String channelName) {
		boolean log = false;
		String [] selChannels = null;
		HashMap<String,String[]> selServicesChannels = params.getSelectedServicesChannels();
		if(selServicesChannels.containsKey(serviceName)) {
			selChannels = selServicesChannels.get(serviceName);
			if(selChannels.length == 0) {
				log = true;
			} else {
				for(int i = 0;i < selChannels.length;++i) {
					if(selChannels[i].equals(channelName)) {
						log = true;
					}
				}
			}
		} else if(selServicesChannels.size() == 0) {
			log = true;
		}
		return log;
	}
	
	/**
	 * Get current services to be treated
	 * @return List of services
	 */
	protected List<NotifyServiceData> getCurrentServices() {
		NotifyServices ns = eventModel.getNotifyServicesRoot();
		List<NotifyServiceData> selServices = new ArrayList<NotifyServiceData>();
		List<NotifyServiceData> services = ns.getServices();
		if(params.getSelectedServicesChannels().isEmpty()) {
			return services;
		} else {
			NotifyServiceData service = null;
			for(Iterator<NotifyServiceData> it = services.iterator();it.hasNext();) {				
				service = it.next();
				if(params.getSelectedServicesChannels().containsKey(service.getName())) {
					selServices.add(service);
				}
			}
		}
		return selServices;
	}
	
	/**
	 * Get current channels to be treated
	 * @param services
	 */
	protected List<ChannelData> getCurrentChannels(NotifyServices ns) {
		ChannelData channel = null;
		List<ChannelData> channels = ns.getAllChannels();
		List<ChannelData> selChannels = new ArrayList<ChannelData>();
		for(Iterator<ChannelData> it = channels.iterator();it.hasNext();) {
			channel = it.next();
			
			if(shouldLogChannel(channel.getParent().getName(), channel.getName())) {
				selChannels.add(channel);
			}
		}
		return selChannels;
	}
	

	/**
	 * 
	 */
	public void run() {
		
		if(params.getFrequency() < ServiceParameters.MIN_2_MS) {
			logger.log(AcsLogLevel.WARNING, 
				"Statistics of Notification Services will be obtained with a very high frequency (less than 1 minute time interval)");
		}
		
		while(stop == false) {			
			try {
				boolean nsExists = eventModel.getChannelStatistics();
				if(false == nsExists) {
					setServicesStatus(Status.UNKNOWN);
					if(eventModel.reconnectNamingService() == false) {
						logger.log(AcsLogLevel.ERROR,"Naming Service is unreachable");
					} else {
						nsExists = true;
					}
				} 
				
				if(true == nsExists) {
					NotifyServices ns = eventModel.getNotifyServicesRoot();
					
					// Get services to be treated
					List<NotifyServiceData> services = getCurrentServices();
					
					// Update status of services
					updateServicesStatus(services);
					
					// Log statistics of each service
					for(Iterator<NotifyServiceData> it = services.iterator();it.hasNext();) {				
						logFactoryStatistics(it.next());
					}
					
					// Get channels to be treated
					List<ChannelData> channels = getCurrentChannels(ns);
					
					// Log statistics of each channel
					for(Iterator<ChannelData> it = channels.iterator();it.hasNext();) {
						logChannelStatistics(it.next());
					}
				}
			} catch(Exception e) {
				logger.warning("Notification Service doesn't exist!");
				String str = "";
				StackTraceElement [] stack = e.getStackTrace();
				for(int k = 0;k < stack.length;++k) {
					str += stack[k] + "\n";
				}
				logger.warning(e.getMessage() + "\n" + str);
			}

			try {
				sleep(params.getFrequency());
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
		
		logger.info("nsStatisticsService thread has been finished");
	}
	
	/**
	 * Stop the execution
	 */
	public void stopIt() {
		this.stop = true;
	}
	
	/**
	 * 
	 * @param service
	 */
	protected void logFactoryStatistics(NotifyServiceData service) {
		//String str = "Factory " + service.getName() /*service.getFactoryName()*/ + " with ";
		String [] activeChannelNames = null;
		int activeChannelsCount = 0;
		NotificationServiceMonitorControl nsm = service.getMc();
		String [] statsNames = nsm.get_statistic_names();

		Map<String,String> infoParams = new HashMap<String, String>();
		//String infInfo = "\t\tSTATISTICS OF NOTIFICATION FACTORY " + service.getName() + "\n";
		
		for(int i = 0;i < statsNames.length;++i) {
			try {
				if(statsNames[i].contains("ActiveEventChannelCount")) {
					Monitor.Numeric n = nsm.get_statistic(statsNames[i]).data_union.num();
					activeChannelsCount = (int)n.last;
				}
				else if(statsNames[i].contains("ActiveEventChannelNames")) {
					activeChannelNames = nsm.get_statistic(statsNames[i]).data_union.list();
				}
			} catch(InvalidName ex) {
				logger.log(AcsLogLevel.ERROR, "Invalid name in ncStatisticsService::logService", ex);
			}
		}
		
		infoParams.put("Active event channels", getListString(activeChannelNames));
		infoParams.put("Num active event channels", String.valueOf(activeChannelsCount));
		//infInfo += "\tActive event channels [" + String.valueOf(activeChannelsCount) + "]\n";
		//infInfo += getListStringDiffLines("\t\t", activeChannelNames);
		
		logger.log(AcsLogLevel.INFO, 
				"STATISTICS OF NOTIFICATION FACTORY " + service.getName(), infoParams);
	}
	
	protected String getListString(String [] list) {
		String str = "";
		if(list != null)
		{
			for(int i = 0;i < (list.length - 1);++i) {
				str += list[i] + ", ";
			}
			if(list.length > 0) {
				str += list[list.length-1];
			}
		}
		return str;
	}
	
	protected String getListStringDiffLines(String prefixLine,String [] list) {
		String str = "";
		if(list != null)
		{
			for(int i = 0;i < list.length;++i) {
				str += prefixLine + list[i] + "\n";
			}
		}
		return str;
	}
		
	protected String strArray(ArrayList<Integer> list) {
		String str = "";
		if(list != null)
		{
			for(int i = 0;i < (list.size() - 1);++i) {
				str += String.valueOf(list.get(i)) + ", ";
			}
			if(list.size() > 0) {
				str += list.get(list.size()-1);
			}
		}
		return str;
	}
	
	protected int maxArray(ArrayList<Integer> list) {
		int maxVal = -1;
		for(Iterator<Integer> it = list.iterator();it.hasNext();) {
			Integer item = it.next();
			if(item.intValue() > maxVal)
			{
				maxVal = item.intValue();
			}
		}
		return maxVal;
	}
	
	protected void logChannelStatistics(ChannelData channel) {
		NotificationServiceMonitorControl nsm = channel.getParent().getMc();
		String [] statsNames = nsm.get_statistic_names();
		
		String channelName = "Channel " + channel.getName() + " - ";
		int numSuppliers = -1;
		int numConsumers = -1;
		ArrayList<Integer> queueSize = new ArrayList<Integer>();
		ArrayList<Integer> queueElementCount = new ArrayList<Integer>();
		long oldestEvent = -1;
		String [] slowestConsumers = null;
		String [] supplierNames = null;
		String [] consumerNames = null;
		String [] supplierAdminNames = null;
		String [] consumerAdminNames = null;
		
		for(int i = 0;i < statsNames.length;++i) {
			//if(false == statsNames[i].contains(channel.getName())) {
				//logger.log(AcsLogLevel.INFO, "Channel discarded because " + statsNames[i] + " not includes "+ channel.getName());
				//continue;
			//}
			try {
				if(statsNames[i].contains("ConsumerCount")) {
					Monitor.Numeric n = nsm.get_statistic(statsNames[i]).data_union.num();
					numConsumers = (int)n.last;
				} else if(statsNames[i].contains("SupplierCount")) {
					Monitor.Numeric n = nsm.get_statistic(statsNames[i]).data_union.num();
					numSuppliers = (int)n.last;
				} else if(statsNames[i].contains("QueueSize")) {
					// n.count, n.maximum, n.last, n.average, n.dlist[0].value, n.dlist[0].timestamp
					Monitor.Numeric n = nsm.get_statistic(statsNames[i]).data_union.num();
					queueSize.add((int)n.last);
					//queueSize = (int)n.last;
				} else if(statsNames[i].contains("QueueElementCount")) {
					Monitor.Numeric n = nsm.get_statistic(statsNames[i]).data_union.num();
					/*logger.info(statsNames[i] + ": max=" + String.valueOf(n.maximum) + ", dlist[0].timestamp=" 
							+ String.valueOf(n.dlist[0].timestamp)+", last="+ String.valueOf(n.last) + ", avg=" 
							+ String.valueOf(n.average) + ", dlist[0].value=" + String.valueOf(n.dlist[0].value));*/
					queueElementCount.add((int)n.last);
					//queueElementCount = (int)n.last;
				} else if(statsNames[i].contains("OldestEvent")) {
					Monitor.Numeric n = nsm.get_statistic(statsNames[i]).data_union.num();
					/*logger.info(statsNames[i] + ": max=" + String.valueOf(n.maximum) + ", dlist[0].timestamp=" 
							+ String.valueOf(n.dlist[0].timestamp)+", last="+ String.valueOf(n.last) + ", avg=" 
							+ String.valueOf(n.average) + ", dlist[0].value=" + String.valueOf(n.dlist[0].value));*/
					oldestEvent = (long)n.last;
				} else if(statsNames[i].contains("SlowestConsumers")) {
					slowestConsumers = nsm.get_statistic(statsNames[i]).data_union.list();
				} else if(statsNames[i].contains("SupplierNames")) {
					supplierNames = nsm.get_statistic(statsNames[i]).data_union.list();
				} else if(statsNames[i].contains("ConsumerNames")) {
					consumerNames = nsm.get_statistic(statsNames[i]).data_union.list();
				} else if(statsNames[i].contains("SupplierAdminNames")) {
					supplierAdminNames = nsm.get_statistic(statsNames[i]).data_union.list();
				} else if(statsNames[i].contains("ConsumerAdminNames")) {
					consumerAdminNames = nsm.get_statistic(statsNames[i]).data_union.list();
				}
							
			} catch(InvalidName ex) {
				logger.log(AcsLogLevel.ERROR, "Invalid name in ncStatisticsService::logMCStats", ex);
			}
		}
		
		Map<String,String> infoParams = new HashMap<String,String>();
		//String infInfo = "\t\tSTATISTICS OF NOTIFICATION CHANNEL " + channel.getName() + "\n";
		//infInfo += "\tThere are " + String.valueOf(numSuppliers) + " suppliers, " 
		//		+ String.valueOf(numConsumers) + " consumers\n";
		//infInfo += "\tNumber of events in queues: " + strArray(queueElementCount) + "\n";

		infoParams.put("Num suppliers", String.valueOf(numSuppliers));
		infoParams.put("Num consumers", String.valueOf(numConsumers));
		infoParams.put("Num events in queues", strArray(queueElementCount));
		
		if(maxArray(queueSize) >= params.getThQueueSize())
		{
			//infInfo += "\tSize of queues in bytes: " + strArray(queueSize) + "\n";
			infoParams.put("Size of queues [bytes]", strArray(queueSize));
		}
		if(maxArray(queueElementCount) >= params.getThOldestEvent()) {
			//infInfo += "\tOldest event: " + String.valueOf(oldestEvent) + "\n";
			infoParams.put("Oldest event", String.valueOf(oldestEvent));
		}
		if(slowestConsumers != null && slowestConsumers.length > 0) {
			//infInfo += "\tSlowest consumers: " + getListString(slowestConsumers) + "\n";
			infoParams.put("Slowest consumers", getListString(slowestConsumers));
		}
		
		
		logger.log(AcsLogLevel.INFO, 
				"STATISTICS OF NOTIFICATION CHANNEL " + channel.getName(), infoParams);
		
		Map<String,String> dbgParams = new HashMap<String, String>();
		dbgParams.put("Num suppliers", String.valueOf(supplierNames.length));
		dbgParams.put("Num consumers", String.valueOf(consumerNames.length));
		dbgParams.put("Num admin suppliers", String.valueOf(supplierAdminNames.length));
		dbgParams.put("Num admin consumers", String.valueOf(consumerAdminNames.length));		
		dbgParams.put("Supplier names", getListString(supplierNames));
		dbgParams.put("Consumer names", getListString(consumerNames));
		dbgParams.put("Supplier admin names", getListString(supplierAdminNames));
		dbgParams.put("Consumer admin names", getListString(consumerAdminNames));
		
		logger.log(AcsLogLevel.DEBUG, 
				"DEBUG STATISTICS OF NOTIFICATION CHANNEL " + channel.getName(), dbgParams);
	}
		
	public static void logErrors(List<String> errors,boolean sysOutIt,Logger logger) {
		for(Iterator<String> it = errors.iterator();it.hasNext();) {
			String msgError = it.next();
			if(logger != null) {
				logger.log(AcsLogLevel.ERROR, msgError);
			}
			if(true == sysOutIt) {
				System.out.println(msgError);
			}
		}	
	}
	
	public static void main(String[] args) {
		
		ServiceParameters params = new ServiceParameters();
		List<String> errors = new ArrayList<String>();
		boolean exec = params.read(TOOL_NAME,args,errors);
		
		try {
			
			final EventModel eventModel = EventModel.getInstance();
			Logger logger = eventModel.getLogger();
			params.log(logger);
			
			if(exec)
			{
					final nsStatisticsService service = new nsStatisticsService(eventModel,params);
					
					Runtime.getRuntime().addShutdownHook(new Thread() {
						@Override
						public void run() {
							System.out.println("interrupt received, killing service ...");
							service.stopIt();
							try {
								//service.interrupt();
								service.join();
							} catch (InterruptedException e) {
							}
						}
					});
					
					service.start();
					logger.info("Service started ...");
					
					// TODO wait for input keys
					
			} else {
				logErrors(errors,true,logger);
				System.exit(1);
			}
		} catch(Throwable thr) {
			logErrors(errors,true,null);
			thr.printStackTrace();
		}
	}
}
