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
import alma.acs.service.ChannelStats;




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
		public ChannelStats stats;
		
		public ChannelInfo() {
			status = Status.UNKNOWN;
			stats = new ChannelStats();
		}

		public ChannelInfo(Status status) {
			this.status = status;
			stats = new ChannelStats();
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
	 * Statistics acquisition frequency in milliseconds
	 */
	static final long ACQUISITION_FREQUENCY = 1 * ServiceParameters.MIN_2_MS; // 1min 
	
	/**
	 * Main loop frequency in milliseconds
	 */
	static final long MAIN_LOOP_FREQUENCY = 1000; // 1s

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
					//System.out.println("---<<PAU>>----------------------- Init status => Channel name: " + channels[i]); // TODO delete it
					serviceInfo.channels.put(service + "#" + channels[i], new ChannelInfo());
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

	protected void updateStatus(List<NotifyServiceData> services,List<ChannelData> channels) {
		NotifyServiceData service = null;
		ChannelData channel = null;

		// Iterate services passed as input parameter to update their status
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

		// Iterate channels passed as input parameter to update their status
		for(Iterator<ChannelData> it = channels.iterator();it.hasNext();) {
			channel = it.next();
			String channelName = channel.getQualifiedName();
			String serviceName = channel.getParent().getName();
			//System.out.println("channelName: " + channelName + ",   serviceName: " + serviceName); // TODO delete it
			ServiceInfo serviceInfo = null;
			if(status.containsKey(serviceName)) {
				serviceInfo = status.get(serviceName);
			
			// Status of the service of the current channel doesn't exist so we create it	
			} else {
				serviceInfo = new ServiceInfo(Status.ENABLED);
				status.put(serviceName, serviceInfo);
				logger.warning("Notify Service '"+serviceName+"' is running");
			}
			
			ChannelInfo channelInfo = null;
			if(serviceInfo.channels.containsKey(channelName)) {
				//System.out.println("Service info " + serviceName + " already contains channel " + channelName); // TODO delete it
				channelInfo = serviceInfo.channels.get(channelName);
				if(Status.DISABLED == channelInfo.status) {
					channelInfo.status = Status.ENABLED;
				} else if(Status.UNKNOWN == channelInfo.status) {
					channelInfo.status = Status.ENABLED;
				}
			} else {
				//System.out.println("Service info " + serviceName + " doesn't contain channel " + channelName); // TODO delete it
				channelInfo = new ChannelInfo(Status.ENABLED);
				serviceInfo.channels.put(channelName, channelInfo);
			}
			// Update channel statistics
			channelInfo.stats.setData(channel);
		}

		// Iterate services and channels already registered to disable the ones
		// that are not found
		Set<String> registeredServices = status.keySet();
		for(Iterator<String> it = registeredServices.iterator();it.hasNext();) {
			String serviceName = it.next();
			ServiceInfo serviceInfo = status.get(serviceName);
			boolean found = false;
			
			for(Iterator<NotifyServiceData> it2 = services.iterator();it2.hasNext() && false == found;) {
				service = it2.next();
				if(service.getName().equals(serviceName)) {
					found = true;
				}
			}
			
			if(false == found && Status.ENABLED == serviceInfo.status) {
				logger.warning("Notify Service '" + serviceName + "' has been stopped");
				serviceInfo.status = Status.DISABLED;
			}

			Set<String> registeredChannels = serviceInfo.channels.keySet();
			for(Iterator<String> itc = registeredChannels.iterator();itc.hasNext();) {
				String channelName = itc.next();
				ChannelInfo channelInfo = serviceInfo.channels.get(channelName);
				found = false;

				for(Iterator<ChannelData> itc2 = channels.iterator();itc2.hasNext() && false == found;) {
					channel = itc2.next();
					if(channel.getQualifiedName().equals(channelName)) {
						found = true;
					}
				}

				if(false == found && Status.ENABLED == channelInfo.status) {
					channelInfo.status = Status.DISABLED;
				}
			}
		}
	}

	protected void logStatus() {
		Set<String> registeredServices = status.keySet();
		for(Iterator<String> it = registeredServices.iterator();it.hasNext();) {
			String serviceName = it.next();
			ServiceInfo serviceInfo = status.get(serviceName);
			//System.out.println("SERVICE " + serviceName + " has " + String.valueOf(serviceInfo.channels.size()) + " channels"); // TODO delete it
			Set<String> registeredChannels = serviceInfo.channels.keySet();
			String chsNames = "";	
			int nChannels = 0;
			for(Iterator<String> itc = registeredChannels.iterator();itc.hasNext();) {
				String channelName = itc.next();
				//System.out.println("<<PAU>>--------------------------------- logStatus: Channel name is: " + channelName + " and service is " + serviceName); // TODO delete it
				ChannelInfo channelInfo = serviceInfo.channels.get(channelName);
				//if(channelInfo.status == Status.ENABLED) { 
					Map<String,String> values = channelInfo.stats.getInfoParams(params);
					logger.log(AcsLogLevel.INFO, 
						"STATISTICS OF NOTIFICATION CHANNEL " + channelName, values);
					values = channelInfo.stats.getDbgParams();
					logger.log(AcsLogLevel.DEBUG, 
						"STATISTICS OF NOTIFICATION CHANNEL " + channelName, values);
				//}
				if(channelInfo.status == Status.ENABLED) {
					chsNames += channelName + ", ";
					nChannels += 1;
				}
			}

			HashMap<String,String> params = new HashMap<String,String>();
			if(nChannels > 0) {
				params.put("Active event channels", chsNames);
			}
			params.put("Num active event channels", String.valueOf(nChannels));
			logger.log(AcsLogLevel.INFO,
				"STATISTICS OF NOTIFICATION FACTORY " + serviceName, params);
		}
	}
	
	/**
	 * Update status of services
	 * @param services
	 */
/*	protected void updateServicesStatus(List<NotifyServiceData> services) {
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
		//for(Iterator<String> it = registeredServices.iterator();it.hasNext();) {
		//	String serviceName = it.next();
		//	ServiceInfo serviceInfo = status.get(serviceName);
		//	String status = "";
		//	if(serviceInfo.status == Status.ENABLED) {
		//		status = "ENABLED";
		//	} else if(serviceInfo.status == Status.DISABLED) {
		//		status = "DISABLED";
		//	} else if(serviceInfo.status == Status.UNKNOWN) {
		//		status = "UNKNOWN";
		//	}
		//	logger.info("SERVICE " + serviceName + " STATUS: " + status);
		//}
	}*/
	
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

		long acqFreq = ACQUISITION_FREQUENCY; // 1min
		long logFreq = params.getFrequency(); // at least 1min		


		if(logFreq < acqFreq) {
			logger.log(AcsLogLevel.WARNING, 
				"Statistics of Notification Services will be obtained with a very high frequency (less than 1 minute time interval)");
			acqFreq = logFreq;
		}

		logger.log(AcsLogLevel.INFO, "Acquisition frequency: " + String.valueOf(acqFreq) + "ms");
		logger.log(AcsLogLevel.INFO, "Log frequency: " + String.valueOf(logFreq) + "ms");

		acqFreq = acqFreq / MAIN_LOOP_FREQUENCY;
		logFreq = logFreq / MAIN_LOOP_FREQUENCY;
		
		boolean nsExists = false;
		long counter = 0;
		while(stop == false) {
			try {
				if(counter % acqFreq == 0) // Time to get statistics from the channels
				{
					nsExists = eventModel.getChannelStatistics();
					if(false == nsExists) {
						setServicesStatus(Status.UNKNOWN);
						if(eventModel.reconnectNamingService() == false) {
							logger.log(AcsLogLevel.ERROR,"Naming Service is unreachable");
						} else {
							nsExists = true;
						}
					// Calculate statistics
					} else {
						// Get notify services 
						NotifyServices ns = eventModel.getNotifyServicesRoot();
						// Get services to be treated
						List<NotifyServiceData> services = getCurrentServices();
						// Get channels to be treated
						List<ChannelData> channels = getCurrentChannels(ns);
						// Update status of services and channels
						updateStatus(services, channels);
					} 
				}
				
				// It's time to log statistics
				if(true == nsExists && (counter+1) % logFreq == 0) {
					// Log services and channels statistics
					logStatus();

					// After logging statistics, we have to reset them
					resetStats();
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
				++counter;
				sleep(MAIN_LOOP_FREQUENCY);
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
	/*
	protected void logFactoryStatistics(NotifyServiceData service) {
		//String str = "Factory " + service.getName() + " with ";
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
	}*/
	
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

	protected void resetStats() {
		Iterator services = status.entrySet().iterator();
		while(services.hasNext()) {
			Map.Entry<String,ServiceInfo> entry = (Map.Entry<String,ServiceInfo>)services.next();
			Iterator channels = entry.getValue().channels.entrySet().iterator();
			while(channels.hasNext()) {
				Map.Entry<String,ChannelInfo> chEntry = (Map.Entry<String,ChannelInfo>)channels.next();
				chEntry.getValue().stats.reset();
			}
		}
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
		
		if(exec) {
			try {
				final EventModel eventModel = EventModel.getInstance();
				Logger logger = eventModel.getLogger();
				params.log(logger);
				
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
						
			} catch(Throwable thr) {
				logErrors(errors,true,null);
				thr.printStackTrace();
			}
		} else {
			logErrors(errors,true,null);
		}
	}
}
