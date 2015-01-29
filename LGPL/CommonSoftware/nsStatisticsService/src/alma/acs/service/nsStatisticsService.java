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
import java.util.concurrent.BlockingQueue;
import java.util.logging.Logger;

import alma.acs.exceptions.AcsJException;
import alma.acs.logging.AcsLogLevel;
import alma.acs.ncstatistics.ChannelData;
import alma.acs.ncstatistics.EventData;
import alma.acs.ncstatistics.EventModel;
import alma.acs.ncstatistics.MCStatistics;
import alma.acs.ncstatistics.NotifyServiceData;
import alma.acs.ncstatistics.NotifyServices;




/**
 * @author pcolomer
 *
 * $Id: ncStatisticsService.java,v 1.39 2014/11/17 15:36:44 pcolomer Exp $
 */
public class nsStatisticsService extends Thread {

	static final String TOOL_NAME = "nsStatisticsService";
	
	protected EventModel eventModel;
	protected ServiceParameters params;
	protected Logger logger;
	protected boolean stop;
	
	public nsStatisticsService(EventModel eventModel,ServiceParameters params) {
		this.params = params;
		this.eventModel = eventModel;
		this.logger = eventModel.getLogger();
		this.stop = false;
	}
	
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
	 * 
	 */
	public void run() {
		
		while(stop == false) {
			logger.info("Getting Notify Services Statistics after " 
					+ String.valueOf(params.getFrequency()) + "ms");
			eventModel.getChannelStatistics();
			
			NotifyServices ns = eventModel.getNotifyServicesRoot();
			
			List<NotifyServiceData> services = ns.getServices();
			
			NotifyServiceData service = null;
			for(Iterator<NotifyServiceData> it = services.iterator();it.hasNext();) {				
				service = it.next();
				if(params.getSelectedServicesChannels().isEmpty() 
				|| params.getSelectedServicesChannels().containsKey(service.getName()))
				{
					logFactoryStatistics(service);
				}
			}
			
			ChannelData channel = null;
			List<ChannelData> channels = ns.getAllChannels();
			for(Iterator<ChannelData> it = channels.iterator();it.hasNext();) {
				channel = it.next();
				
				if(shouldLogChannel(channel.getParent().getName(), channel.getName())) {
					logChannelStatistics(channel);
				}
			}

			try {
				sleep(params.getFrequency());
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
		
		logger.info("nsStatisticsService thread has been finished");
	}
	
	public void stopIt() {
		this.stop = true;
	}
	
	protected void logFactoryStatistics(NotifyServiceData service) {
		//String str = "Factory " + service.getName() /*service.getFactoryName()*/ + " with ";
		String [] activeChannelNames = null;
		int activeChannelsCount = 0;
		NotificationServiceMonitorControl nsm = service.getMc();
		String [] statsNames = nsm.get_statistic_names();
		
		String infInfo = "\t\tSTATISTICS OF NOTIFICATION FACTORY " + service.getName() + "\n";
		
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
		
		infInfo += "\tActive event channels [" + String.valueOf(activeChannelsCount) + "]\n";
		infInfo += getListStringDiffLines("\t\t", activeChannelNames);
		
		logger.log(AcsLogLevel.INFO, infInfo);
		/*logger.log(AcsLogLevel.INFO, "Active event channels in " + service.getName() 
				+ "[" + String.valueOf(activeChannelsCount) + "]: " 
				+ getListString(activeChannelNames));*/
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
			if(false == statsNames[i].contains(channel.getName())) {
				//logger.log(AcsLogLevel.INFO, "Channel discarded because " + statsNames[i] + " not includes "+ channel.getName());
				//continue;
			}
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
		
		String infInfo = "\t\tSTATISTICS OF NOTIFICATION CHANNEL " + channel.getName() + "\n";
		infInfo += "\tThere are " + String.valueOf(numSuppliers) + " suppliers, " 
				+ String.valueOf(numConsumers) + " consumers\n";
		infInfo += "\tNumber of events in queues: " + strArray(queueElementCount) + "\n";
		
		//logger.log(AcsLogLevel.INFO, channelName + "There are " + String.valueOf(numSuppliers) + " suppliers, " 
		//			+ String.valueOf(numConsumers) + " consumers");
		//logger.log(AcsLogLevel.INFO, channelName + "Number of events in queues: " + strArray(queueElementCount));
		if(maxArray(queueSize) >= params.getThQueueSize())
		{
			//logger.log(AcsLogLevel.INFO, channelName + "Size of queues in bytes: " + strArray(queueSize));
			infInfo += "\tSize of queues in bytes: " + strArray(queueSize) + "\n";
		}
		if(maxArray(queueElementCount) >= params.getThOldestEvent()) {
			//logger.log(AcsLogLevel.INFO, channelName + "Oldest event: " + String.valueOf(oldestEvent));
			infInfo += "\tOldest event: " + String.valueOf(oldestEvent) + "\n";
		}
		if(slowestConsumers != null && slowestConsumers.length > 0) {
			//logger.log(AcsLogLevel.INFO, channelName + "Slowest consumers: " + getListString(slowestConsumers));
			infInfo += "\tSlowest consumers: " + getListString(slowestConsumers) + "\n";
		}
		
		logger.log(AcsLogLevel.INFO, infInfo);
		
		String dbgInfo = "\t\tDEBUG STATISTICS OF NOTIFICATION CHANNEL " + channel.getName() + "\n";
		
		//if(supplierNames != null && supplierNames.length > 0) {
			//logger.log(AcsLogLevel.DEBUG, channelName + "Supplier names: " + getListString(supplierNames));
			dbgInfo += "\tSupplier names [" + String.valueOf(supplierNames.length) + "]: " + getListString(supplierNames) + "\n";
		//}
		//if(consumerNames != null && consumerNames.length > 0) {
			//logger.log(AcsLogLevel.DEBUG, channelName + "Consumer names: " + getListString(consumerNames));
			dbgInfo += "\tConsumer names [" + String.valueOf(consumerNames.length) + "]: " + getListString(consumerNames) + "\n";
			//}
		//if(supplierAdminNames != null && supplierAdminNames.length > 0) {
			/*logger.log(AcsLogLevel.DEBUG, channelName + 
					"Supplier admin names [" + String.valueOf(supplierAdminNames.length) 
					+ "]: " + getListString(supplierAdminNames));*/
			dbgInfo += "\tSupplier admin names [" + String.valueOf(supplierAdminNames.length) 
					+ "]: " + getListString(supplierAdminNames) + "\n";
		//}
		//if(consumerAdminNames != null && consumerAdminNames.length > 0) {
		/*logger.log(AcsLogLevel.DEBUG, channelName + 
					"Consumer admin names [" + String.valueOf(consumerAdminNames.length) 
					+ "]: " + getListString(consumerAdminNames));*/
			dbgInfo += "\tConsumer admin names [" + String.valueOf(consumerAdminNames.length) 
					+ "]: " + getListString(consumerAdminNames) + "\n";
		//}
		logger.log(AcsLogLevel.DEBUG, dbgInfo);
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
