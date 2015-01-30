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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

public class ServiceParameters {

	public static final int DEFAULT_FREQUENCY = 600000;
	public static final int DEFAULT_TH_OLDEST_EVENT = 1000;
	public static final int DEFAULT_TH_QUEUE_SIZE = 1000;
	
	public static final int MIN_ALLOWED_FREQUENCY = 50; // Minimum allowed frequency in ms
	
	public static final int MIN_2_MS = 60000; 
	
	public static final String ARG_FREQUENCY = "-f";
	public static final String ARG_TH_OLDEST_EVENT = "-toe";
	public static final String ARG_TH_QUEUE_SIZE = "-tqs";
	public static final String ARG_MS = "-ms";
	public static final String ARG_ADD = "-add";
	public static final String ARG_HELP = "--help";
	public static final String ARG_HELP2 = "-help";
	public static final String ARG_HELP3 = "help";
	
	protected int frequency; // Frequency in milliseconds
	protected int thOldestEvent; // Log OldestEvent timestamp when QueueElementCount is greater than this threshold
	protected int thQueueSize; // Log QueueSize when its value is greater than this threshold
	protected HashMap<String, String[]> selectedServicesChannels;
	
	public ServiceParameters() {
		frequency = DEFAULT_FREQUENCY;
		thOldestEvent = DEFAULT_TH_OLDEST_EVENT;
		thQueueSize = DEFAULT_TH_QUEUE_SIZE;
		selectedServicesChannels = new HashMap<String,String[]>();
	}
	
	public String help(String toolName) {
		String str = "> " + toolName + " options\n\n";
		str += "\tOptions can be:\n";
		str += "\t\t" + ServiceParameters.ARG_ADD + "\tService and channels to look at. Format is: serviceName:channelName1,channelName2\n";
		str += "\t\t" + ServiceParameters.ARG_FREQUENCY + "\tFrequency in milliseconds at witch statistics will be get. Default value is " 
				+ String.valueOf(ServiceParameters.DEFAULT_FREQUENCY) + "\n";
		str += "\t\t" + ServiceParameters.ARG_TH_OLDEST_EVENT + "\tThershold used to log the oldest event timestamp. When QueueElementCount is greater than this threshold the oldest event timestamp will be logged. Default value is " 
				+ String.valueOf(ServiceParameters.DEFAULT_TH_OLDEST_EVENT) + "\n";
		str += "\t\t" + ServiceParameters.ARG_TH_QUEUE_SIZE + "\tThreshold used to log the queue size. Default value is " + ServiceParameters.DEFAULT_TH_QUEUE_SIZE + "\n";
		str += "\t\t" + ServiceParameters.ARG_MS + "\tWhen this parameter is set, frequency value is considered as milliseconds\n\n";
		str += "\tWhen there are no services defined, all services will be logged.\n";
		str += "\tWhen there are no channels defined in a service, all channels will be logged.\n";
		return str;
	}
	
	
	boolean read(String toolName,String[] args,List<String> errors) {
		boolean ret = true;
		boolean inMillis = false;
		for(int i = 0;i < args.length;++i) {
			if(args[i].equals(ARG_FREQUENCY)) {
				if(i + 1 < args.length) {
					try {
						this.frequency = Integer.parseInt(args[++i]);
					} catch(NumberFormatException ex) {
						errors.add("Frequency must be an integer (milliseconds)");
					}
				} else {
					errors.add("Option \""+ ARG_FREQUENCY + "\" without value");
				}
			} else if(args[i].equals(ARG_ADD)) {
				if(i + 1 < args.length) {
					String str = args[++i];
					int pos = str.indexOf(":");
					if(pos < 0) { // No channels 
						selectedServicesChannels.put(str, new String[0]);
					} else {
						String service = str.substring(0, pos).trim();
						String [] channels = str.substring(pos + 1).split(",");
						for(int j = 0;j < channels.length;++j) {
							channels[j] = channels[j].trim();
						}
						selectedServicesChannels.put(service, channels);
					}
				} else {
					errors.add("Option \"" + ARG_ADD + "\" without value");
				}
			} else if(args[i].equals(ARG_TH_OLDEST_EVENT)) {
				if(i + 1 < args.length) {
					try {
						thOldestEvent = Integer.parseInt(args[++i]);
						if(thOldestEvent < 0) {
							errors.add("Option \"" + ARG_TH_OLDEST_EVENT + "\" value must be equal or greater than 0");
						}
					} catch(NumberFormatException ex) {
						errors.add("Option \"" + ARG_TH_OLDEST_EVENT + "\" value must be an integer");
					}
				} else {
					errors.add("Option \"" + ARG_TH_OLDEST_EVENT + "\" without value");
				}
			} else if(args[i].equals(ARG_TH_QUEUE_SIZE)) {
				if(i + 1 < args.length) {
					try {
						thQueueSize = Integer.parseInt(args[++i]);
						if(thQueueSize < 0) {
							errors.add("Option \"" + ARG_TH_QUEUE_SIZE + "\" value must be equal or greater than 0");
						}
					} catch(NumberFormatException ex) {
						errors.add("Option \"" + ARG_TH_QUEUE_SIZE + "\" value must be an integer");
					}
				} else {
					errors.add("Option \"" + ARG_TH_QUEUE_SIZE + "\" without value");
				}
			} else if(args[i].equals(ARG_HELP) || args[i].equals(ARG_HELP2) || args[i].equals(ARG_HELP3)) {
				System.out.println(help(toolName));
				ret = false;
			} else if(args[i].equals(ARG_MS)) {
				inMillis = true;
			} else {
				errors.add("Unknown option: " + args[i]);
			}
		}
		
		if(this.frequency > 0) {
			if(!inMillis) {
				this.frequency = this.frequency * MIN_2_MS;
			}
		}
		
		if(frequency < MIN_ALLOWED_FREQUENCY)
		{
			errors.add("Frequency must be greater or equal than " + String.valueOf(MIN_ALLOWED_FREQUENCY));
		}
		
		return errors.isEmpty() && ret;
	}
	
	int getFrequency() {
		return frequency;
	}
	
	int getThOldestEvent() {
		return thOldestEvent;
	}
	
	int getThQueueSize() {
		return thQueueSize;
	}
	
	void setFrequency(int frequency) {
		this.frequency = frequency;
	}
	
	void setThOldestEvent(int th) {
		this.thOldestEvent = th;
	}
	
	void setThQueueSize(int th) {
		this.thQueueSize = th;
	}
	
	HashMap<String, String[]> getSelectedServicesChannels() {
		return this.selectedServicesChannels;
	}
	
	void log(Logger logger) {
		logger.info("Frequency: " + String.valueOf(frequency) + "ms");
		logger.info("OldestEvent threshold: " + String.valueOf(thOldestEvent));
		logger.info("QueueSize threshold: " + String.valueOf(thQueueSize));
		if(this.selectedServicesChannels.size() == 0) {
			logger.info("Log all channels");
		} else {
			for(Iterator<Map.Entry<String, String[]>> it = selectedServicesChannels.entrySet().iterator();
					it.hasNext();) {
				Map.Entry<String, String[]> data = it.next();
				String[] chList = data.getValue();
				String channels = "";
				for(int i = 0;i < chList.length - 1;++i) {
					channels += ", " + chList[i];
				}
				if(chList.length > 0) {
					channels += chList[chList.length - 1];
				}
				logger.info("Service " + data.getKey() + " with channels " + channels);
			}
		}
	}
}
