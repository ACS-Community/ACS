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
import java.util.Map;
import java.util.List;
//import java.util.logging.Logger;

import alma.acs.exceptions.AcsJException;
//import alma.acs.logging.AcsLogLevel;
import alma.acs.nsstatistics.ChannelData;


class ChannelStats {
	static final String N_CONSUMERS = "n. consumers";
	static final String N_SUPPLIERS = "n. suppliers";
	static final String N_SLOWEST_CONSUMERS = "n. slowest consumers";
	static final String N_SUPPLIERS_ADMIN = "n. suppliers admin";
	static final String N_CONSUMERS_ADMIN = "n. consumers admin";

	static final String CURR_SLOWEST_CONSUMERS = "Current slowest consumers";
	static final String CURR_CONSUMERS = "Current consumers";
	static final String CURR_SUPPLIERS = "Current suppliers";
	static final String CURR_CONSUMERS_ADMIN = "Current consumers admin";
	static final String CURR_SUPPLIERS_ADMIN = "Current consumers admin";

	static final String ALL_SLOWEST_CONSUMERS = "All slowest consumers";
	static final String ALL_CONSUMERS = "All consumers";
	static final String ALL_SUPPLIERS = "All suppliers";
	static final String ALL_CONSUMERS_ADMIN = "All consumers admin";
	static final String ALL_SUPPLIERS_ADMIN = "All consumers admin";

	class AggregateData {
		public long max;
		public long min;
		public float avg;
		public long sum;

		public AggregateData() {
			max = 0;
			min = 0;
			avg = 0;
			sum = 0;
		}

		public AggregateData(long value) {
			max = value;
			min = value;
			avg = (float)value;
			sum = value;
		}

		public void set(long value, long counter) {
			if(max < value || counter == 0) {
				max = value;
			}
			if(min > value || counter == 0) {
				min = value;
			}
			sum += value;
			avg = sum / (counter + 1);
		}

		public String toString() {
			return "MIN=" + String.valueOf(min) + ", MAX=" + String.valueOf(max) + ", AVG=" + String.valueOf(avg);
		}
	}

	private HashMap<String, AggregateData> aggregateData;
	private HashMap<String, ArrayList<String>> currStringLists; // Current slowest consumers, consumers, suppliers, consumers admin, suppliers admin
	private HashMap<String, ArrayList<String>> allStringLists; // All slowest consumers, consumers, suppliers, consumers admin, suppliers admin

	private HashMap<String, AggregateData> queuesElementCount; // key: Queue name
	private HashMap<String, AggregateData> queuesSize; // key: Queue name

	private HashMap<String, Double> queuesSizeAvg; // key: Queue name
	private HashMap<String, Long> queuesSizeMax; // key: Queue name

	private long oldestEvent;

	public long counter;

	ChannelStats() {
		aggregateData = new HashMap<String, AggregateData>();
		currStringLists = new HashMap<String, ArrayList<String>>();
		allStringLists = new HashMap<String, ArrayList<String>>();

		queuesElementCount = new HashMap<String, AggregateData>();
		queuesSize  = new HashMap<String, AggregateData>();

		queuesSizeAvg = new HashMap<String, Double>();
		queuesSizeMax = new HashMap<String, Long>();

		oldestEvent = -1;

		counter = 0;
	}

	public void reset() {
		counter = 0;

		aggregateData.clear();
		currStringLists.clear();
		allStringLists.clear();
		queuesElementCount.clear();
		queuesSize.clear();
		queuesSizeAvg.clear();
		queuesSizeMax.clear();

		oldestEvent = -1;
	}

	public void setData(ChannelData channel) {
		NotificationServiceMonitorControl nsm = channel.getParent().getMc();
		String [] statsNames = nsm.get_statistic_names();
		
		String channelName = "Channel " + channel.getName() + " - ";
		long oldestEvent = -1;
		
		for(int i = 0;i < statsNames.length;++i) {
			try {
				if(statsNames[i].contains("ConsumerCount")) {
					Monitor.Numeric n = nsm.get_statistic(statsNames[i]).data_union.num();
					setValue(N_CONSUMERS, (long)n.last);
				} else if(statsNames[i].contains("SupplierCount")) {
					Monitor.Numeric n = nsm.get_statistic(statsNames[i]).data_union.num();
					setValue(N_SUPPLIERS, (long)n.last);
				} else if(statsNames[i].contains("QueueSize")) {
					// n.count, n.maximum, n.last, n.average, n.dlist[0].value, n.dlist[0].timestamp
					Monitor.Numeric n = nsm.get_statistic(statsNames[i]).data_union.num();
					setQueueSize(statsNames[i], (long)n.maximum, n.average);
				} else if(statsNames[i].contains("QueueElementCount")) {
					Monitor.Numeric n = nsm.get_statistic(statsNames[i]).data_union.num();
					setQueueElementCount(statsNames[i], (long)n.maximum);
				} else if(statsNames[i].contains("OldestEvent")) {
					Monitor.Numeric n = nsm.get_statistic(statsNames[i]).data_union.num();
					this.oldestEvent = (long)n.last;
				} else if(statsNames[i].contains("SlowestConsumers")) {
					String [] slowestConsumers = nsm.get_statistic(statsNames[i]).data_union.list();
					setArrayValues(CURR_SLOWEST_CONSUMERS, currStringLists, slowestConsumers);
					addArrayValues(ALL_SLOWEST_CONSUMERS, allStringLists, slowestConsumers);
					setValue(N_SLOWEST_CONSUMERS, slowestConsumers.length);
				} else if(statsNames[i].contains("SupplierNames")) {
					String [] supplierNames = nsm.get_statistic(statsNames[i]).data_union.list();
					setArrayValues(CURR_SUPPLIERS, currStringLists, supplierNames);
					addArrayValues(ALL_SUPPLIERS, allStringLists, supplierNames);
				} else if(statsNames[i].contains("ConsumerNames")) {
					String [] consumerNames = nsm.get_statistic(statsNames[i]).data_union.list();
					setArrayValues(CURR_CONSUMERS, currStringLists, consumerNames);
					addArrayValues(ALL_CONSUMERS, allStringLists, consumerNames);
				} else if(statsNames[i].contains("SupplierAdminNames")) {
					String [] supplierAdminNames = nsm.get_statistic(statsNames[i]).data_union.list();
					setArrayValues(CURR_SUPPLIERS_ADMIN, currStringLists, supplierAdminNames);
					addArrayValues(ALL_SUPPLIERS_ADMIN, allStringLists, supplierAdminNames);
					setValue(N_SUPPLIERS_ADMIN, supplierAdminNames.length);
				} else if(statsNames[i].contains("ConsumerAdminNames")) {
					String [] consumerAdminNames = nsm.get_statistic(statsNames[i]).data_union.list();
					setArrayValues(CURR_CONSUMERS_ADMIN, currStringLists, consumerAdminNames);
					addArrayValues(ALL_CONSUMERS_ADMIN, allStringLists, consumerAdminNames);
					setValue(N_CONSUMERS_ADMIN, consumerAdminNames.length);
				}
							
			} catch(InvalidName ex) {
				//logger.log(AcsLogLevel.ERROR, "Invalid name in ncStatisticsService::logMCStats", ex);
				// TODO 
			}
		}

		iterationDone();

	}

	protected void setArrayValues(String key,HashMap<String,ArrayList<String>> data,String [] values) {
		ArrayList<String> array = new ArrayList<String>();
		for(int i = 0;i < values.length;++i) {
			array.add(values[i]);
		}

		data.put(key, array);
	}

	protected void setQueueElementCount(String name,long value) {
		int i = name.indexOf("QueueElementCount");
		String queueName = name.substring(0, i);
		AggregateData data = queuesElementCount.get(queueName);
		if(null == data) {
			data = new AggregateData(value);
		} else {
			data.set(value, counter);
		}
		queuesElementCount.put(queueName, data);
	}

	protected void setQueueSize(String name,long value,double average) {
		int i = name.indexOf("QueueSize");
		String queueName = name.substring(0, i);
		AggregateData data = queuesSize.get(queueName);
		if(null == data) {
			data = new AggregateData(value);
		} else {
			data.set(value, counter);
		}
		queuesSize.put(queueName, data);
		
		Double lastAvg = queuesSizeAvg.get(queueName);
		if(lastAvg == null) {
			queuesSizeAvg.put(queueName, new Double(average));
		} else {
			queuesSizeAvg.put(queueName, lastAvg + average);
		}

		Long lastMax = queuesSizeMax.get(queueName);
		if(lastMax == null || lastMax < value) {
			queuesSizeMax.put(queueName, new Long(value));
		}
	}

	protected void addArrayValues(String key,HashMap<String,ArrayList<String>> data,String [] values) {
		ArrayList<String> array = null;
		if(false == data.containsKey(key)) {
			array = new ArrayList<String>();
		} else {
			array = data.get(key);
		}
		for(int i = 0;i < values.length;++i) {
			if(false == array.contains(values[i])) {
				array.add(values[i]);
			}
		}
		data.put(key, array);
	}

	public void setValue(String key,long value) {
		AggregateData data = null;
		if(0 == counter) {
			data = new AggregateData(value);
		} else {
			data = aggregateData.get(key);
			if(null == data) {
				data = new AggregateData(value);
			} else {
				data.set(value, counter);
			}
		}
		aggregateData.put(key, data);
	}

	public Long getMaxValue(String key) {
		if(0 == counter) {
			return null;
		}
		AggregateData data = aggregateData.get(key);
		if(null == data) {
			return null;
		}
		return data.max;
	}

	public Long getMinValue(String key) {
		if(0 == counter) {
			return null;
		}
		AggregateData data = aggregateData.get(key);
		if(null == data) {
			return null;
		}
		return data.min;
	}

	public Float getAvgValue(String key) {
		if(0 == counter) {
			return null;
		}
		AggregateData data = aggregateData.get(key);
		if(null == data) {
			return null;
		}
		return data.avg;
	}

	public long iterationDone() {
		counter++;
		return counter;
	}

	public Map<String,String> getDbgParams() {
		Map<String,String> dbgParams = new HashMap<String,String>();
		String str = "";
		AggregateData data = null;
		ArrayList<String> strList = null;

		data = aggregateData.get(N_CONSUMERS);
		dbgParams.put("Num consumers",data.toString()); 
		data = aggregateData.get(N_SUPPLIERS);
		dbgParams.put("Num suppliers",data.toString()); 
		data = aggregateData.get(N_SLOWEST_CONSUMERS); 
		dbgParams.put("Num slowest consumers",data.toString()); 
		data = aggregateData.get(N_CONSUMERS_ADMIN);
		dbgParams.put("Num consumers admin",data.toString()); 
		data = aggregateData.get(N_SUPPLIERS_ADMIN);
		dbgParams.put("Num suppliers admin",data.toString()); 

		strList = currStringLists.get(CURR_CONSUMERS);
		dbgParams.put("Current consumers", array2str(strList));

		strList = currStringLists.get(CURR_SUPPLIERS);
		dbgParams.put("Current suppliers", array2str(strList));

		strList = currStringLists.get(CURR_SLOWEST_CONSUMERS);
		dbgParams.put("Current slowest consumers", array2str(strList));

		strList = currStringLists.get(CURR_CONSUMERS_ADMIN);
		dbgParams.put("Current consumers admin", array2str(strList));

		strList = currStringLists.get(CURR_SUPPLIERS_ADMIN);
		dbgParams.put("Current suppliers admin", array2str(strList));

		strList = allStringLists.get(ALL_CONSUMERS);
		dbgParams.put("All consumers", array2str(strList));

		strList = allStringLists.get(ALL_SUPPLIERS);
		dbgParams.put("All suppliers", array2str(strList));

		strList = allStringLists.get(ALL_SLOWEST_CONSUMERS);
		dbgParams.put("All slowest consumers", array2str(strList));

		strList = allStringLists.get(ALL_CONSUMERS_ADMIN);
		dbgParams.put("All consumers admin", array2str(strList));

		strList = allStringLists.get(ALL_SUPPLIERS_ADMIN);
		dbgParams.put("All suppliers admin", array2str(strList));

		return dbgParams;
	}

	public Map<String,String> getInfoParams(ServiceParameters params) {
		Map<String,String> infoParams = new HashMap<String,String>();
		String str = "", strMax = "", strAvg = "";
		AggregateData data = null;
		ArrayList<String> strList = null;
		long lMax = 0;
		double dMax = 0;

		data = aggregateData.get(N_CONSUMERS);
		infoParams.put("Num consumers",data.toString()); 
		data = aggregateData.get(N_SUPPLIERS);
		infoParams.put("Num suppliers",data.toString()); 


		strMax = "";
		strAvg = "";
		lMax = 0;
		for(Map.Entry<String, AggregateData> entry : queuesElementCount.entrySet()) {
			strMax += String.valueOf(entry.getValue().max) + ", ";	
			strAvg += String.valueOf(entry.getValue().avg) + ", ";
			if(entry.getValue().max > lMax) {
				lMax = entry.getValue().max;
			}
		}
		infoParams.put("Max events in queues", strMax);
		infoParams.put("Avg events in queues", strAvg);

		if(lMax >= params.getThOldestEvent()) {
			infoParams.put("Oldest event", String.valueOf(oldestEvent));
		}

		str = "";
		lMax = 0;
		for(Map.Entry<String, Long> entry : queuesSizeMax.entrySet()) {
			str +=  entry.getValue().toString() + ", ";
			if(entry.getValue() > lMax) {
				lMax = entry.getValue().longValue();
			}
		}
		if(lMax >= params.getThQueueSize()) {
			infoParams.put("Max size of queues [bytes]", str);
		}

		str = "";
		for(Map.Entry<String, Double> entry : queuesSizeAvg.entrySet()) {
			double avg = entry.getValue().doubleValue();
			if(counter == 0) {
				avg = 0;
			} else {
				avg = avg / counter;
			}
			str +=  String.valueOf(avg) + ", ";
		}
		if(lMax >= params.getThQueueSize()) {
			infoParams.put("Avg size of queues [bytes]", str);
		}

		strList = currStringLists.get(CURR_SLOWEST_CONSUMERS);
		if(strList.size() > 0) {
			infoParams.put("Current slowest consumers", array2str(strList));
		}

		strList = allStringLists.get(ALL_SLOWEST_CONSUMERS);
		if(strList.size() > 0) {
			infoParams.put("All slowest consumers", array2str(strList));
		}

		return infoParams;
	}


	protected String array2str(List<String> list) {
		if(list.size() <= 0) {
			return "";
		}

		String str = "";
		for(int i = 0;i < list.size() - 1;++i) {
			str += list.get(i) + ",";
		}
		return str + list.get(list.size() - 1);
	}
}


