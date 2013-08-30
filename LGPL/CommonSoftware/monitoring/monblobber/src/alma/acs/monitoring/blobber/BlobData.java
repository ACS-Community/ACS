/*
 * ALMA - Atacama Large Millimiter Array
 * Copyright (c) European Southern Observatory, 2011 
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
package alma.acs.monitoring.blobber;

import java.util.logging.Logger;

import org.apache.commons.math.stat.descriptive.SummaryStatistics;

import alma.acs.monitoring.MonitorPointTimeSeries;
import alma.acs.monitoring.MonitorPointValue;
import alma.acs.monitoring.DAO.ComponentData;
import alma.acs.monitoring.DAO.ComponentStatistics;

/**
 * Extends the ComponentData type with a list of monitor data (see {@link #getMonitorPointTimeSeries()})
 * and computes the {@link #statistics} field if the data type 
 * is numeric and single-valued.
 * 
 * @author hsommer
 */
public class BlobData extends ComponentData
{
	public BlobData(MonitorPointTimeSeries mpTs, Logger logger) {
		super(mpTs, logger);
	}
	
	/**
	 * Gets the number of MonitorPointValue objects
	 * contained in the MonitorPointTimeSeries.
	 */
	int getDataSize() {
		return mpTs.getDataList().size();
	}
	
	
	/**
	 * Calculates the statistics and stores it in {@link #statistics}
	 * if our monitor point data is represented as Number objects;
	 * otherwise this call is ignored.
	 * 
	 * @param inDataList
	 */
	void calculateStatistics() {
		if (getDataSize() > 0) {
			
			// We trust that the data is homogeneous and check only the first MonitorPointValue
			
			MonitorPointValue sampleMonitorPointValue = mpTs.getDataList().get(0);
			
			if (sampleMonitorPointValue.getData().isEmpty()) {
				logger.finer("Ignoring calculateStatistics() call for a time series of MonitorPointValue objects that hold no data."); 
				return;
			}
			
			// TODO: Should we also compute statistics for multi-valued properties?
			// This was not done in the original (= pre-ACS 12.0) implementation of BlobberWorker#calculateStatistics
			// and so far we keep this behavior. 
			if (sampleMonitorPointValue.isMultiValued()) {
				logger.finer("Ignoring calculateStatistics() call for a time series of multi-valued MonitorPointValue objects."); 
				return;
			}

			// After the above checks, there should be a single data item in our sampleMonitorPointValue
			// We now verify that it has one of the expected numeric types.
			Object sampleData = sampleMonitorPointValue.getData().get(0);
			if (!(sampleData instanceof Integer || 
				  sampleData instanceof Long || 
				  sampleData instanceof Float || 
				  sampleData instanceof Double)) 
			{
				logger.finer("Ignoring calculateStatistics() call for data type " + sampleData.getClass().getName()); 
				return;
			}
			
			// Now we calculate the statistics, 
			// using apache math lib that works only with 'double' type
			
			SummaryStatistics stat = new SummaryStatistics();
			for (MonitorPointValue blobData : mpTs.getDataList()) {
				Number value = (Number) blobData.getData().get(0);
				stat.addValue(value.doubleValue());
			}

			statistics = new ComponentStatistics();
			
			// We store the results in a ComponentStatistics object, 
			// converting to original data types where it makes sense
			if (sampleData instanceof Integer) {
				statistics.min = new Integer((int) Math.round(stat.getMin()));
				statistics.max = new Integer((int) Math.round(stat.getMax()));
				statistics.mean = new Double(stat.getMean()); // or Float, to indicate lower precision?
				statistics.stdDev = new Double(stat.getStandardDeviation()); // or Float, to indicate lower precision?
			}
			else if (sampleData instanceof Long) {
				statistics.min = new Long(Math.round(stat.getMin()));
				statistics.max = new Long(Math.round(stat.getMax()));
				statistics.mean = new Double(stat.getMean()); 
				statistics.stdDev = new Double(stat.getStandardDeviation()); 
			}
			else if (sampleData instanceof Float) {
				statistics.min = new Float(stat.getMin());
				statistics.max = new Float(stat.getMax());
				statistics.mean = new Float(stat.getMean());
				statistics.stdDev = new Float(stat.getStandardDeviation());
			}
			else if (sampleData instanceof Double) {
				statistics.min = new Double(stat.getMin());
				statistics.max = new Double(stat.getMax());
				statistics.mean = new Double(stat.getMean());
				statistics.stdDev = new Double(stat.getStandardDeviation());
			}
		}
	}

}