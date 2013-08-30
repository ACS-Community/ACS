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
package alma.acs.monitoring.DAO;

import java.util.logging.Logger;

import alma.acs.monitoring.MonitorPointTimeSeries;


/**
 * Holds the monitor data of one logical monitor point
 * along with meta data, collected over a time interval.
 * <p>
 * The data can be accessed as time series objects or 
 * in the "clobbed" format which is meant only for Oracle TMCDB
 * but for legacy reasons is supported on the interface level as well
 * (see http://ictjira.alma.cl/browse/ICT-1167)
 * <p> 
 * Note that one baci property may have been expanded to multiple 
 * ComponentData instances. 
 * <p>
 * The name "ComponentData" is misleading because a device component
 * can have many monitor points and thus produce many instances
 * of this class for a given time interval. 
 * However we have not bothered yet to change this legacy issue.
 */
public class ComponentData {

	public final MonitorPointTimeSeries mpTs;

	public final int sampleSize;
	
	public String componentName;

	public String propertyName;

	public Integer index;

	public String serialNumber;

	public long startTime;

	public long stopTime;

	private String clobCache;

	public ComponentStatistics statistics = null;

	protected final Logger logger;


	public ComponentData(MonitorPointTimeSeries mpTs, Logger logger) {
		if (mpTs != null) {
			this.mpTs = mpTs;
			this.sampleSize = mpTs.getDataList().size();
		}
		else {
			// this can happen for dummy ComponentData used in tests or as queue sentinel in class BlobDataQueue
			this.mpTs = null;
			this.sampleSize = -1;
		}
		this.logger = logger;
	}

	/**
	 * Gets the monitor point data.
	 */
	public MonitorPointTimeSeries getMonitorPointTimeSeries() {
		return mpTs;
	}

	/**
	 * Gets the monitor point data in "clobbed" format.
	 */
	public synchronized String getClob() {
		if (clobCache == null) {
			Clobber clobber = new Clobber(logger);
			String clob = clobber.generateClob(mpTs);
			if (clob != null) {
				clobCache = clob;
			}
			else {
				// this can happen for NaN floats/doubles
				clobCache = "";
			}
		}
		return clobCache;
	}

	/**
	 * Returns a formatted name-value string, including the clobbed data.
	 */
	public String toStringWithClob() {
		return toString(true);
	}

	/**
	 * Returns a formatted name-value string, excluding the clobbed data.
	 */
	@Override
	public String toString() {
		return toString(false);
	}

	protected String toString(boolean includeClobData) {
		StringBuilder builder = new StringBuilder();
		builder.append("componentName [");
		builder.append(componentName);
		builder.append("] propertyName: [");
		builder.append(propertyName);
		builder.append("] serialNumber: [");
		builder.append(serialNumber);
		builder.append("] startTime: [");
		builder.append(startTime);
		builder.append("] stopTime: [");
		builder.append(stopTime);
		if (includeClobData) {
			builder.append("] clob: [");
			builder.append(getClob());
		}
		builder.append("] index: [");
		builder.append(index);
		builder.append("]");
		if (statistics != null) {
			builder.append(" ");
			builder.append(statistics);
		}
		return builder.toString();
	}

	/**
	 * hashCode based on {@link ComponentData#componentName}, {@link #propertyName},
	 * {@link #index}, and {@link #serialNumber}.
	 */
	public int hashCode() {
		return componentName.toString().hashCode()
				+ serialNumber.toString().hashCode()
				+ propertyName.toString().hashCode() + index.hashCode();
	}

	/**
	 * equals based on {@link ComponentData#componentName}, {@link #propertyName},
	 * {@link #index}, and {@link #serialNumber}.
	 */
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;

		ComponentData componentData = (ComponentData) obj;

		if (componentData.componentName.equals(this.componentName)
				&& componentData.propertyName.equals(this.propertyName)
				&& componentData.serialNumber.equals(this.serialNumber)
				&& componentData.index.equals(this.index)) {
			return true;
		}

		return false;
	}
	
	public String propertyPathname() {
		return componentName + ":" + propertyName + ":" + index;
	}
}
