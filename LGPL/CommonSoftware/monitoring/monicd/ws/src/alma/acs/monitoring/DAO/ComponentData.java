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

public class ComponentData {
    public String componentName;

    public String propertyName;

    public Integer index;

    public String serialNumber;

    public long startTime;

    public long stopTime;
    
    public int sampleSize;

    public String clob = "";
    
    public ComponentStatistics statistics = null;

    public void reset() {
        clob = "";
        startTime = 0;
        stopTime = 0;
        statistics = null;
        index = null;
    }

    @Override
    public String toString() {
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
// 	HSO: logging the clob in alma.archive.tmcdb.monitor.BlobberWorker.storeData(BlobData) is too much for the current tests, so I temporarily comment it out here.
//      	TODO: check where else it is used, or if we should log it when a special debug property is set.
//		builder.append("] clobBuilder: [");
//		builder.append(clob);
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
