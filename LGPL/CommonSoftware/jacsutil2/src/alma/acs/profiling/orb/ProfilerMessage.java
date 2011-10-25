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
package alma.acs.profiling.orb;

public class ProfilerMessage implements Comparable<ProfilerMessage>
{
	ProfilerMessage(Type type) {
		this.type = type;
	}
	enum Type {CONNECTION_POOL, REQUEST_QUEUE, THREAD_POOL, REQUEST_STARTED, REQUEST_FINISHED}
	final Type type;
	long timestamp;
	int requestId;
	String poaName;
	// todo fields for pool/queue sizes
	String operation;
	long timeElapsedMillis; // only for REQUEST_FINISHED
	
	
	/** 
	 * Currently not consistent with equals! 
	 */
	@Override
	public int compareTo(ProfilerMessage other) {
		if (this.timestamp < other.timestamp) return -1;
		if (this.timestamp > other.timestamp) return 1;
		
		// important to have REQUEST_STARTED before REQUEST_FINISHED, even with same timestamp
		int comp = this.type.compareTo(other.type);
		if (comp != 0) return comp;
		
		if (this.operation != null) {
			comp = this.operation.compareTo(other.operation);
			if (comp != 0) return comp;
		}
		
		if (this.requestId < other.requestId) return -1;
		if (this.requestId > other.requestId) return 1;
		
		if (this.poaName != null) {
			comp = this.poaName.compareTo(other.operation);
			if (comp != 0) return comp;
		}
		
		return 0;
	}

}
