/*
ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2013 
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
package alma.alarmsystem.clients.test.utils;

import java.net.URL;
import java.sql.Timestamp;
import java.util.Collection;
import java.util.Properties;

import cern.laser.client.data.Alarm;
import cern.laser.client.data.Location;
import cern.laser.client.data.ResponsiblePerson;
import cern.laser.client.data.Source;
import cern.laser.client.data.Status;
import cern.laser.client.data.Triplet;

/**
 * A helper class to generate alarm useful for testing without the need
 * of the alarm server.
 * 
 * @author acaproni
 * @since ACS-12.2
 */
public class AlarmForTesting implements Alarm {

	/**
	 * Implementation of the {@link Triplet}
	 * 
	 * @author acaproni
	 * @since ACS1-12.2
	 */
	private class TripletForTesting implements Triplet {
		
		private String faultFamily;
		private String faultMember;
		private Integer faultCode;
		
		/**
		 * Constructor 
		 * 
		 * @param faultFamily The FF
		 * @param faultMember The FM
		 * @param faultCode The FC
		 */
		public TripletForTesting(String faultFamily, String faultMember, int faultCode) {
			this.faultCode=Integer.valueOf(faultCode);
			this.faultMember=faultMember;
			this.faultFamily=faultFamily;
		}

		@Override
		public String getFaultFamily() {
			return faultFamily;
		}

		@Override
		public String getFaultMember() {
			return faultMember;
		}

		@Override
		public Integer getFaultCode() {
			return faultCode;
		}

		@Override
		public Object clone() throws CloneNotSupportedException {
			throw new CloneNotSupportedException("Not implemented");
		}
	}
	
	/**
	 * Implementation of the {@link Status}
	 * @author acaproni
	 * @since ACS 12-2
	 */
	private class StatusForTesting implements Status {
		
		private boolean active;
		private boolean masked;
		private boolean reduced;
		
		public StatusForTesting(boolean active, boolean masked, boolean reduce) {
			updateStatus(active, masked, reduced);
		}
		
		/**
		 * To change the status for testing
		 * 
		 * @param active
		 * @param masked
		 * @param reduced
		 */
		public void updateStatus(boolean active, boolean masked, boolean reduced) {
			this.masked=masked;
			this.reduced=reduced;
			this.active=active;
		}

		@Override
		public boolean isActive() {
			return active;
		}

		@Override
		public boolean isMasked() {
			return masked;
		}

		@Override
		public boolean isReduced() {
			return reduced;
		}

		@Override
		public String getSourceHostname() {
			return null;
		}

		@Override
		public Timestamp getSourceTimestamp() {
			return null;
		}

		@Override
		public Timestamp getUserTimestamp() {
			return null;
		}

		@Override
		public Timestamp getSystemTimestamp() {
			return null;
		}

		@Override
		public Properties getUserProperties() {
			return null;
		}

		@Override
		public Object clone() throws CloneNotSupportedException {
			throw new CloneNotSupportedException("Not implemented");
		}
	}
	
		
	/**
	 * The triplet
	 */
	public Triplet triplet;
	
	/**
	 * The priority
	 */
	public Integer priority;
	
	/**
	 * Status
	 */
	public Status status;
	
	public boolean multiplicityChild=false;
	public boolean multiplicityParent=false;
	public boolean nodeyChild=false;
	public boolean nodeParent=false;
	
	/**
	 * Constructor 
	 * 
	 * @param faultFamily The FF
	 * @param faultMember The FM
	 * @param faultCode The FC
	 */
	public AlarmForTesting(
			String faultFamily, 
			String faultMember, 
			int faultCode, 
			int priority,
			boolean active,
			boolean masked,
			boolean reduced) {
		triplet = new TripletForTesting(faultFamily, faultMember, faultCode);
		status = new StatusForTesting(active, masked, reduced);
		this.priority=Integer.valueOf(priority);
	}

	@Override
	public String getAlarmId() {
		return triplet.getFaultFamily()+":"+triplet.getFaultMember()+":"+triplet.getFaultCode();
	}

	@Override
	public Triplet getTriplet() {
		return triplet;
	}

	@Override
	public String getSystemName() {
		return "Unknowne system name";
	}

	@Override
	public String getIdentifier() {
		return "Unknown identifier";
	}

	@Override
	public String getProblemDescription() {
		return "No description";
	}

	@Override
	public Integer getPriority() {
		return priority;
	}

	@Override
	public String getCause() {
		return "No cause";
	}

	@Override
	public String getAction() {
		return "No action";
	}

	@Override
	public String getConsequence() {
		return "No consequences";
	}

	@Override
	public Source getSource() {
		return null;
	}

	@Override
	public URL getHelpURL() {
		return null;
	}

	@Override
	public String getPiquetGSM() {
		return "No GSM";
	}

	@Override
	public String getPiquetEmail() {
		return "No email";
	}

	@Override
	public Collection getCategories() {
		return null;
	}

	@Override
	public Location getLocation() {
		return null;
	}

	@Override
	public ResponsiblePerson getResponsiblePerson() {
		return null;
	}

	@Override
	public Status getStatus() {
		return status;
	}

	@Override
	public boolean isInstant() {
		return false;
	}

	@Override
	public boolean isMultiplicityParent() {
		return multiplicityParent;
	}

	@Override
	public boolean isNodeParent() {
		return nodeParent;
	}

	@Override
	public boolean isMultiplicityChild() {
		return multiplicityChild;
	}

	@Override
	public boolean isNodeChild() {
		return nodeyChild;
	}
	
	/**
	 * To update moltiplicty and node statuses
	 */
	public void updateReductionRuleStatus(
		boolean multiplicityChild,
		boolean multiplicityParent,
		boolean nodeyChild,
		boolean nodeParent) {
		this.multiplicityChild=multiplicityChild;
		this.multiplicityParent=multiplicityParent;
		this.nodeyChild=nodeyChild;
		this.nodeParent=nodeParent;
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		throw new CloneNotSupportedException("Not implemented");
	}	
}
