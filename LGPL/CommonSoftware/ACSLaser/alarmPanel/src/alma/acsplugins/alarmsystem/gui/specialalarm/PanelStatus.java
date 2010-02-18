/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2010
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */
package alma.acsplugins.alarmsystem.gui.specialalarm;

import java.sql.Timestamp;
import java.util.Properties;

import cern.laser.client.data.Status;

/**
 * The status
 * 
 * @author acaproni
 */
public class PanelStatus implements Status {
	/**
	 * <code>true</code> if the alarm is active
	 */
	private boolean active;
	
	/**
	 * The user properties
	 */
	private Properties properties;
	
	/**
	 * The timestamp
	 */
	private final Timestamp timestamp=new Timestamp(System.currentTimeMillis());
	
	/**
	 * Build the status
	 * 
	 * @param active <code>true</code> if the alarm is active
	 * @param props The user properties
	 */
	public PanelStatus(boolean active, Properties props) {
		setStatus(active);
		setUserProps(props);
	}

	/**
	 * @see cern.laser.client.data.Status#getSourceHostname()
	 */
	@Override
	public String getSourceHostname() {
		return "";
	}

	/**
	 * @see cern.laser.client.data.Status#getSourceTimestamp()
	 */
	@Override
	public Timestamp getSourceTimestamp() {
		return timestamp;
	}

	/**
	 * @see cern.laser.client.data.Status#getSystemTimestamp()
	 */
	@Override
	public Timestamp getSystemTimestamp() {
		return timestamp;
	}

	/**
	 * @see cern.laser.client.data.Status#getUserProperties()
	 */
	@Override
	public Properties getUserProperties() {
		return properties;
	}

	/**
	 * @see cern.laser.client.data.Status#getUserTimestamp()
	 */
	@Override
	public Timestamp getUserTimestamp() {
		return timestamp;
	}

	/**
	 * @see cern.laser.client.data.Status#isActive()
	 */
	@Override
	public boolean isActive() {
		return active;
	}

	/**
	 * @see cern.laser.client.data.Status#isMasked()
	 */
	@Override
	public boolean isMasked() {
		return false;
	}

	/**
	 * @see cern.laser.client.data.Status#isReduced()
	 */
	@Override
	public boolean isReduced() {
		return false;
	}
	
	public Object clone() throws CloneNotSupportedException {
		throw new CloneNotSupportedException();
	}
	
	/**
	 * Set the status active/inactive and update
	 * the time stamp
	 * 
	 * @param active
	 */
	public void setStatus(boolean active) {
		timestamp.setTime(System.currentTimeMillis());
		this.active=active;
	}
	
	/**
	 * Set the user properties of the alarm
	 * 
	 * @param props The new properties
	 */
	public void setUserProps(Properties props) {
		this.properties=props;
	}
}
