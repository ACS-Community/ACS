/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2007
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
package alma.acs.alarm.test.panel;

import java.sql.Timestamp;
import java.util.Properties;

import cern.laser.client.data.Status;

public class TestAlarmStatus implements Status {
	
	/**
	 * <code>true</code> if the alarm is active
	 */
	private boolean active;
	
	/**
	 * <code>true</code> if the alarm is reduced
	 */
	private boolean reduced;
	
	/**
	 * <code>true</code> if the alarm is masked
	 */
	private boolean masked;
	
	public TestAlarmStatus(boolean active, boolean masked, boolean reduced) {
		this.active=active;
		this.reduced=reduced;
		this.masked=masked;
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
	public Timestamp getSystemTimestamp() {
		return null;
	}

	@Override
	public Properties getUserProperties() {
		return null;
	}

	@Override
	public Timestamp getUserTimestamp() {
		return null;
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
	
	/**
	 * @see cern.laser.client.data.Status
	 */
	@Override
	public Object clone() throws CloneNotSupportedException {
		return new TestAlarmStatus(active,masked,reduced);
	}

}
