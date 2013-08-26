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

import cern.laser.client.data.Triplet;

/**
 * The triplet
 * 
 * @author acaproni
 *
 */
public class PanelTriplet implements Triplet {
	
	/**
	 * Fault family
	 */
	public final String FF;
	
	/**
	 * Fault member
	 */
	public final String FM;
	
	/**
	 * Fault code
	 */
	public final int FC;
	
	/**
	 * Constructor
	 * 
	 * @param FF Fault Family
	 * @param FM Fault Member
	 * @param FC Fault Code
	 */
	public PanelTriplet(String FF, String FM, int FC) {
		this.FF=FF;
		this.FC=FC;
		this.FM=FM;
	}

	/* (non-Javadoc)
	 * @see cern.laser.client.data.Triplet#getFaultCode()
	 */
	@Override
	public Integer getFaultCode() {
		return FC;
	}

	/* (non-Javadoc)
	 * @see cern.laser.client.data.Triplet#getFaultFamily()
	 */
	@Override
	public String getFaultFamily() {
		return FF;
	}

	/**
	 * @see cern.laser.client.data.Triplet#getFaultMember()
	 */
	@Override
	public String getFaultMember() {
		return FM;
	}
	
	public Object clone() {
		return new PanelTriplet(FF,FM, FC);
	}
}
