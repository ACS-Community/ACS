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
package alma.alarmsystem.clients.source;

import cern.laser.source.alarmsysteminterface.FaultState;

/**
 * The listener of alarms published from sources
 * 
 * @author acaproni
 *
 */
public interface SourceListener {
	
	/**
	 * An alarm has been received from the source NC.
	 * 
	 * @param alarm The alarm received from a source
	 */
	public void faultStateReceived(FaultState faultState);
	
	/**
	 * An XML has been received from the source NC.
	 * <P>
	 * The XML follows the ASIMessage.xsd schema definition and therefore
	 * it might have one or more fault states.
	 * 
	 * @param asiMessage The ASIMessage received from an alarm source
	 */
	public void sourceXMLMsgReceived(String asiMessage);
}
