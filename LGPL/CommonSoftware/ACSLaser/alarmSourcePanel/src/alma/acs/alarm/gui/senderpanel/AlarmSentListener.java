/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2012
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
package alma.acs.alarm.gui.senderpanel;

import alma.acs.alarm.gui.senderpanel.SenderPanelUtils.AlarmDescriptorType;
import alma.acs.alarm.gui.senderpanel.SenderPanelUtils.Triplet;

/**
 * Notify the listener that an alarm has been sent to the alarm service
 * 
 * @author acaproni
 *
 */
public interface AlarmSentListener {
	
	/**
	 * Notifies the listener that an alarm has been sent to the
	 * alarm service.
	 * 
	 * @param triplet The triplet of the alarm
	 * @param descriptor The descriptor
	 * @param success is <code>true</code> if the alarm has been successfully published;
	 * 				  <code>false</code> otherwise
	 */
	public void alarmSent(Triplet triplet, AlarmDescriptorType descriptor, boolean success);

}
