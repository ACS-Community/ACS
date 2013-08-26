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
package alma.acs.alarmsanalyzer.engine;

import cern.laser.client.services.selection.AlarmSelectionListener;
import cern.laser.client.data.Alarm;

/**
 * The listener of alarms.
 * <P>
 * This is basically a duplicate of {@link AlarmSelectionListener} redefined
 * here only to avoid implementing the unneeded onException()
 * 
 * @author acaproni
 *
 */
public interface AlarmCategoryListener {
	/**
	 * Called on alarm change arrival.
	 * 
	 * @param alarm The alarm
	 */
	public void onAlarm(Alarm alarm);

}
