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
package alma.acsplugins.alarmsystem.gui.sound;

/**
 * The listener of sound events.
 * 
 * @author acaproni
 * @since ACS 8.1.0
 *
 */
public interface AlarmSoundListener {

	/**
	 * The level of sounds has been reset because the
	 * time interval has elapsed.
	 */
	public void reset();
	
	/**
	 * A sound is playing for the passed priority.
	 * 
	 * @param priority The priority of the alarm for which a sound 
	 * 					is playing 
	 */
	public void playing(int priority);
	
	/**
	 * A sound has been played.
	 */
	public void played();
	
}
