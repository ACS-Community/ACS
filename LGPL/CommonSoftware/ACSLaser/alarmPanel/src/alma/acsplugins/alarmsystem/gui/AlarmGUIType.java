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
package alma.acsplugins.alarmsystem.gui;

import java.awt.Color;

import javax.swing.ImageIcon;

import com.cosylab.logging.engine.log.LogTypeHelper;

import cern.laser.client.data.Alarm;

/**
 * The type of each alarm as relevant for the GUI.
 * 
 * In this context the type is given by its priority and its state.
 * 
 * The enum contains additional fields for each alarm type. 
 * These fields are related to the representation of the alarm in the GUI.
 * 
 * Each type has a unique ID.
 * 
 * @author acaproni
 *
 */
public enum AlarmGUIType {
	PRIORITY_0(Color.white,Color.red,"Priority 0","flag_red.png"),
	PRIORITY_1(Color.black, new Color(255,165,31),"Priority 1","flag_red.png"),
	PRIORITY_2(Color.black,Color.yellow,"Priority 2","flag_red.png"),
	PRIORITY_3(Color.black,new Color(255,255,198),"Priority 3","flag_red.png"),
	INACTIVE(Color.black,new Color(188,255,188),"Inactive",null);
	
	// The name of the folder with icons
	private static final String iconFolder = "/alma/acsplugins/alarmsystem/gui/resources/";
	
	// Background and foreground colors
	public final Color backg;
	public final Color foreg;
	
	// The string to display
	public final String tile;
	
	// The unique identifier of this type of alarm
	public final int id;
	
	public final ImageIcon icon;
	
	/**
	 * Constructor
	 * 
	 * @param fg Foreground
	 * @param bg Background
	 * @param title The string to show for this alarm type
	 * @param iconName The name of the icon of this alarm type
	 */
	private AlarmGUIType(Color fg, Color bg, String title, String iconName) {
		this.backg=bg;
		this.foreg=fg;
		this.tile=title;
		id=this.ordinal();
		// Load the icon
		if (iconName!=null) {
			icon=new ImageIcon(AlarmGUIType.this.getClass().getResource(iconFolder+iconName));
		} else {
			icon=null;
		}
	}
	
	public AlarmGUIType fromID(int id) {
		if (id<0 || id>=AlarmGUIType.values().length) {
			throw new IllegalArgumentException("Invalid id "+id);
		}
		return AlarmGUIType.values()[id];
	}

	/**
	 * Return the type from an alarm
	 * 
	 * @param alarm The not alarm
	 * @return The type of an alarm
	 * 
	 * @throws IllegalStateException If the priority of the alarm is not in range
	 */
	public static AlarmGUIType fromAlarm(Alarm alarm) {
		if (alarm==null) {
			throw new IllegalArgumentException("The a null alar has no type!");
		}
		if (!alarm.getStatus().isActive()) {
			return INACTIVE;
		}
		switch (alarm.getPriority()) {
		case 0: return PRIORITY_0;
		case 1: return PRIORITY_1;
		case 2: return PRIORITY_2;
		case 3: return PRIORITY_3;
		default: {
			throw new IllegalStateException("Inavlid priority from alarm: "+alarm.getPriority());
		}
		}
	}
	
	/**
	 * Override toString()
	 */
	public String toString() {
		return tile;
	}
}
