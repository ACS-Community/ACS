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
package alma.acsplugins.alarmsystem.gui.table;

import java.awt.Color;

import javax.swing.ImageIcon;
import javax.swing.JLabel;

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
	PRIORITY_1(Color.black, new Color(255,165,31),"Priority 1","flag_orange.png"),
	PRIORITY_2(Color.black,Color.yellow,"Priority 2","flag_light_yellow.png"),
	PRIORITY_3(Color.black,new Color(255,255,198),"Priority 3","flag_yellow.png"),
	INACTIVE(Color.black,new Color(188,255,188),"Inactive","flag_green.png");
	
	/**
	 *  The name of the folder with icons
	 */
	public static final String iconFolder = "/alma/acsplugins/alarmsystem/gui/resources/";
	
	/** 
	 * Background color
	 */
	public final Color backg;
	
	/**
	 * Foreground color
	 */
	public final Color foreg;
	
	/**
	 *  The string to display
	 */
	public final String tile;
	
	/**
	 *  The unique identifier of this type of alarm
	 */
	public final int id;
	
	/**
	 *  The renderer for the table showing the flag icon
	 */
	public final JLabel flagRenderer;
	
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
		ImageIcon flagIcon;
		if (iconName!=null) {
			flagIcon=new ImageIcon(AlarmGUIType.this.getClass().getResource(iconFolder+iconName));
		} else {
			flagIcon=null;
		}
		flagRenderer = new JLabel(flagIcon,JLabel.CENTER);
	}
	
	public static AlarmGUIType fromID(int id) {
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
	public static AlarmGUIType fromAlarm(AlarmTableEntry alarm) {
		if (alarm==null) {
			throw new IllegalArgumentException("A null alarm can't have type!");
		}
		if (!alarm.getStatus().isActive()) {
			return INACTIVE;
		}
		switch (alarm.getPriority().intValue()) {
		case 0: return PRIORITY_0;
		case 1: return PRIORITY_1;
		case 2: return PRIORITY_2;
		case 3: return PRIORITY_3;
		default: {
			throw new IllegalStateException("Inavlid priority from alarm: "+alarm.getPriority().intValue());
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
