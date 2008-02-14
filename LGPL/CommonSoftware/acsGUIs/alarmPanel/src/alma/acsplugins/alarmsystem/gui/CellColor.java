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

/**
 * The colors of a cell depending on the priority of the alarm.
 * Inactive alarms are also here
 * 
 * @author acaproni
 *
 */
public enum CellColor {
	PRI_0(Color.white,Color.red),
	PRI_1(Color.black, new Color(255,165,31)),
	PRI_2(Color.black,Color.yellow),
	PRI_3(Color.black,new Color(255,255,198)),
	INACTIVE(Color.black,new Color(188,255,188));
	
	// Background and foreground colors
	public final Color backg;
	public final Color foreg;
	
	/**
	 * Constructor
	 * 
	 * @param fg Foreground
	 * @param bg Background
	 */
	private CellColor(Color fg, Color bg) {
		this.backg=bg;
		this.foreg=fg;
	}
	
	/**
	 * Return the CellColor for the given priority
	 * 
	 * @param priority The priority of the CellColor to find
	 * @return The CellColor for the given priority
	 */
	public static CellColor fromPriority(int priority) {
		if (priority<0 || priority>3) {
			throw new IllegalArgumentException("Invalid priority: "+priority+" not in [0,3]");
		}
		return CellColor.values()[priority];
	}
	
	/**
	 * Get the foreground color for the given priority
	 * 
	 * @param priority The priority of the foreground color to find
	 * @return The foreground color of the given priority
	 */
	public static Color getFGColor(int priority) {
		CellColor cc = CellColor.fromPriority(priority);
		return cc.foreg;
	}
	
	/**
	 * Get the foreground color for the given priority
	 * 
	 * @param priority The priority of the foreground color to find
	 * @return The foreground color of the given priority
	 */
	public static Color getBGColor(int priority) {
		CellColor cc = CellColor.fromPriority(priority);
		return cc.backg;
	}
}
