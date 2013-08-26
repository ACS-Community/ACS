/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package alma.acs.gui.standards;

import java.awt.Color;

/**
 * The predefined colors available to Alma applications.
 */
public enum StandardColors {

	// ==> http://www.easycalculation.com/color-coder.php
	
	TEXT_FG("333333"), // darkgray-ish

	MAIN_BG("F9F7F0"), // sand-ish
	EDITOR_BG ("white"),
	SELECTION_BG ("A0B4D2"), // blueGray-ish
	SELECTION_NOFOCUS_BG ("lightGray"),

	STATUS_OKAY_BG ("54FB3C"), // green-ish
	STATUS_UNAVAILABLE_BG ("gray"),
	STATUS_UNKNOWN_BG ("gray"),
	STATUS_WARNING_BG ("FCD152"), // orange-ish
	STATUS_DELAY_BG ("F9FD4A"), // yellow-ish
	STATUS_ERROR_BG ("F95A3C"), // red-ish

	STATE_SHUTDOWN_BG ("F9FD4A"), // yellow-ish
	STATE_TRANSITING_BG ("F9FD4A"), // yellow-ish
	STATE_OPERATIONAL_BG ("54FB3C"); // green-ish


	/**
	 * This field contains the actual color needed by clients
	 */
	public Color color;

	private StandardColors(String rgb) {
		this.color = decode(rgb);
	}

	/**
	 * Translates colors to strings
	 */
	public static String encode (Color c) {
		return Integer.toHexString(c.getRGB()).toUpperCase().substring(2);
	}

	/**
	 * Translates strings to colors
	 */
	public static Color decode (String rgb) {
		try {
			return Color.decode("0x" + rgb);
		} catch (NumberFormatException exc) {/* simply continue */}

		try {
			return (Color) Color.class.getDeclaredField(rgb).get(null);
		} catch (Exception exc) {/* simply continue */}

		System.err.println("failed to decode " + rgb);
		return null;
	}
}