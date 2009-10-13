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

	STATUS_OK_BG ("54FB3C"), // green-ish
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