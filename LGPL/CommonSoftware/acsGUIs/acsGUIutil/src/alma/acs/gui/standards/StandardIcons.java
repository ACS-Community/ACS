package alma.acs.gui.standards;

import java.net.URL;

import javax.swing.ImageIcon;

/**
 * The predefined icons available to Alma applications.
 */
public enum StandardIcons {

	 START ("play.gif")
	,STOP ("stop.gif")
	,PAUSE ("pause.gif")
	,RESUME ("resume.gif")

	,CANCEL ("cross_red.gif")
	,CANCEL_INACTIVE ("cross_gray.gif")
	,APPROVE ("checkbox_on.gif")
	,DISMISS ("cross_black.gif")
	
	,UNKNOWN ("questionmark.gif")
	,WARNING ("warning.gif")
	,WARNING_INACTIVE ("warning_gray.gif")
	,IDEA ("bulb_yellow.gif")
	,IDEA_INACTIVE ("bulb_gray.gif")
	,LOCK ("lock.gif")
	,TRASH ("trash.gif")

	,LEFTWARD("arrow_left_yellow.gif")
	,RIGHTWARD("arrow_right_yellow.gif")
	,UPWARD("arrow_up_yellow.gif")
	,DOWNWARD("arrow_down_yellow.gif")
	
	,DOT_GRAY ("bubble_gray.gif")
	,DOT_GREEN ("checkmark.gif")
	,DOT_RED ("errorstate.gif")
	,DOT_YELLOW ("triangle.gif")
	;

	/**
	 * This field contains the actual icon needed by clients
	 */
	public ImageIcon icon;

	private StandardIcons(String loc) {
		this.icon = decode(loc);
	}

	/**
	 * Translates icons to strings
	 */
	public static String encode (ImageIcon icon) {
		return icon.getDescription();
	}

	/**
	 * Translates strings to icons
	 */
	public static ImageIcon decode (String loc) {
		String resdir = "alma/acs/gui/standards/resources/";
		try {
			if (!loc.startsWith("/") && !loc.startsWith(resdir))
				loc = resdir + loc;
			URL url = StandardIcons.class.getClassLoader().getResource(loc);
			return new ImageIcon(url, loc);

		} catch (Exception exc) {
			System.err.println("failed to decode " + loc);
			return null;
		}
	}
}