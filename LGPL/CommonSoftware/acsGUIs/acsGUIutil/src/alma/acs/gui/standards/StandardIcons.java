package alma.acs.gui.standards;

import java.net.URL;

import javax.swing.ImageIcon;

/**
 * The predefined icons available to Alma applications.
 */
public enum StandardIcons {

	 ACTION_START ("play.gif")
	,ACTION_STOP ("stop.gif")
	,ACTION_PAUSE ("pause.gif")
	,ACTION_RESUME ("resume.gif")

	,ACTION_CANCEL ("cross_red.gif")
	,ACTION_CANCEL_INACTIVE ("cross_gray.gif")
	,ACTION_DISMISS ("cross_black.gif")

	,ACTION_OPEN ("openfolder.gif")
	,ACTION_SAVE ("floppydisk.gif")
	,ACTION_DELETE ("cross_red.gif")
	,ACTION_TRASH ("trash.gif")
	,ACTION_HELP ("help.gif")

	,STATUS_OKAY ("checkmark.gif")
	,STATUS_UNKNOWN ("bubble_gray.gif")
	,STATUS_NOTEWORTHY ("triangle.gif")
	,STATUS_WARNING ("warning.gif")
	,STATUS_WARNING_INACTIVE ("warning_gray.gif")
	,STATUS_ERROR ("errorstate.gif")

	,LEFTWARD("arrow_left_yellow.gif")
	,RIGHTWARD("arrow_right_yellow.gif")
	,UPWARD("arrow_up_yellow.gif")
	,DOWNWARD("arrow_down_yellow.gif")
	
	,QUESTION ("questionmark.gif")
	,IDEA ("bulb_yellow.gif")
	,IDEA_INACTIVE ("bulb_gray.gif")
	,INFO ("info.gif")
	,APPROVED ("checkbox_on.gif")
	,LOCK ("lock.gif")

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