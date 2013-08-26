package com.cosylab.gui.components.r2;

/**
 * Insert the type's description here.
 * Creation date: (26-Oct-01 12:38:04 PM)
 * @author: 
 */

public class SearchListListener implements java.awt.event.KeyListener {
	private long lastKeyPressedTime = 0;
	private String searchString = "";
/**
 * SearchListListener constructor comment.
 */
public SearchListListener() {
	super();
}
	/**
	 * Invoked when a key has been pressed.
	 */
public void keyPressed(java.awt.event.KeyEvent e) {}
	/**
	 * Invoked when a key has been released.
	 */
public void keyReleased(java.awt.event.KeyEvent e) {}
	/**
	 * Invoked when a key has been typed.
	 * This event occurs when a key press is followed by a key release.
	 */
public void keyTyped(java.awt.event.KeyEvent e) {

	char ch = e.getKeyChar();
	if (!Character.isLetterOrDigit(ch))
		return;

	if (lastKeyPressedTime + 1000 < System.currentTimeMillis())
		searchString = "";
	lastKeyPressedTime = System.currentTimeMillis();

	searchString += Character.toLowerCase(ch);

/*	int listSize = clm.getSize();
	
	for (int k = 0; k < listSize; k++) {
		String str = ((String) clm.get(k)).toLowerCase();
		if (str.startsWith(m_key)) {
			ownerList.setSelectedIndex(k);
			ownerList.ensureIndexIsVisible(k);
			break;
		}
	}
*/

}
}
