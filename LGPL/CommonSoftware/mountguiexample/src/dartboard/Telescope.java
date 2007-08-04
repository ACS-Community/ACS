package dartboard;

import java.awt.*;
import javax.swing.ImageIcon;
/**
 * Insert the type's description here.
 * Creation date: (11/7/00 9:02:03 PM)
 * @author: 
 */
public class Telescope extends Dart {
	
	// The normal and error icons
	private ImageIcon icon = null;
	private ImageIcon iconError = null;
	
	// The icon used to draw.
	// It can be the normal or the error one depending on the error
	// status
	private ImageIcon iconDraw;
	
	/**
	 * Telescope constructor comment.
	 * @param x int
	 * @param y int
	 */
	public Telescope() {
		super();
		java.net.URL url = this.getClass().getClassLoader().getSystemResource("dartboard/telescope.png");
		java.net.URL urlError = this.getClass().getClassLoader().getSystemResource("dartboard/telescopeError.png");
		if (url != null) {
			icon = new ImageIcon(url);
		} else {
			System.err.println("Telescope icon is null");
		}
		if (urlError != null) {
			iconError = new ImageIcon(urlError);
		} else {
			System.err.println("Telescope error icon is null");
		}
		// Start with a normal icon
		iconDraw=icon; 
	}
	protected void drawDart(Graphics g) {
		if (iconDraw != null) {
			iconDraw.paintIcon(dartboard, g, -iconDraw.getIconWidth() / 2, -iconDraw.getIconHeight() / 2);
		}
		else {
			g.setColor(Color.green);
			g.fillRect(-10, -10, 20, 20);
		}
	}
	
	public void setError(boolean err) {
		if (err) {
			iconDraw=iconError;
		} else {
			iconDraw=icon;
		}
	}

}
