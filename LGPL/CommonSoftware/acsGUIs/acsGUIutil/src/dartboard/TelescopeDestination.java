package dartboard;

import java.awt.*;

import javax.swing.ImageIcon;

/**
 * Insert the type's description here. Creation date: (11/27/00 9:09:15 PM)
 * 
 * @author: Administrator
 */
public class TelescopeDestination extends Dart {

	// The cross icon
	private ImageIcon icon = null;

	/**
	 * TelescopeDestination constructor comment.
	 */
	public TelescopeDestination() {
		super();
		java.net.URL url = this.getClass().getClassLoader().getSystemResource("dartboard/dest.png");
		if (url != null) {
			icon = new ImageIcon(url);
		} else {
			System.err.println("Destination icon is null");
		}
	}

	protected void drawDart(Graphics g) {
		if (icon != null) {
			icon.paintIcon(dartboard, g, -icon.getIconWidth() / 2, -icon.getIconHeight() / 2);
		}
		else {
			g.setColor(Color.yellow);
			g.fillRect(-10, -10, 20, 20);
		}
	}

	public void setError(boolean err) {
	}
}
