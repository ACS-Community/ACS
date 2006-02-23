package dartboard;

import java.awt.*;
import javax.swing.ImageIcon;
/**
 * Insert the type's description here.
 * Creation date: (11/7/00 9:02:03 PM)
 * @author: 
 */
public class Telescope extends Dart {
	private ImageIcon icon = null;
/**
 * Telescope constructor comment.
 * @param x int
 * @param y int
 */
public Telescope() {
	super();
	java.net.URL url = this.getClass().getClassLoader().getSystemResource("dartboard/telescope.gif");
	if (url != null) icon = new javax.swing.ImageIcon(url);
	icon = new javax.swing.ImageIcon(url);
}
protected void drawDart(Graphics g) {
	if (icon != null) icon.paintIcon(dartboard, g, -icon.getIconWidth() / 2, -icon.getIconHeight() / 2);
	else {
		g.setColor(Color.green);
		g.fillRect(-10, -10, 20, 20);
	}
}

}
