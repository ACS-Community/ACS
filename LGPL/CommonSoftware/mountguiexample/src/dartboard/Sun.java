package dartboard;

import java.awt.*;
import javax.swing.ImageIcon;
/**
 * Insert the type's description here.
 * Creation date: (11/7/00 9:04:45 PM)
 * @author: 
 */
public class Sun extends Planet {
	private ImageIcon icon = null;
/**
 * Sun constructor comment.
 * @param x int
 * @param y int
 */
public Sun() {
	super();
	java.net.URL url = this.getClass().getClassLoader().getSystemResource("Dartboard/sun.gif");
	if (url != null) icon = new javax.swing.ImageIcon(url);
}
protected void drawDart(Graphics g) {
	if (icon != null) icon.paintIcon(dartboard, g, -icon.getIconWidth() / 2, -icon.getIconHeight() / 2);
	else {
		g.setColor(Color.yellow);
		g.fillOval(-15, -15, 30, 30);
	}
}

public void setError(boolean err) {}
}
