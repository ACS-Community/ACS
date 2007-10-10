package dartboard;

import java.awt.*;
/**
 * Insert the type's description here.
 * Creation date: (11/8/00 12:06:28 AM)
 * @author: 
 */
public class Grid extends Dart {
	
	// Set to true in case of error
	boolean error=false;
/**
 * Grid constructor comment.
 * @param x int
 * @param y int
 * @param isDisplayed boolean
 */
public Grid() {
	super();
	setPosition(90, 0);
}
protected void drawDart(Graphics g) {
	if (error) {
		g.setColor(Color.red);
	} else {
		g.setColor(Color.blue);
	}

	// needed only for this Dart, the Grid.
	Font f = g.getFont();
	g.setFont(new Font("SansSerif", Font.PLAIN, 12));
	int x1, y1, x2, y2;
	
	// draw a horizontal and a vertical line
	x1 = calculateX(0,0);
	y1 = calculateY(0,0);
	x2 = calculateX(0,180);
	y2 = calculateY(0,180);
	g.drawLine(x1, y1, x2, y2);

	x1 = calculateX(0,90);
	y1 = calculateY(0,90);
	x2 = calculateX(0,270);
	y2 = calculateY(0,270);
	g.drawLine(x1, y1, x2, y2);

	// draw the circles and text
	for (int i = 10; i <= 90; i += 20) {
		x1 = calculateX(i,0);
		y1 = calculateY(i,0);
		x2 = calculateX(i,270);
		y2 = calculateY(i,270);
		g.drawOval(x2, y1, 2 * (x1 - x2), 2 * (y2 - y1));
		
		x1 = calculateX(i,90);
		y1 = calculateY(i,90);
		g.setColor(Color.black);
		g.drawString(Integer.toString(i), x1 + 3, y1 - 2);
		if (error) {
			g.setColor(Color.red);
		} else {
			g.setColor(Color.blue);
		}
	}
	g.setFont(f);
}

public void setError(boolean err) {
	error=err;
}
}
