package dartboard;

import java.awt.*;
/**
 * Insert the type's description here.
 * Creation date: (11/27/00 9:09:15 PM)
 * @author: Administrator
 */
public class TelescopeDestination extends Dart {
	private static int a = 2;
	private static int[] xs = {0*a, 2*a, 3*a, 3*a, 1*a, 3*a, 3*a, 2*a, 0*a, -2*a, -3*a, -3*a, -1*a, -3*a, -3*a, -2*a};
	private static int[] ys = {1*a, 3*a, 3*a, 2*a, 0*a, -2*a, -3*a, -3*a, -1*a, -3*a, -3*a, -2*a, 0*a, 2*a, 3*a, 3*a};

	private static Polygon cross;
/**
 * TelescopeDestination constructor comment.
 */
public TelescopeDestination() {
	super();
	cross = new Polygon(xs, ys, xs.length);
}
protected void drawDart(Graphics g) {
	g.setColor(Color.black);
	g.fillPolygon(cross);
	g.drawPolygon(cross);
}
}
