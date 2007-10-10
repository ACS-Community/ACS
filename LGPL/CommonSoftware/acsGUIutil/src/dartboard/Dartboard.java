package dartboard;

import java.awt.event.*;
import javax.swing.*;

public class Dartboard extends JPanel {
	private Dart[] darts;
	
	private class ComponentHandler implements ComponentListener {
		public void componentResized(ComponentEvent e) {
			if (darts != null)
				for (int i = 0; i < darts.length; i++)
					darts[i].setDartboardSize(getSize());
		}
		public void componentMoved(ComponentEvent e) {
		}
		public void componentHidden(ComponentEvent e) {
		}
		public void componentShown(ComponentEvent e) {
		}
	}
public Dartboard() {
	super();
	initialize();
}
private void initialize() {
	addComponentListener(new ComponentHandler());
}
public void paintComponent(java.awt.Graphics g) {
	super.paintComponent(g);

	// draw all the darts
	if (darts != null) {
		for (int i = 0; i < darts.length; i++) {
			darts[i].draw(g);
		}
	}
	
	
}
protected void setDarts(Dart[] darts) {
	this.darts = darts;
	for (int i = 0; i < darts.length; i++) {
		darts[i].setDartboard(this);
	}
	repaint();
}
}