package dartboard;

import java.awt.*;

public abstract class Dart {
	protected double azimuth = 0;
	protected double elevation = 0;

	protected Dartboard dartboard = null;
	private Dimension dartboardSize = new Dimension(0,0);
	private int x = 0;
	private int y = 0;
	private boolean isDisplayed = false;
public Dart() {
}
protected int calculateX(double elevation, double azimuth) {
	int x;
	int l = Math.min(dartboardSize.width, dartboardSize.height) / 2;
	
	x = (int)(0.9 * ((90 - elevation) / 90) * l * Math.sin(azimuth * Math.PI / 180));
	
	return x;
}
protected int calculateY(double elevation, double azimuth) {
	int y;
	int l = Math.min(dartboardSize.width, dartboardSize.height) / 2;
	
	y = (int)(-0.9 * ((90 - elevation) / 90) * l * Math.cos(azimuth * Math.PI / 180));

	return y;
}
public void draw(java.awt.Graphics g) {
	if (isDisplayed) {
		Color c = g.getColor();
		g.translate(x, y);
	
		drawDart(g);
	
		g.translate(-x, -y);
		g.setColor(c);
	}
}
protected void drawDart(Graphics g) {}
private void recalculateCartesian(double elevation, double azimuth) {
	int x;
	int y;
	
	x = dartboardSize.width / 2 + calculateX(elevation, azimuth);
	y = dartboardSize.height / 2 + calculateY(elevation, azimuth);

	this.x = x;
	this.y = y;

	if (dartboard != null) dartboard.repaint();
}
public void setAzimuth(double azimuth) {
	this.azimuth = azimuth;
	if (!isDisplayed) isDisplayed = true;
	recalculateCartesian(elevation, azimuth);
}
public void setDartboard(Dartboard dartboard) {
	this.dartboard = dartboard;
}
public void setDartboardSize(Dimension dartboardSize){
	this.dartboardSize = dartboardSize;
	recalculateCartesian(elevation, azimuth);
}
public void setElevation(double elevation) {
	this.elevation = elevation;
	if (!isDisplayed) isDisplayed = true;
	recalculateCartesian(elevation, azimuth);
}
public void setPosition(double elevation, double azimuth) {
	this.elevation = elevation;
	this.azimuth = azimuth;
	if (!isDisplayed) isDisplayed = true;
	recalculateCartesian(elevation, azimuth);
}

/**
 * Called in case of error.
 * 
 * Each derived class will take the opportune action
 * @param error
 */
public abstract void setError(boolean error);
}
