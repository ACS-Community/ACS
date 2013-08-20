package com.cosylab.gui.components.r2.chart;

/**
 * Insert the type's description here.
 * Creation date: (11/27/00 14:49:13)
 * @author: 
 */
public class Chartics {
	private java.awt.Graphics graphics;
	private java.awt.image.BufferedImage image;
	private java.awt.Color background = java.awt.Color.white;
/**
 * Chartics constructor comment.
 */
public Chartics() {
	super();
}
/**
 * Insert the method's description here.
 * Creation date: (11/27/00 14:51:06)
 * @param x int[]
 * @param y int[]
 */
public void drawGraph(IntPointIterator it, ChartStyle style) {
	style.drawGraph(it,graphics);
}
/**
 * Insert the method's description here.
 * Creation date: (1/2/01 17:06:04)
 * @return java.awt.Color
 */
public java.awt.Color getBackground() {
	return background;
}
/**
 * Insert the method's description here.
 * Creation date: (11/27/00 14:50:00)
 * @return java.awt.Graphics
 */
public java.awt.Graphics getGraphics() {
	return graphics;
}
/**
 * Insert the method's description here.
 * Creation date: (1/2/01 16:47:01)
 * @return java.awt.image.BufferedImage
 */
public java.awt.image.BufferedImage getImage() {
	return image;
}
/**
 * Insert the method's description here.
 * Creation date: (1/2/01 17:06:04)
 * @param newBackgroundColor java.awt.Color
 */
public void setBackground(java.awt.Color newBackground) {
	background = newBackground;
}
/**
 * Insert the method's description here.
 * Creation date: (1/2/01 16:47:01)
 * @param newImage java.awt.image.BufferedImage
 */
public void setImage(java.awt.image.BufferedImage newImage) {
	image = newImage;
	graphics= image!=null ? image.getGraphics() : null;

	if (background!=null && image!=null) {
		((java.awt.Graphics2D)graphics).setBackground(background);
		graphics.setColor(background);
		graphics.fillRect(0,0,image.getWidth(),image.getHeight());
	}
}
/**
 * Insert the method's description here.
 * Creation date: (1/2/01 16:49:18)
 * @param shift int
 */
public void shift(int x, int y, int width, int height, int dx, int dy) {
//	java.awt.image.BufferedImage im= image.getSubimage(x,0,width,image.getHeight());
//	graphics.drawImage(im,0,0,null);
	graphics.copyArea(x,y,width,height,dx,dy);
	java.awt.Color c= graphics.getColor();
	graphics.setColor(background);
	if (dx<0) graphics.fillRect(x+width+dx,y,-dx,height);
	else if (dx>0) graphics.fillRect(x,y,dx,height);
	if (dy<0) graphics.fillRect(x,y+height+dy,width,-dy);
	else if (dy>0) graphics.fillRect(x,y,width,dy);
	graphics.setColor(c);
}
}
