package com.cosylab.gui.components.r2.chart;

/**
 * Insert the type's description here.
 * Creation date: (12/4/00 15:18:28)
 * @author: 
 */
public class PlainChartStyle implements ChartStyle {
	private int symbolStyle= SYMBOL_DOT;
	private int lineStyle= LINE_PLAIN;
	private java.awt.Color symbolColor= java.awt.Color.black;
	private java.awt.Color lineColor= java.awt.Color.black;
	private int symbolSize = 7;
	public final static int SYMBOL_NONE = 0;
	public final static int SYMBOL_DOT = 1;
	public final static int LINE_NONE = 0;
	public final static int LINE_PLAIN = 1;
	protected boolean editable = true;
/**
 * ChartStyle constructor comment.
 */
public PlainChartStyle() {
	super();
}
/**
 * ChartStyle constructor comment.
 */
public PlainChartStyle(int lineStyle, java.awt.Color lineColor, int symbolStyle, int symbolSize, java.awt.Color symbolColor) {
	super();
	this.lineStyle= lineStyle;
	this.lineColor= lineColor;
	this.symbolStyle= symbolStyle;
	this.symbolSize= symbolSize;
	this.symbolColor= symbolColor;
}
/**
 * Insert the method's description here.
 * Creation date: (31.03.2001 00:20:53)
 * @param x int[]
 * @param y int[]
 * @param g java.awt.Graphics
 */
protected void drawDots(IntPointIterator it, java.awt.Graphics g) {
	g.setColor(symbolColor);
	int s= (int)(symbolSize/2.0);
	IntPoint p;
	while (it.hasNext()) {
		p=it.next();
		g.fillOval(p.x-s,p.y-s,symbolSize,symbolSize);
	}
}
/**
 * Insert the method's description here.
 * Creation date: (31.03.2001 00:11:03)
 * @param x int[]
 * @param y int[]
 * @param g java.awt.Graphics
 */
public void drawGraph(IntPointIterator it, java.awt.Graphics g) {
	if (lineStyle==LINE_PLAIN&&symbolStyle==SYMBOL_NONE) drawLine(it,g);
	else if (lineStyle==LINE_NONE&&symbolStyle==SYMBOL_DOT) drawDots(it,g);
	else if (lineStyle==LINE_PLAIN&&symbolStyle==SYMBOL_DOT) drawLineDots(it,g);
}
/**
 * Insert the method's description here.
 * Creation date: (31.03.2001 00:20:53)
 * @param x int[]
 * @param y int[]
 * @param g java.awt.Graphics
 */
protected void drawLine(IntPointIterator it, java.awt.Graphics g) {
	g.setColor(lineColor);
	IntPoint p,p1=null;
	if (it.hasNext()) p1= it.next();
	while(it.hasNext()) {
		p= it.next();
		g.drawLine(p1.x,p1.y,p.x,p.y);
		p1=p;
	}
}
/**
 * Insert the method's description here.
 * Creation date: (31.03.2001 00:20:53)
 * @param x int[]
 * @param y int[]
 * @param g java.awt.Graphics
 */
protected void drawLineDots(IntPointIterator it, java.awt.Graphics g) {
	int s= (int)(symbolSize/2.0);
	IntPoint p,p1=null;
	if (it.hasNext()) p1= it.next();
	while(it.hasNext()) {
		p= it.next();
		g.setColor(lineColor);
		g.drawLine(p1.x,p1.y,p.x,p.y);
		g.setColor(symbolColor);
		g.fillOval(p1.x-s,p1.y-s,symbolSize,symbolSize);
		p1=p;
	}
	if (p1!=null) {
		g.setColor(symbolColor);
		g.fillOval(p1.x-s,p1.y-s,symbolSize,symbolSize);
	}
}
/**
 * Insert the method's description here.
 * Creation date: (12/4/00 15:19:53)
 * @return java.awt.Color
 */
public java.awt.Color getLineColor() {
	return lineColor;
}
/**
 * Insert the method's description here.
 * Creation date: (12/4/00 15:19:19)
 * @return int
 */
public int getLineStyle() {
	return lineStyle;
}
/**
 * Insert the method's description here.
 * Creation date: (12/4/00 15:19:37)
 * @return java.awt.Color
 */
public java.awt.Color getSymbolColor() {
	return symbolColor;
}
/**
 * Insert the method's description here.
 * Creation date: (12/4/00 15:20:40)
 * @return int
 */
public int getSymbolSize() {
	return symbolSize;
}
/**
 * Insert the method's description here.
 * Creation date: (12/4/00 15:19:01)
 * @return int
 */
public int getSymbolStyle() {
	return symbolStyle;
}
/**
 * Insert the method's description here.
 * Creation date: (31.03.2001 00:13:43)
 * @return boolean
 */
public boolean isEditable() {
	return editable;
}
/**
 * Insert the method's description here.
 * Creation date: (12/4/00 15:19:53)
 * @param newLineColor java.awt.Color
 */
public void setLineColor(java.awt.Color newLineColor) {
	lineColor = newLineColor;
}
/**
 * Insert the method's description here.
 * Creation date: (12/4/00 15:19:19)
 * @param newLineStyle int
 */
public void setLineStyle(int newLineStyle) {
	lineStyle = newLineStyle;
}
/**
 * Insert the method's description here.
 * Creation date: (12/4/00 15:19:37)
 * @param newSymbolColor java.awt.Color
 */
public void setSymbolColor(java.awt.Color newSymbolColor) {
	symbolColor = newSymbolColor;
}
/**
 * Insert the method's description here.
 * Creation date: (12/4/00 15:20:40)
 * @param newSymbolSize int
 */
public void setSymbolSize(int newSymbolSize) {
	symbolSize = newSymbolSize;
}
/**
 * Insert the method's description here.
 * Creation date: (12/4/00 15:19:01)
 * @param newSymbolStyle int
 */
public void setSymbolStyle(int newSymbolStyle) {
	symbolStyle = newSymbolStyle;
}
}
