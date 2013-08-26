package com.cosylab.gui.components.r2.chart;

/**
 * Default class for drawing y axis on chart.
 */
public class DefaultChartYAxis extends AbstractChartAxis implements ChartYAxis {
/**
 * DefaultChartYAxis default constructor.
 */
public DefaultChartYAxis() {
	super();
}
/**
 * This method draw y axis (with thickes, signatures etc.)
 * @param g java.awt.Graphics
 */
public void drawAxis(java.awt.Graphics g) {

	if (chartSize==null) return;

	if (yScale.isLogaritmic()) {
		drawLogAxis(g);
	} else {
		drawLinAxis(g);
	}
}
/**
 * This method draw y axis (with thickes, signatures etc.)
 * @param g java.awt.Graphics
 */
private void drawLinAxis(java.awt.Graphics g) {

	g.setColor(lineColor);

	// draw axis

	g.drawLine(0,-majorTickLength,0,chartSize.height-1+majorTickLength);
//	g.drawLine(chartSize.width-1,0,chartSize.width-1,chartSize.height-1);

	//  min and max tickmarks

	g.drawLine(
		1-majorTickLength,
		0,
		0,
		0);

	// draw minor tickmarks

	if (chartSize.width < 20)
		return;

	for (int i= 0; i < 9; i++) {
		g.drawLine(
			1-minorTickLength,
			(int) ((i + 1) * (double) chartSize.height / 10.0),
			0,
			(int) ((i + 1) * (double) chartSize.height / 10.0));
	}

	// write out min and max values for x and y

	g.setColor(fontColor);
	g.setFont(font);

	String s= format(yScale.min());
	java.awt.FontMetrics fm= g.getFontMetrics(font);

	int width= fm.stringWidth(s);
	
	g.drawString(
		s,
		-width-majorTickLength-textMargin.right,
		chartSize.height-2+(int)(fm.getHeight()/3.0));
	
	s= format(yScale.max());

	int width1= fm.stringWidth(s);

	g.drawString(
		s,
		-width1-majorTickLength-textMargin.right,
		(int)(fm.getHeight()/3.0)-1);

	preferedHeight= fm.getHeight() + textMargin.bottom+textMargin.top+majorTickLength;
	preferedWidth= ( width>width1 ? width : width1 ) + textMargin.left+textMargin.right+majorTickLength;


}
/**
 * This method draw y axis (with thickes, signatures etc.)
 * @param g java.awt.Graphics
 */
private void drawLogAxis(java.awt.Graphics g) {

	// draw axis

	g.drawLine(0,-majorTickLength,0,chartSize.height-1+majorTickLength);

	//  min and max tickmarks

	g.drawLine(
		1-majorTickLength,
		0,
		0,
		0);

	// write out min and max values for x and y

	String s= format(yScale.min());
	java.awt.FontMetrics fm= g.getFontMetrics(font);

	int width= fm.stringWidth(s);
	
	g.drawString(
		s,
		-width-majorTickLength-textMargin.right,
		chartSize.height-2+(int)(fm.getHeight()/2.0));
	
	s= format(yScale.max());

	int width1= fm.stringWidth(s);

	g.drawString(
		s,
		-width1-majorTickLength-textMargin.right,
		(int)(fm.getHeight()/2.0)-1);

	preferedHeight= fm.getHeight() + textMargin.bottom+textMargin.top+majorTickLength;
	preferedWidth= ( width>width1 ? width : width1 ) + textMargin.left+textMargin.right+majorTickLength;

	// draw minor tickmarks

	if (chartSize.width < 20)
		return;

	for (int i= 1; i < 10; i++) {
		g.drawLine(
			1-minorTickLength,
			(int) (Math.log(i) * (double) chartSize.height ),
			0,
			(int) (Math.log(i) * (double) chartSize.height ));
	}


}
}
