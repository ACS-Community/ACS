package com.cosylab.gui.components.r2.chart;

import com.cosylab.gui.components.r2.chart.accessories.*;
/**
 * This is class for drawing y axis.
 */
public class ImprovedChartYAxis extends AbstractChartAxis implements ChartYAxis {
/**
 * ImprovedChartYAxis constructor comment.
 */
public ImprovedChartYAxis() {
	super();
}
/**
 * This method draw Y axis (with thickes, signatures etc.). It is more sophisticated than <code>DefaultChartXAxis</code>.
 * @param g java.awt.Graphics
 */
public void drawAxis(java.awt.Graphics g) {

	if (chartSize==null) return;

	double ymin=yScale.min();
	double ymax=yScale.max();

	double diff=ymax-ymin;

	double densityFactor=2.5;

	// draw axis

	g.drawLine(0,0,0,chartSize.height);

//	xminDraw is rounded number, greather than ymin, around which all numbers are placed
	double yminDraw=ymin;
	
	try {
		yminDraw=RoundDoubleToDouble.roundUpToDigits(ymin,diff,1);
	} catch (IllegalArgumentException e) {
		System.out.print(" In com.cosylab.gui.chart.ImprovedChartYAxis.drawAxis() occured exception: "+e.toString());
		}

//	setting output format 
	setFormat(FormatStringMaker.getFormatString(yminDraw));

	String s= format(ymin);
	String s1=format(ymax);
	java.awt.FontMetrics fm= g.getFontMetrics(font);

//	calculating width of string for output	
	int width;
	if (s.length()>s1.length()) {
		width= fm.stringWidth(s);
	} else width= fm.stringWidth(s1);

//	number of output strings
	int numOfNumber=(int)Math.rint(chartSize.height/(fm.getHeight()*densityFactor));

//	intervals at outputing
	double mInterval=diff/numOfNumber;

	double minInterval=IntervalNumber.getRightInterval(mInterval);
	
	int yLowLimit=fm.getHeight()/2; 

	double thicketInterval=IntervalNumber.getSubInterval(mInterval);
	double minThicketInterval=IntervalNumber.getSubSubInterval(mInterval);

//	re-setting output format 
	setFormat(FormatStringMaker.getFormatString(minInterval));

	s=format(ymin);
	s1=format(ymax);

//	tests if number of strings is not at least 2
	if (numOfNumber<2) {
		g.drawLine(-majorTickLength,chartSize.height,0,chartSize.height);
		g.drawLine(-majorTickLength,0,0,0);

		g.drawString(s,-fm.stringWidth(s)-majorTickLength-textMargin.right,
			chartSize.height-2+yLowLimit);

		g.drawString(s1,-fm.stringWidth(s1)-majorTickLength-textMargin.right,
		-2+yLowLimit);

		preferedHeight= fm.getHeight() + textMargin.bottom+textMargin.top+majorTickLength;
		preferedWidth= width + textMargin.left+textMargin.right+majorTickLength;
		return;
	}

	int i;

//	draw numbers left from yminDraw
	for (i=0;(yminDraw-i*minInterval)>=ymin;i++) { 
		g.drawString(
			format(yminDraw-i*minInterval),
			-fm.stringWidth(format(yminDraw-i*minInterval))-majorTickLength-textMargin.right,
			(int)((1-(yminDraw-ymin-i*minInterval)/diff)*chartSize.height)-2+yLowLimit);
		}

//	draw numbers right from yminDraw
	for (i=1;(yminDraw+i*minInterval)<=ymax;i++) { 
		g.drawString(
			format(yminDraw+i*minInterval),
			-fm.stringWidth(format(yminDraw+i*minInterval))-majorTickLength-textMargin.right,
			(int)((1-(yminDraw-ymin+i*minInterval)/diff)*chartSize.height)-2+yLowLimit);
		}

//	draw thicket and minThicket, left from xminDraw

	for (i=0;(yminDraw-i*thicketInterval)>=ymin;i++) {
		g.drawLine(
			-majorTickLength,
			(int)Math.rint((1-(yminDraw-ymin-i*thicketInterval)/diff)*chartSize.height),
			0,
			(int)Math.rint((1-(yminDraw-ymin-i*thicketInterval)/diff)*chartSize.height));
		}
	for (i=1;(yminDraw-i*minThicketInterval)>=ymin;i++) {
		g.drawLine(
			-minorTickLength,
			(int)Math.rint((1-(yminDraw-ymin-i*minThicketInterval)/diff)*chartSize.height),
			0,
			(int)Math.rint((1-(yminDraw-ymin-i*minThicketInterval)/diff)*chartSize.height));
		}

//	draw thicket and minThicket, right from xminDraw	
	for (i=0;(yminDraw+i*thicketInterval)<=ymax;i++) {
		g.drawLine(
			-majorTickLength,
			(int)((1-(yminDraw-ymin+i*thicketInterval)/diff)*chartSize.height),
			0,
			(int)((1-(yminDraw-ymin+i*thicketInterval)/diff)*chartSize.height));
		}
	for (i=1;(yminDraw+i*minThicketInterval)<=ymax;i++) {
		g.drawLine(
			-minorTickLength,
			(int)((1-(yminDraw-ymin+i*minThicketInterval)/diff)*chartSize.height),
			0,
			(int)((1-(yminDraw-ymin+i*minThicketInterval)/diff)*chartSize.height));
		}


	preferedHeight= fm.getHeight() + textMargin.bottom+textMargin.top+majorTickLength;
	preferedWidth= width + textMargin.left+textMargin.right+majorTickLength;
}
}
