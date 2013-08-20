package com.cosylab.gui.components.r2.chart;

import com.cosylab.gui.components.r2.chart.accessories.*;
/**
 * This is class for drawing x axis.
 */
public class ImprovedChartXAxis extends AbstractChartAxis implements ChartXAxis {
	private boolean isLogaritmic=false;
	private double xmin, xminDraw;
	private double xmax, xmaxDraw;
	private java.awt.FontMetrics fm;
	private double yOrigin=-1.0;
	private double ymax, ymin;
/**
 * ImprovedChartXAxis default constructor.
 */
public ImprovedChartXAxis() {
	super();
}
/**
 * This method draw X axis (with thickes, signatures etc.). It is more siphisticated than <code>DefaultChartXAxis</code>.
 * @param g java.awt.Graphics
 */
public void drawAxis(java.awt.Graphics g) {

	xmin=xScale.min();
	xmax=xScale.max();

	ymax=yScale.max();
	ymin=yScale.min();
	
//	if (chartSize==null||ymin>yOrigin||ymax<yOrigin) return;
	if (chartSize==null) return;

	if (isLogaritmic) 
		if (xmin!=0.0) drawLogaritmicAxis(g);
		else {
//			System.out.println("Cannot draw logaritmic axis, because log(0) is not finite number!");
			drawLinearAxis(g);
		}
	else drawLinearAxis(g);

}
/**
 * This is internal method, which is called by <code>drawAxis()</code> method
 * and draw axis with linear scale.
 */
private void drawLinearAxis(java.awt.Graphics g) {
	if (chartSize==null) return;

	double xmin=xScale.min();
	double xmax=xScale.max();

	double diff=xmax-xmin;

	double densityFactor=2.5;
	
	g.setColor(lineColor);

	// draw axis

	g.drawLine(0,chartSize.height,chartSize.width+majorTickLength,chartSize.height);

//	xminDraw is rounded number, greather than xmin, around which all numbers are placed
	double xminDraw=xmin;
	
	try {
		xminDraw=RoundDoubleToDouble.roundUpToDigits(xmin,diff,1);
	} catch (IllegalArgumentException e) {
		System.out.print(" In com.cosylab.gui.chart.ImprovedChartXAxis.drawAxis() occured exception: "+e.toString());
		}

//	setting output format 
	String format=FormatStringMaker.getFormatString(xminDraw);
	setFormat(format);

	String s= format(xmin);
	String s1=format(xmax);
	java.awt.FontMetrics fm= g.getFontMetrics(font);

//	calculating width of string for output	
	int width;
	if (s.length()>s1.length()) {
		width= fm.stringWidth(s);
	} else width= fm.stringWidth(s1);

//	number of output strings
	int numOfNumber=(int)Math.rint(chartSize.width/(width*densityFactor));

//	intervals at axis
	double mInterval=diff/numOfNumber;

	double minInterval=IntervalNumber.getRightInterval(mInterval);
	
	int xLowLimit=-width/2; 

	double thicketInterval=IntervalNumber.getSubInterval(mInterval);
	double minThicketInterval=IntervalNumber.getSubSubInterval(mInterval);


	g.setColor(fontColor);

	
//	re-setting output format 
	setFormat(FormatStringMaker.getFormatString(minInterval));

	s=format(xmin);
	s1=format(xmax);
	if (s.length()>s1.length()) {
		width= fm.stringWidth(s);
	} else width= fm.stringWidth(s1);

//	re-setting number of output strings
	numOfNumber=(int)Math.rint(chartSize.width/(width*densityFactor));

//	re-setting intervals at axis
	mInterval=diff/numOfNumber;

	minInterval=IntervalNumber.getRightInterval(mInterval);
	
	xLowLimit=-width/2; 

	thicketInterval=IntervalNumber.getSubInterval(mInterval);
	minThicketInterval=IntervalNumber.getSubSubInterval(mInterval);

//	tests if number of strings is not at least 2
	if (numOfNumber<2) {
		g.drawLine(chartSize.width,chartSize.height,chartSize.width,chartSize.height+majorTickLength);
		g.drawLine(0,chartSize.height,0,chartSize.height+majorTickLength);

		g.drawString(s,(int)(-width/2.0),
		chartSize.height + fm.getHeight() + textMargin.top);

		g.drawString(s1,chartSize.width-(int)(width/2.0),
		chartSize.height + fm.getHeight() + textMargin.top);

		preferedHeight= fm.getHeight() + textMargin.bottom+textMargin.top+majorTickLength;
		preferedWidth= width + textMargin.left+textMargin.right+majorTickLength;
		return;
	}

	try {
		xminDraw=RoundDoubleToDouble.truncToDigits(xmin,IntervalNumber.getRoundedInterval(minInterval),1);
		xminDraw=Math.ceil((xmin-xminDraw)/minInterval)*minInterval+xminDraw;
	} catch (IllegalArgumentException e) {
		System.out.print(" In com.cosylab.gui.chart.ImprovedChartXAxis.drawAxis() occured exception: "+e.toString());
		}
	
	int i;
//	draw numbers left from xminDraw
	for (i=0;(xminDraw-i*minInterval)>=xmin;i++) { 
		g.drawString(
			format(xminDraw-i*minInterval),
			(int)((xminDraw-xmin-i*minInterval)/diff*chartSize.width)+xLowLimit,
			chartSize.height + fm.getHeight() + textMargin.top);	
		}

//	draw numbers right from xminDraw
	for (i=1;(xminDraw+i*minInterval)<=xmax;i++) {
		g.drawString(
			format(xminDraw+i*minInterval),
			(int)((xminDraw-xmin+i*minInterval)/diff*chartSize.width)+xLowLimit,
			chartSize.height + fm.getHeight() + textMargin.top);
		}


	g.setColor(lineColor);

//	draw thicket and minThicket, left from xminDraw

	for (i=1;(xminDraw-i*thicketInterval)>=xmin;i++) {
		g.drawLine(
			(int)((xminDraw-xmin-i*thicketInterval)/diff*chartSize.width),
			chartSize.height,
			(int)((xminDraw-xmin-i*thicketInterval)/diff*chartSize.width),
			chartSize.height+majorTickLength);
		}
	for (i=1;(xminDraw-i*minThicketInterval)>=xmin;i++) {
		g.drawLine(
			(int)((xminDraw-xmin-i*minThicketInterval)/diff*chartSize.width),
			chartSize.height,
			(int)((xminDraw-xmin-i*minThicketInterval)/diff*chartSize.width),
			chartSize.height+minorTickLength);
		}

//	draw thicket and minThicket, right from xminDraw	
	for (i=0;(xminDraw+i*thicketInterval)<=xmax;i++) { 
		g.drawLine(
			(int)((xminDraw-xmin+i*thicketInterval)/diff*chartSize.width),
			chartSize.height,
			(int)((xminDraw-xmin+i*thicketInterval)/diff*chartSize.width),
			chartSize.height+majorTickLength);
		}
	
	for (i=1;(xminDraw+i*minThicketInterval)<=xmax;i++) {				
		g.drawLine(
			(int)((xminDraw-xmin+i*minThicketInterval)/diff*chartSize.width),
			chartSize.height,
			(int)((xminDraw-xmin+i*minThicketInterval)/diff*chartSize.width),
			chartSize.height+minorTickLength);
		}


	preferedHeight= fm.getHeight() + textMargin.bottom+textMargin.top+majorTickLength;
	preferedWidth= width + textMargin.left+textMargin.right+majorTickLength;
}
/**
 * This is internal method, which is called by <code>drawAxis()</code> method
 * and draw axis with logaritmic scale.
 */
private void drawLogaritmicAxis(java.awt.Graphics g) {

//	factors, which defines density of thicks and displayed digits (greather means higher density)	
	double densityFactor1=1;
	double densityFactor2=1;
	
	double dens1, dens2;
	int thick, digit;

// draw axis

	g.drawLine(0,(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin)),chartSize.width+majorTickLength,(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin)));

//	xminDraw is power of ten, first greather than xmin
//	xmaxDraw is power of ten, first lower than xmax

	xminDraw=RoundDoubleToDouble.roundUpToTenth(xmin);
	xmaxDraw=RoundDoubleToDouble.truncToTenth(xmax);
	
//	calculating width of string for output	
	String formatMin=FormatStringMaker.getFormatString(xminDraw/10);
	String formatMax=FormatStringMaker.getFormatString(xmaxDraw);
	
	setFormat(formatMax);
	String sMax=format(xmax);

	setFormat(formatMin);
	String sMin= format(xmin);

	fm= g.getFontMetrics(font);

	int width;
	if (sMin.length()>sMax.length()) {
		width= fm.stringWidth(sMin);
	} else width= fm.stringWidth(sMax);

//	first approximate of possible number of displayed digits
	int numOfNumber=(int)Math.rint(chartSize.width/(width*2));
	
//	tests if number of displayed digit is not at least 2
	if (numOfNumber<2||Math.log(xmax/xmin)<0.7) {
		g.drawLine(
			chartSize.width,
			(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin)),
			chartSize.width,
			(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin))+majorTickLength);
		
		g.drawLine(
			0,
			(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin)),
			0,
			(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin))+majorTickLength);

/*		g.drawLine(
					(int)(Math.log((1+xmax/xmin)/2)/Math.log(xmax/xmin)*chartSize.width),
					(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin)),
					(int)(Math.log((1+xmax/xmin)/2)/Math.log(xmax/xmin)*chartSize.width),
					(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin))+minorTickLength);
*/

		g.drawString(sMin,(int)(-width/2.0),
		chartSize.height + fm.getHeight() + textMargin.top);

		g.drawString(sMax,chartSize.width-(int)(width/2.0),
		(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin)) + fm.getHeight() + textMargin.top);

		preferedHeight= fm.getHeight() + textMargin.bottom+textMargin.top+majorTickLength;
		preferedWidth= width + textMargin.left+textMargin.right+majorTickLength;
		return;
	} 
// parameter for thick density 
	dens1=densityFactor1*chartSize.width/Math.log(xmax/xmin);
	
// parameter for text density 
	dens2=densityFactor2*dens1/width; 

	digit=(int)Math.rint(2/dens2);

	try {
		if (dens1>20) {
			//	thick=-1
			if (dens2>4) {
 				logDraw(-1,-1,g);
				}
			else if (dens2>2) {
 				logDraw(-1,0,g);
				}  
			else {
				logDraw(-1,digit,g);
				}
		} 
		else if (dens1>8) {
			//	thick=0
			logDraw(0,digit,g);
		} 
		else {
			thick=(int)Math.ceil(1/dens1);
			if (digit<thick) digit=thick;
			logDraw(thick,digit,g);
		}
	} catch (IllegalArgumentException e) {
		System.out.print(e.toString());
	}
					
	preferedHeight= fm.getHeight() + textMargin.bottom+textMargin.top+majorTickLength;
	preferedWidth= width + textMargin.left+textMargin.right+majorTickLength;
}
/**
 * Tests if scale is logaritmic.
 */
public boolean isIsLogaritmic() {
	return isLogaritmic;
}
/**
 * This is internal method that is used only by <code>drawLogaritmicAxis()</code> just
 * to draw thickets and digits.
 *
 * Used when ratio between <code>xmax</code> and <code>xmin</code> is
 * greather than 10.
 */
private void logDraw(int thick, int digit, java.awt.Graphics g) throws IllegalArgumentException {

	if (!((thick==-1&&digit>=-1)||(thick==0&&digit>=1)||(thick>=1&&digit>=thick))) throw new IllegalArgumentException("Not possible combination of arguments of logDraw1() method!");
	
	int i, j;

	double xVariable=xminDraw;
	String formatDraw;

//	draw thicks and digits before xminDraw (if neccessary)
	if (thick==-1) {
		if (xVariable<=xmax) i=1; else i=(int)Math.ceil(10*(1-xmax/xVariable));
		
		for (; xVariable*(1-.1*i)>=xmin; i++){
			
			if (((i==3||i==5||i==7||i==8)&&digit==-1)) {
				g.drawString(
					format(xVariable*(1-.1*i)),
					(int)(Math.log(xVariable*(1-.1*i)/xmin)/Math.log(xmax/xmin)*chartSize.width-fm.stringWidth(format(xVariable*(1-.1*i)))/2),
					(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin)) + fm.getHeight() + textMargin.top);

				g.drawLine(
					(int)(Math.log((1-.1*i)*xVariable/xmin)/Math.log(xmax/xmin)*chartSize.width),
					(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin)),
					(int)(Math.log((1-.1*i)*xVariable/xmin)/Math.log(xmax/xmin)*chartSize.width),
					(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin))+majorTickLength);
			}
			else if (((i==5||i==8)&&digit==0)) {
				g.drawString(
					format(xVariable*(1-.1*i)),
					(int)(Math.log(xVariable*(1-.1*i)/xmin)/Math.log(xmax/xmin)*chartSize.width-fm.stringWidth(format(xVariable*(1-.1*i)))/2),
					(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin)) + fm.getHeight() + textMargin.top);

				g.drawLine(
					(int)(Math.log((1-.1*i)*xVariable/xmin)/Math.log(xmax/xmin)*chartSize.width),
					(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin)),
					(int)(Math.log((1-.1*i)*xVariable/xmin)/Math.log(xmax/xmin)*chartSize.width),
					(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin))+majorTickLength);
			}
			else {
				g.drawLine(
					(int)(Math.log((1-.1*i)*xminDraw/xmin)/Math.log(xmax/xmin)*chartSize.width),
					(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin)),
					(int)(Math.log((1-.1*i)*xminDraw/xmin)/Math.log(xmax/xmin)*chartSize.width),
					(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin))+minorTickLength);
			}
		}
	}
	else if (thick==0) {
		for (i=3; xVariable*(1-.1*i)>xmin; i+=3){
			g.drawLine(
				(int)(Math.log((1-.1*i)*xVariable/xmin)/Math.log(xmax/xmin)*chartSize.width),
				(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin)),
				(int)(Math.log((1-.1*i)*xVariable/xmin)/Math.log(xmax/xmin)*chartSize.width),
				(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin))+minorTickLength);
		}
	}

//	draw thicks and digits between xminDraw and xmaxDraw
	if (thick==-1) {
		for (i=0;xVariable<xmaxDraw;i++) {

			if (xVariable<=1) {
				formatDraw=FormatStringMaker.getFormatString(xVariable);
				setFormat(formatDraw);
			}

			for (j=1; j<=9; j++) {
				
				if ((j==1&&(digit<=0||i%digit==0))||((j==2||j==5)&&digit<=0)||((j==3||j==7)&&digit==-1)) {
					g.drawString(
						format(j*xVariable),
						(int)(Math.log(j*xVariable/xmin)/Math.log(xmax/xmin)*chartSize.width-fm.stringWidth(format(xVariable))/2),
						(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin)) + fm.getHeight() + textMargin.top);
				
					g.drawLine(
						(int)(Math.log(j*xVariable/xmin)/Math.log(xmax/xmin)*chartSize.width),
						(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin)),
						(int)(Math.log(j*xVariable/xmin)/Math.log(xmax/xmin)*chartSize.width),
						(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin))+majorTickLength);
				}
				else if (j==1) {
					g.drawLine(
						(int)(Math.log(j*xVariable/xmin)/Math.log(xmax/xmin)*chartSize.width),
						(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin)),
						(int)(Math.log(j*xVariable/xmin)/Math.log(xmax/xmin)*chartSize.width),
						(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin))+majorTickLength);
				}
				else {
					g.drawLine(
						(int)(Math.log(j*xVariable/xmin)/Math.log(xmax/xmin)*chartSize.width),
						(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin)),
						(int)(Math.log(j*xVariable/xmin)/Math.log(xmax/xmin)*chartSize.width),
						(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin))+minorTickLength);
				}
			}
			
			xVariable*=10;
		}
	}
	else if (thick==0) {
		for (i=0;xVariable<xmaxDraw;i++) {

			if (xVariable<=1) {
				formatDraw=FormatStringMaker.getFormatString(xVariable);
				setFormat(formatDraw);
			}

			if (i%digit==0) {
				g.drawString(
					format(xVariable),
					(int)(Math.log(xVariable/xmin)/Math.log(xmax/xmin)*chartSize.width-fm.stringWidth(format(xVariable))/2),
					(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin)) + fm.getHeight() + textMargin.top);
			}
				
			g.drawLine(
				(int)(Math.log(xVariable/xmin)/Math.log(xmax/xmin)*chartSize.width),
				(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin)),
				(int)(Math.log(xVariable/xmin)/Math.log(xmax/xmin)*chartSize.width),
				(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin))+majorTickLength);
			
			for (j=4; j<=9; j+=3) {
				g.drawLine(
					(int)(Math.log(j*xVariable/xmin)/Math.log(xmax/xmin)*chartSize.width),
					(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin)),
					(int)(Math.log(j*xVariable/xmin)/Math.log(xmax/xmin)*chartSize.width),
					(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin))+minorTickLength);
			}
			xVariable*=10;
		}
	}
	else {
		for (i=0;xVariable<xmaxDraw;i++) {

			if (xVariable<=1) {
				formatDraw=FormatStringMaker.getFormatString(xVariable);
				setFormat(formatDraw);
			}

			if (i%digit==0) {
				g.drawString(
					format(xVariable),
					(int)(Math.log(xVariable/xmin)/Math.log(xmax/xmin)*chartSize.width-fm.stringWidth(format(xVariable))/2),
					(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin)) + fm.getHeight() + textMargin.top);
			}
			
			if (i%thick==0) {	
				g.drawLine(
					(int)(Math.log(xVariable/xmin)/Math.log(xmax/xmin)*chartSize.width),
					(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin)),
					(int)(Math.log(xVariable/xmin)/Math.log(xmax/xmin)*chartSize.width),
					(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin))+majorTickLength);			
			}
			xVariable*=10;
		}
	}

//	draw digits and thicks after xmaxDraw

	if (xVariable<=1) {
		formatDraw=FormatStringMaker.getFormatString(xVariable);
		setFormat(formatDraw);
	}

	if (thick==-1) {
		for (j=1; j*xVariable<=xmax; j++) {
				
			if (((j==1||j==10)&&(digit<=0||i%digit==0))||((j==2||j==5)&&digit<=0||((j==3||j==7)&&digit==-1))) {
				g.drawString(
					format(j*xVariable),
					(int)(Math.log(j*xVariable/xmin)/Math.log(xmax/xmin)*chartSize.width-fm.stringWidth(format(xVariable))/2),
					(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin)) + fm.getHeight() + textMargin.top);
				
				g.drawLine(
					(int)(Math.log(j*xVariable/xmin)/Math.log(xmax/xmin)*chartSize.width),
					(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin)),
					(int)(Math.log(j*xVariable/xmin)/Math.log(xmax/xmin)*chartSize.width),
					(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin))+majorTickLength);
			}
			else if (j==1||j==10) {
				g.drawLine(
					(int)(Math.log(j*xVariable/xmin)/Math.log(xmax/xmin)*chartSize.width),
					(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin)),
					(int)(Math.log(j*xVariable/xmin)/Math.log(xmax/xmin)*chartSize.width),
					(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin))+majorTickLength);
			}
			else {
				g.drawLine(
					(int)(Math.log(j*xVariable/xmin)/Math.log(xmax/xmin)*chartSize.width),
					(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin)),
					(int)(Math.log(j*xVariable/xmin)/Math.log(xmax/xmin)*chartSize.width),
					(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin))+minorTickLength);
			}
		}
	}
	else if (thick==0) {
	
		if (i%digit==0) {
			g.drawString(
				format(xVariable),
				(int)(Math.log(xVariable/xmin)/Math.log(xmax/xmin)*chartSize.width-fm.stringWidth(format(xVariable))/2),
				(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin)) + fm.getHeight() + textMargin.top);
		}
							
		for (j=1; j*xVariable<=xmax; j+=3) {
			if (j==1||j==10) {
				g.drawLine(
					(int)(Math.log(j*xVariable/xmin)/Math.log(xmax/xmin)*chartSize.width),
					(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin)),
					(int)(Math.log(j*xVariable/xmin)/Math.log(xmax/xmin)*chartSize.width),
					(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin))+majorTickLength);
			}
			else {
				g.drawLine(
					(int)(Math.log(j*xVariable/xmin)/Math.log(xmax/xmin)*chartSize.width),
					(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin)),
					(int)(Math.log(j*xVariable/xmin)/Math.log(xmax/xmin)*chartSize.width),
					(int)Math.rint(chartSize.height*(ymax-yOrigin)/(ymax-ymin))+minorTickLength);
			}
		}
	}
	
	
}
/**
 * Sets if scale is logaritmic or linear.
 */
public void setIsLogaritmic(boolean newIsLogaritmic) {
	isLogaritmic = newIsLogaritmic;
}
}
