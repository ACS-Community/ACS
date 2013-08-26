package com.cosylab.gui.components.r2.chart;

/**
 * This class is abstract class that is used for drawing axis.
 * For more information:
 * @see com.cosylab.gui.chart.ChartAxis
 */
public abstract class AbstractChartAxis implements ChartAxis {
	protected int preferedHeight=10;
	protected int preferedWidth= 10;
	protected java.awt.Color lineColor;
	protected java.awt.Font font;
	protected java.awt.Color fontColor;
	protected java.awt.Dimension chartSize;
	protected Interval xScale= new Interval(0.0,1.0);
	protected Interval yScale= new Interval(0.0,1.0);
	protected java.lang.String format;
	protected java.text.DecimalFormat decFormat = new java.text.DecimalFormat();
	protected int minorTickLength = 3;
	protected int majorTickLength = 5;
	protected ChartViewManager viewManager;
	protected java.awt.Insets textMargin= new java.awt.Insets(5,7,5,7);
/**
 * <code>AbstractChartAxis</code> default constructor.
 */
public AbstractChartAxis() {
	super();
	setFont(null);
	setFontColor(null);
	setLineColor(null);
	setFormat(null);
}
/**
 * This method is from interface <code>com.cosylab.gui.chart.ChartAxis</code>
 * @see com.cosylab.gui.chart.ChartAxis.format()
 */
public String format(double number) {
	return decFormat.format(number);
}
/**
 * This method is from interface <code>com.cosylab.gui.chart.ChartAxis</code>
 * @see com.cosylab.gui.chart.ChartAxis.getChartSize()
 */
public java.awt.Dimension getChartSize() {
	return chartSize;
}
/**
 * This method is from interface <code>com.cosylab.gui.chart.ChartAxis</code>
 * @see com.cosylab.gui.chart.ChartAxis.getFont()
 */
public java.awt.Font getFont() {
	return font;
}
/**
 * This method is from interface <code>com.cosylab.gui.chart.ChartAxis</code>
 * @see com.cosylab.gui.chart.ChartAxis.getFontColor()
 */
public java.awt.Color getFontColor() {
	return fontColor;
}
/**
 * This method is from interface <code>com.cosylab.gui.chart.ChartAxis</code>
 * @see com.cosylab.gui.chart.ChartAxis.getFormat()
 */
public java.lang.String getFormat() {
	return format;
}
/**
 * This method is from interface <code>com.cosylab.gui.chart.ChartAxis</code>
 * @see com.cosylab.gui.chart.ChartAxis.getLineColor()
 */
public java.awt.Color getLineColor() {
	return lineColor;
}
/**
 * This method return length of major thick (often used for first and last signature on axis).
 */
public int getMajorTickLength() {
	return majorTickLength;
}
/**
 * This method return length of minor thick (often used for all signatures on axis, except first and last).
 */
public int getMinorTickLength() {
	return minorTickLength;
}
/**
 * This method is from interface <code>com.cosylab.gui.chart.ChartAxis</code>
 * @see com.cosylab.gui.chart.ChartAxis.getPreferedHeight()
 */
public int getPreferedHeight() {
	return preferedHeight;
}
/**
 * This method is from interface <code>com.cosylab.gui.chart.ChartAxis</code>
 * @see com.cosylab.gui.chart.ChartAxis.getPreferedWidth()
 */
public int getPreferedWidth() {
	return preferedWidth;
}
/**
 * This method return margins.
 */
public java.awt.Insets getTextMargin() {
	return textMargin;
}
/**
 * This method is from interface <code>com.cosylab.gui.chart.ChartAxis</code>
 * @see com.cosylab.gui.chart.ChartAxis.getViewManager()
 */
public ChartViewManager getViewManager() {
	return viewManager;
}
/**
 * This method is from interface <code>com.cosylab.gui.chart.ChartAxis</code>
 * @see com.cosylab.gui.chart.ChartAxis.setChartSize(java.awt.Dimension newChartSize)
 */
public void setChartSize(java.awt.Dimension newChartSize) {
	chartSize = newChartSize;
}
/**
 * This method is from interface <code>com.cosylab.gui.chart.ChartAxis</code>
 * @see com.cosylab.gui.chart.ChartAxis.setFont(java.awt.Font newFont)
 */
public void setFont(java.awt.Font newFont) {
	font = newFont;
	if (font==null) {
		font= new java.awt.Font("SansSerif",java.awt.Font.PLAIN,11);
	}
}
/**
 * This method is from interface <code>com.cosylab.gui.chart.ChartAxis</code>
 * @see com.cosylab.gui.chart.ChartAxis.setFontColor(java.awt.Color newFontColor)
 */
public void setFontColor(java.awt.Color newFontColor) {
	fontColor = newFontColor;
	if (fontColor==null) fontColor= java.awt.Color.black;
}
/**
 * This method is from interface <code>com.cosylab.gui.chart.ChartAxis</code>
 * @see com.cosylab.gui.chart.ChartAxis.setFormat(java.lang.String newFormat)
 */
public void setFormat(java.lang.String newFormat) {
	format = newFormat;
	if (format==null) format="0.00";
	decFormat.applyPattern(format);
}
/**
 * This method is from interface <code>com.cosylab.gui.chart.ChartAxis</code>
 * @see com.cosylab.gui.chart.ChartAxis.setLineColor(java.awt.Color newLineColor)
 */
public void setLineColor(java.awt.Color newLineColor) {
	lineColor = newLineColor;
	if (lineColor==null) lineColor= java.awt.Color.black;
}
/**
 * This method sets major thick length.
 * @see #getMajorThickLength()
 */
public void setMajorTickLength(int newMajorTickLength) {
	majorTickLength = newMajorTickLength;
}
/**
 * This method sets minor thick length.
 * @see #getMinorThickLength()
 */
public void setMinorTickLength(int newMinorTickLength) {
	minorTickLength = newMinorTickLength;
}
/**
 * This method sets prefered height.
 * @see com.cosylab.gui.chart.ChartAxis.GetPreferedHeight()
 */
public void setPreferedHeight(int newPreferedHeight) {
	preferedHeight = newPreferedHeight;
}
/**
 * This method sets prefered width.
 * @see com.cosylab.gui.chart.ChartAxis.GetPreferedWidth()
 */
public void setPreferedWidth(int newPreferedWidth) {
	preferedWidth = newPreferedWidth;
}
/**
 * This method sets margins.
 */
public void setTextMargin(java.awt.Insets newTextMargin) {
	textMargin = newTextMargin;
}
/**
 * This method is from interface <code>com.cosylab.gui.chart.ChartAxis</code>
 * @see com.cosylab.gui.chart.ChartAxis.setViewManager(ChartViewManager newViewManager)
 */
public void setViewManager(ChartViewManager newViewManager) {
	xScale= (Interval)newViewManager.extractedXScaleIterator().next();
	yScale= (Interval)newViewManager.extractedYScaleIterator().next();
	viewManager = newViewManager;
}
}
