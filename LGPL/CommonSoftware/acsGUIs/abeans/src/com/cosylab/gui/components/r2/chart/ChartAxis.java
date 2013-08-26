package com.cosylab.gui.components.r2.chart;

/**
 * This is interface for classes for drawing axis on charts. One of them is <code>AbstractChartAxis</code>.
 */
public interface ChartAxis {
/**
 * This is method for drawing axis.
 * @param g java.awt.Graphics
 */
void drawAxis(java.awt.Graphics g);
/**
 * This method is to return class that contains dimensions of chart.
 * @return java.awt.Dimension
 */
java.awt.Dimension getChartSize();
/**
 * This method is to return font that is used for texts on chart.
 * @return java.awt.Font
 */
java.awt.Font getFont();
/**
 * This method is to return color of text, used on chart.
 * @return java.awt.Color
 */
java.awt.Color getFontColor();
/**
 * This method is to return Format, which is string that is used to specify format of output.
 * @return java.lang.String
 */
String getFormat();
/**
 * This method is to return color of lines, used on chart.
 * @return java.awt.Color
 */
java.awt.Color getLineColor();
/**
 * This method is to return PreferedHeight, which is place that text needs for not too replate output.
 * @return int
 */
int getPreferedHeight();
/**
 * This method is to return PreferedWidth, which is place that text needs for not too replate output.
 * @return int
 */
int getPreferedWidth();
/**
 * This method is to return <code>ChartViewManager</code>.
 * @see com.cosylab.gui.chart.ChartViewManager
 * @return com.cosylab.gui.chart.ChartViewManager
 */
ChartViewManager getViewManager();
/**
 * This method is to set dimension of chart.
 * @param size java.awt.Dimension
 */
void setChartSize(java.awt.Dimension size);
/**
 * This method is to set font that is used for text on chart.
 * @param font java.awt.Font
 */
void setFont(java.awt.Font font);
/**
 * This method is to set color for text on chart.
 * @param color java.awt.Color
 */
void setFontColor(java.awt.Color color);
/**
 * This method is to set format, which is string that is used to specify format of output.
 * @param format java.lang.String
 */
void setFormat(String format);
/**
 * This method is to set color of lines on chart.
 * @param color java.awt.Color
 */
void setLineColor(java.awt.Color color);
/**
 * This method is to set <code>ChartViewManager</code>.
 * @see com.cosylab.gui.chart.ChartViewManager
 * @param com.cosylab.gui.chart.ChartViewManager
 */
void setViewManager(ChartViewManager view);
}
