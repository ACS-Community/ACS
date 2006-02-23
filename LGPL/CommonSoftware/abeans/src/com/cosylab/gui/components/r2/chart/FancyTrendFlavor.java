package com.cosylab.gui.components.r2.chart;

/**
 * Insert the type's description here.
 * Creation date: (28.1.2002 12:01:53)
 * @author: 
 */
public class FancyTrendFlavor extends FancyChartFlavor {
/**
 * TrendFlavor constructor comment.
 */
public FancyTrendFlavor() {
	super();
}
/**
 * Returns <code>ChartArea</code> interface.
 * @return si.ijs.anka.databush.utilities.ChartArea
 */
public ChartArea getChartArea() {
	return new HistoryTrendChartArea();
}
/**
 * Returns description text of this flavor.
 * @return java.lang.String
 */
public String getDescription() {
	return "Trend flavor with fancy features.";
}
/**
 * Returns unique name of this flavor. 
 * @return java.lang.String
 */
public String getName() {
	return "FancyTrendFlavor";
}
}
