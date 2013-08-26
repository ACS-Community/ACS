package com.cosylab.gui.components.r2.chart;

/**
 * Insert the type's description here.
 * Creation date: (28.1.2002 12:01:53)
 * @author: 
 */
public class SimpleTrendFlavor extends DefaultChartFlavor {
/**
 * TrendFlavor constructor comment.
 */
public SimpleTrendFlavor() {
	super();
}
/**
 * Returns <code>ChartArea</code> interface.
 * @return si.ijs.anka.databush.utilities.ChartArea
 */
public ChartArea getChartArea() {
	return new TrendChartArea();
}
/**
 * Returns description text of this flavor.
 * @return java.lang.String
 */
public String getDescription() {
	return "Simple trend flavor, does not catch draw points.";
}
/**
 * Returns unique name of this flavor. 
 * @return java.lang.String
 */
public String getName() {
	return "SimpleTrendChartFlavor";
}
}
