package com.cosylab.gui.components.r2.chart;

/**
 * Insert the type's description here.
 * Creation date: (28.1.2002 12:01:53)
 * @author: 
 */
public class FancyTimeTrendFlavor extends FancyTrendFlavor {
/**
 * TrendFlavor constructor comment.
 */
public FancyTimeTrendFlavor() {
	super();
}
/**
 * Returns description text of this flavor.
 * @return java.lang.String
 */
public String getDescription() {
	return "Fancy trend flavor where X axis is time based";
}
/**
 * Returns unique name of this flavor. 
 * @return java.lang.String
 */
public String getName() {
	return "FancyTimeTrendFlavor";
}
/**
 * Returns ChartXAxis fot this decoration
 * @return si.ijs.kgb.chart.ChartXAxis
 */
public ChartXAxis getXAxis() {
	return new ImprovedTimeXAxis();
}
}
