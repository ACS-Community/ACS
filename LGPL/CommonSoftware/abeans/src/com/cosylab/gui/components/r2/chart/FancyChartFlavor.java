package com.cosylab.gui.components.r2.chart;

/**
 * Insert the type's description here.
 * Creation date: (8.8.2002 21:26:32)
 * @author: 
 */
public class FancyChartFlavor extends DefaultChartFlavor {
/**
 * FancyChartFlavor constructor comment.
 */
public FancyChartFlavor() {
	super();
}
/**
 * Returns <code>ChartArea</code> interface.
 * @return si.ijs.anka.databush.utilities.ChartArea
 */
public ChartArea getChartArea() {
	return new FancyChartArea();
}
/**
 * Returns description text of this flavor.
 * @return java.lang.String
 */
public String getDescription() {
	return "Chart flavor with avaliable fancy features.";
}
/**
 * Returns unique name of this flavor. 
 * @return java.lang.String
 */
public String getName() {
	return "FancyChartFlavor";
}
public ChartXAxis getXAxis() {
	return new ImprovedChartXAxis();
}
public ChartYAxis getYAxis() {
	return new ImprovedChartYAxis();
}
}
