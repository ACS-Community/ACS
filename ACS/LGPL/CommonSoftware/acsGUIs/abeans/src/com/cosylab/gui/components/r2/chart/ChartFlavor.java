package com.cosylab.gui.components.r2.chart;

/**0*********1*********2*********3*********4*********5*********6*********7**
 * <CODE>ChartDecoration</CODE> is interface which should implement classes,
 * which want to hold complete set of interfaces used by BaseChart.
 * Following interfaces are hold for <CODE>BaseChart</CODE>:
 * <DL>
 * 	<DT><CODE>Chartics</CODE><DD> a graphics wrapper
 *	<DT><CODE>ChartArea</CODE><DD> chart image producer and data holder
 *	<DT><CODE>ChartXAxis</CODE><DD> draws X axis
 *	<DT><CODE>ChartYAxis</CODE><DD> draws Y axis
 *	<DT><CODE>ChartViewManager</CODE><DD> handles chart scale and zooming
 * </DL>
 *
 * <CODE>ChartDecoration</CODE> objects are handled by 
 * <CODE>ChartDecoratir</CODE> interface inplementators.
 * 
 * @see com.cosylab.gui.chart.Chartics
 * @see com.cosylab.gui.chart.ChartArea
 * @see com.cosylab.gui.chart.ChartXAxis
 * @see com.cosylab.gui.chart.ChartYAxis
 * @see com.cosylab.gui.chart.ChartViewManager
 *
 */
public interface ChartFlavor extends java.io.Serializable {
/**
 * Returns <code>ChartArea</code> interface.
 * @return si.ijs.anka.databush.utilities.ChartArea
 */
ChartArea getChartArea();
/**
 * Returns ChartStyleManager
 * @return si.ijs.kgb.chart.ChartStyleManager
 */
ChartStyleManager getChartStyleManager();
/**
 * Returns aray of default ChartDataModels for tests and demonstratiron.
 * @return com.cosylab.gui.chart.ChartDataModel[]
 */
ChartDataModel[] getDefaultModels();
/**
 * Returns description text of this flavor.
 * @return java.lang.String
 */
String getDescription();
/**
 * Returns unique name of this flavor. 
 * @return java.lang.String
 */
String getName();
/**
 * Returns ChartViewManager
 * @return com.cosylab.gui.chart.ChartViewManager
 */
ChartViewManager getViewManager();
/**
 * Returns ChartXAxis fot this decoration
 * @return si.ijs.kgb.chart.ChartXAxis
 */
ChartXAxis getXAxis();
/**
 * Returns ChartYAxis for this decoration.
 * @return si.ijs.kgb.chart.ChartYAxis
 */
ChartYAxis getYAxis();
}
