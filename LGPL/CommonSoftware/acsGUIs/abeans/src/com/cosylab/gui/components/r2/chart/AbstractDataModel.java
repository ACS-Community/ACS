package com.cosylab.gui.components.r2.chart;

/**
 * This class is data model for <code>BaseChart</code>.
 */
public abstract class AbstractDataModel implements ChartDataModel, PointIterator {
	protected Interval preferedXScale = null;
	protected Interval preferedYScale = null;
	protected int pointCount = 0;
	protected ChartService chartService = null;
	protected Point point = new Point();
	protected ChartStyle chartStyle;
/**
 * FunctionModel constructor comment.
 */
public AbstractDataModel() {
	super();
}
/**
 * Insert the method's description here.
 * Creation date: (11/24/00 18:27:53)
 * @return si.ijs.anka.databush.utilities.ChartService
 */
public ChartService getChartService() {
	return chartService;
}
/**
 * Insert the method's description here.
 * Creation date: (11/27/00 18:17:31)
 * @return int
 */
public ChartStyle getChartStyle() {
	return chartStyle;
}
/**
 * getPoinCount method comment.
 */
public int getPointCount() {
	return pointCount;
}
/**
 * Insert the method's description here.
 * Creation date: (11/24/00 18:12:33)
 * @return si.ijs.anka.databush.utilities.Interval
 */
public Interval getPreferedXScale() {
	return preferedXScale;
}
/**
 * getPreferedYScale method comment.
 */
public Interval getPreferedYScale() {
	return preferedYScale;
}
/**
 * Insert the method's description here.
 * Creation date: (11/24/00 20:58:22)
 */
public void reloadChartData() {
	if (chartService!=null) chartService.updateChart(ChartUpdateRequest.RELOAD_ALL);
}
/**
 * Insert the method's description here.
 * Creation date: (11/24/00 18:27:53)
 * @param newChartService si.ijs.anka.databush.utilities.ChartService
 */
public void setChartService(ChartService newChartService) {
	chartService = newChartService;
}
/**
 * Insert the method's description here.
 * Creation date: (11/27/00 18:17:31)
 * @param newChartStyle int
 */
public void setChartStyle(ChartStyle newChartStyle) {
	chartStyle = newChartStyle;
}
/**
 * Insert the method's description here.
 * Creation date: (11/24/00 18:12:33)
 * @param newPreferedXScale si.ijs.anka.databush.utilities.Interval
 */
public void setPreferedXScale(Interval newPreferedXScale) {
	preferedXScale = newPreferedXScale;
}
/**
 * Insert the method's description here.
 * Creation date: (11/24/00 18:12:33)
 * @param newPreferedXScale si.ijs.anka.databush.utilities.Interval
 */
public void setPreferedYScale(Interval newPreferedYScale) {
	preferedYScale = newPreferedYScale;
}
/**
 * Insert the method's description here.
 * Creation date: (11/24/00 20:58:22)
 */
public void updateChart(ChartUpdateRequest request) {
	if (chartService!=null) chartService.updateChart(request);
}
/**
 * Insert the method's description here.
 * Creation date: (11/24/00 20:58:22)
 */
public void updateChartData() {
	if (chartService!=null) chartService.updateChart(ChartUpdateRequest.UPDATE_ALL);
}
}
