package com.cosylab.gui.components.r2.chart;

/**
 * Insert the type's description here.
 * Creation date: (11.04.2001 16:56:22)
 * @author: 
 */
public class HistoryTrendPointService extends DefaultDataService {
	public java.util.LinkedList history = new java.util.LinkedList();
	public PointCollector pointCollector = new PointCollector();
	public PointInterceptor interceptor= new PointInterceptor();
	public HistoryIterator historyInterator= new HistoryIterator();
class PointInterceptor implements PointIterator {
	PointIterator it;
	Point last;
	public void intercept(PointIterator iterator) {
		it= iterator;
	}
	public Point next() {
		return addPoint(it.next());
	}
	public boolean hasNext() {
		return it.hasNext();
	}
}
class HistoryIterator implements PointIterator {
	public java.util.Iterator it;
	public void init(java.util.LinkedList h) {
		it= h.iterator();
	}
	public Point next() {
		return (Point)it.next();
	}
	public boolean hasNext() {
		return it.hasNext();
	}
}
	private boolean resetToHistory = false;
/**
 * HistoryTrendPointService constructor comment.
 * @param model si.ijs.kgb.chart.ChartDataModel
 * @param service si.ijs.kgb.chart.ChartService
 * @param transform si.ijs.kgb.chart.PointTransformerModel
 */
public HistoryTrendPointService(ChartDataModel model, ChartService service, PointTransformerModel transform) {
	super(model, service, transform);
}
/**
 * Insert the method's description here.
 * Creation date: (11.04.2001 17:02:31)
 * @param point si.ijs.kgb.chart.Point
 */
public Point addPoint(Point point) {
	Point p= pointCollector.newPoint();
	p.x=point.x;
	p.y=point.y;
	history.addLast(p);
	return point;
}
/**
 * Insert the method's description here.
 * Creation date: (11.04.2001 16:57:56)
 * @return java.util.LinkedList
 */
public java.util.LinkedList getHistory() {
	return history;
}
/**
 * Insert the method's description here.
 * Creation date: (11.04.2001 17:02:03)
 * @return si.ijs.kgb.chart.PointCollector
 */
public PointCollector getPointCollector() {
	return pointCollector;
}
/**
 * Insert the method's description here.
 * Creation date: (12/10/01 8:25:57)
 * @return boolean
 */
public boolean isResetToHistory() {
	return resetToHistory;
}
/**
 * Insert the method's description here.
 * Creation date: (11.04.2001 17:12:29)
 * @param period double
 */
public void packHistory(double period) {
	double d= ((Point)history.getLast()).x-period;
	while (d>((Point)history.getFirst()).x) 
		history.removeFirst();
}
/**
 * Insert the method's description here.
 * Creation date: (12/10/01 8:25:57)
 * @param newResetToHistory boolean
 */
public void resetToHistory() {
	resetToHistory = true;
}
public int transform() {
	if (resetToHistory) {
		if (ChartProperties.getChartProperties().isDebugData()) {
			System.out.println("ResetToHistory");
		}
		resetToHistory=false;
		if (history.size()==0) return FAILED;
		historyInterator.init(history);
		((TrendPointTransformer)pointTransformer).clearLast();
		pointTransformer.transform(historyInterator);
	} else {
		if (dataModel.getPointCount()==0) return FAILED;
		interceptor.intercept(dataModel.getPointIterator());
		pointTransformer.transform(interceptor);
		packHistory(pointTransformer.getXScale().getLength());
	}
	return DONE;
}
}
