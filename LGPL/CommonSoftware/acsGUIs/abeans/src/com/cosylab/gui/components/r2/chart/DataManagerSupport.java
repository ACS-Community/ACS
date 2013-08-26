package com.cosylab.gui.components.r2.chart;

/**
 * Insert the type's description here.
 * Creation date: (17.12.2001 20:59:15)
 * @author: 
 */
public class DataManagerSupport implements DataManager {

	private java.util.ArrayList list;
/**
 * DataManagerSupport constructor comment.
 */
public DataManagerSupport() {
	super();
}
/**
 * Insert the method's description here.
 * Creation date: (17.12.2001 20:59:15)
 * @param listener com.cosylab.gui.chart.ChartDataListener
 */
public synchronized void addDataChartListener(ChartDataListener listener) {
	if (list==null) list= new java.util.ArrayList(10);
	list.add(listener);
}
/**
 * Insert the method's description here.
 * Creation date: (17.12.2001 21:34:13)
 * @param change com.cosylab.gui.chart.DataChange
 */
public void fireAllPointsChanged() {
	fireDataChanged(DataChange.ALL_POINTS_CHANGED);
}
/**
 * Insert the method's description here.
 * Creation date: (17.12.2001 21:34:13)
 * @param change com.cosylab.gui.chart.DataChange
 */
public void fireDataChanged(DataChange change) {
	if (list==null) return;
	for (int i=0; i<list.size(); i++) 
		try {
			((ChartDataListener)list.get(i)).dataChanged(change);
		} catch (Throwable t) {System.out.println("Error DataChange event dispatching:");t.printStackTrace();}
}
/**
 * Insert the method's description here.
 * Creation date: (17.12.2001 21:34:13)
 * @param change com.cosylab.gui.chart.DataChange
 */
public void firePointsAdded() {
	fireDataChanged(DataChange.POINTS_ADDED);
}
/**
 * Insert the method's description here.
 * Creation date: (17.12.2001 21:34:13)
 * @param change com.cosylab.gui.chart.DataChange
 */
public void firePointsChanged() {
	fireDataChanged(DataChange.POINTS_CHANGED);
}
/**
 * Insert the method's description here.
 * Creation date: (17.12.2001 21:34:13)
 * @param change com.cosylab.gui.chart.DataChange
 */
public void firePointsRemoved() {
	fireDataChanged(DataChange.POINTS_REMOVED);
}
/**
 * Insert the method's description here.
 * Creation date: (17.12.2001 21:34:13)
 * @param change com.cosylab.gui.chart.DataChange
 */
public void fireResetAll() {
	fireDataChanged(DataChange.RESET_ALL);
}
/**
 * Insert the method's description here.
 * Creation date: (17.12.2001 20:59:15)
 * @param listener com.cosylab.gui.chart.ChartDataListener
 */
public synchronized void removeDataChartListener(ChartDataListener listener) {
	if (list!=null) list.remove(listener);
}
}
