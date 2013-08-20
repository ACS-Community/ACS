package com.cosylab.gui.components.r2.chart;

/**
 * Insert the type's description here.
 * Creation date: (11.04.2001 17:21:30)
 * @author: 
 */
public class HistoryTrendChartArea extends TrendChartArea {
	class XListener implements IntervalListener {
			private double length=0.0;
			public void intervalChange(Interval source) {
				if (Math.abs(length-source.length())>0.0000000001) {
					length=source.length();
					java.util.Iterator it= models.iterator();
					while (it.hasNext()) ((HistoryTrendPointService)it.next()).resetToHistory();
				}
			}
		};
	class YListener implements IntervalListener {
			private double length=0.0;
			public void intervalChange(Interval source) {
				if (Math.abs(length-source.length())>0.0000000001) {
					length=source.length();
					java.util.Iterator it= models.iterator();
					while (it.hasNext()) ((HistoryTrendPointService)it.next()).resetToHistory();
				}
			}
		};

	private IntervalListener intervalXListener= new XListener();
	private IntervalListener intervalYListener= new YListener();
/**
 * HistoryTrendChartArea constructor comment.
 */
public HistoryTrendChartArea() {
	super();
}
/**
 * HistoryTrendChartArea constructor comment.
 * @param service si.ijs.kgb.chart.ChartService
 */
public HistoryTrendChartArea(ChartService service) {
	super(service);
}
public void addDataModel(ChartDataModel model) {
	ChartDataService c;
	TrendPointTransformer t=null;
	models.add(c= new HistoryTrendPointService(model,chartService,t=new TrendPointTransformer()));
	model2service.put(model, c);
	t.setChartRectangle(chartRectangle);
}
/**
 * Insert the method's description here.
 * Creation date: (11/27/00 11:49:26)
 * @param newDrawSize java.awt.Dimension
 */
public void setChartService(ChartService service) {
	java.util.Iterator it;
	
	if (chartService!=null) {
		it= chartService.getViewManager().extractedXScaleIterator();
		while (it.hasNext()) ((Interval)it.next()).removeIntervalListener(intervalXListener);
		it= chartService.getViewManager().extractedYScaleIterator();
		while (it.hasNext()) ((Interval)it.next()).removeIntervalListener(intervalYListener);
	}

	super.setChartService(service);

	it= chartService.getViewManager().extractedXScaleIterator();
	while (it.hasNext()) ((Interval)it.next()).addIntervalListener(intervalXListener);
	it= chartService.getViewManager().extractedYScaleIterator();
	while (it.hasNext()) ((Interval)it.next()).addIntervalListener(intervalYListener);
}
/**
 * Insert the method's description here.
 * Creation date: (11/27/00 11:49:26)
 * @param newDrawSize java.awt.Dimension
 */
public void setSize(java.awt.Dimension newDrawSize) {
	super.setSize(newDrawSize);
	java.util.Iterator it= models.iterator();
	while (it.hasNext()) ((HistoryTrendPointService)it.next()).resetToHistory();
}
}
