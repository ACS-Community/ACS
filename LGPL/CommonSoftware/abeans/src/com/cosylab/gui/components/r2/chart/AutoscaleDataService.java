package com.cosylab.gui.components.r2.chart;

/**
 * Insert the type's description here.
 * Creation date: (11/26/00 17:59:37)
 * @author: 
 */
class AutoscaleDataService implements ChartService, ChartDataService {
	public ChartDataModel dataModel;
	public ChartService chartService;
	public PointTransformerModel pointTransformer;
	public Interval xScale;
	public Interval yScale;
	protected PointCollector points = new PointCollector();
	protected Point point;
	protected int count = 0;
/**
 * PrivateChartService constructor comment.
 */
public AutoscaleDataService(ChartDataModel model, ChartService service, PointTransformerModel transform) {
	chartService= service;
	dataModel= model;
	pointTransformer= transform;

	dataModel.setChartService(this);

	chartService.getChartStyleManager().getStyleForSerie(dataModel);

//	xScale= new Interval(model.getPreferedXScale().min(),model.getPreferedXScale().max());
	xScale= model.getPreferedXScale();
	yScale= new Interval();
	if (model.getPreferedYScale()!=null) {
		yScale.set(model.getPreferedYScale().min(),model.getPreferedYScale().max());
	}
	
	pointTransformer.setXScale(chartService.getViewManager().addUserXScale(xScale));
	pointTransformer.setYScale(chartService.getViewManager().addUserYScale(yScale));
	
	if (dataModel instanceof DataManageable) {
		((DataManageable)dataModel).getDataManager().addDataChartListener(new ChartDataListener() {
			public void dataChanged(DataChange change) {
				updateChart(change);
			}
		});
	}
}
/**
 * getChartDisplayablePointCount method comment.
 */
public int getChartDisplayablePointCount() {
	return chartService.getChartDisplayablePointCount();
}
/**
 * Insert the method's description here.
 * Creation date: (11/26/00 18:00:49)
 * @return si.ijs.anka.databush.utilities.ChartService
 */
public ChartService getChartService() {
	return chartService;
}
/**
 * Insert the method's description here.
 * Creation date: (31.03.2001 00:34:10)
 * @return si.ijs.kgb.chart.ChartStyleManager
 */
public ChartStyleManager getChartStyleManager() {
	return chartService.getChartStyleManager();
}
/**
 * Insert the method's description here.
 * Creation date: (11/26/00 18:00:24)
 * @return si.ijs.anka.databush.utilities.ChartDataModel
 */
public ChartDataModel getDataModel() {
	return dataModel;
}
/**
 * Insert the method's description here.
 * Creation date: (11/26/00 18:12:35)
 * @return si.ijs.anka.databush.utilities.PointTransformerModel
 */
public PointTransformerModel getPointTransformer() {
	return pointTransformer;
}
/**
 * Insert the method's description here.
 * Creation date: (24/12/01 16:19:04)
 * @return com.cosylab.gui.chart.ChartViewManager
 */
public ChartViewManager getViewManager() {
	return chartService.getViewManager();
}
/*
 * This service is asked to prepare itself for transformation. 
 * Operations like autoscaling and filtering could be done at this time.
 * ChartAred only guarantee to call this method before transformation. 
 * Some implementation need this method to be called synchronized with other transformers.
 */
public void prepare() {
	if (dataModel.getPointCount()==0) {
		count=0;
		return;
	}
//	double maxX;
//	double maxY;
	double maxY=0.0;
	double minY=0.0;
	Point tmp;

	count=0;
	
	Point p=point;
	
	PointIterator it= dataModel.getPointIterator();
	if (it.hasNext()) {
		tmp= it.next();
//		maxX=minX=tmp.x();
		maxY=minY=tmp.y;
		if (p!=null) {
			p.x = tmp.x;
			p.y= tmp.y;
		} else {
			point=p= points.newPoint(tmp.x,tmp.y);
		}
		count++;
	} else return;
	while (it.hasNext()) {
		tmp=it.next();
//		if (tmp.x()>maxX) maxX=tmp.x();
//		else if (tmp.x()<minX) minX=tmp.x();
		if (tmp.y>maxY) maxY=tmp.y;
		else if (tmp.y<minY) minY=tmp.y;

		if (p.next!=null) {
			p=p.next;
			p.x = tmp.x;
			p.y= tmp.y;
		} else {
			p.next= points.newPoint(tmp.x,tmp.y);
			p=p.next;
		}
		count++;
	}

//	double lx= maxX-minX;
	double ly= maxY-minY;
	if (ly<Math.pow(10,-7)) ly=Math.pow(10,-7);

	if ((Math.abs(yScale.max()-maxY-ly*0.1)>ly*0.1) || (Math.abs(yScale.min()-minY+ly*0.1)>ly*0.1))
		yScale.set(minY-ly*0.1,maxY+ly*0.1);

	tmp=p.next;
	p.next=null;
	p=tmp;
	
	while (p!=null) {
		tmp=p.next;
		points.recyclePoint(p);
		p=tmp;
	}
}
/**
 * PrivateChartService constructor comment.
 */
public void setChartService(ChartService service) {
	chartService.getViewManager().removeUserXScale(xScale);
	chartService.getViewManager().removeUserYScale(yScale);

	chartService= service;
	dataModel.setChartService(this);
	chartService.getChartStyleManager().getStyleForSerie(dataModel);

	pointTransformer.setXScale(chartService.getViewManager().addUserXScale(xScale));
	pointTransformer.setYScale(chartService.getViewManager().addUserYScale(yScale));
}
/*
 * Performes transformation from data points to chart points.
 * Return code signalize success of transformation.
 * @return boolean valid return codes are
 * <ul>
 *   <li><code>DONE</code> - if transformation was performed</li>
 *   <li><code>FAILED</code> - if transformation failed ofr some reason, e.g. no data available.</li>
 * </ul>
 */
public int transform() {
	if (count==0) return FAILED;
	pointTransformer.transform(new PointIterator(){
		Point p= point;
		public boolean hasNext() {
			return p!=null;
		}
		public Point next() {
			Point tmp= p;
			p= p.next;
			return tmp;
		}
	});
	return DONE;
}
/**
 * updateChart method comment.
 */
public void updateChart(ChartUpdateRequest request) {
	chartService.updateChart(request);
}
/**
 * updateChart method comment.
 */
public void updateChart(DataChange change) {
	switch (change.getCode()) {
		case DataChange.RESET_ALL_CODE : {
			chartService.updateChart(ChartUpdateRequest.RELOAD_ALL); 
		} break;
		case DataChange.ALL_POINTS_CHANGED_CODE : {
			chartService.updateChart(ChartUpdateRequest.UPDATE_ALL); 
		} break;
		default : {
			chartService.updateChart(ChartUpdateRequest.UPDATE_SERIE); 
		} break;
	} 
}
}
