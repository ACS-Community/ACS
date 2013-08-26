package com.cosylab.gui.components.r2.chart;

/**
 * Insert the type's description here.
 * Creation date: (11/26/00 17:59:37)
 * @author: 
 */
class DefaultDataService implements ChartService, ChartDataService {
	public ChartDataModel dataModel;
	public ChartService chartService;
	public PointTransformerModel pointTransformer;
/**
 * PrivateChartService constructor comment.
 */
public DefaultDataService(ChartDataModel model, ChartService service, PointTransformerModel transform) {
	chartService= service;
	dataModel= model;
	pointTransformer= transform;

	dataModel.setChartService(this);

	chartService.getChartStyleManager().getStyleForSerie(dataModel);

	pointTransformer.setXScale(chartService.getViewManager().addUserXScale(model.getPreferedXScale()));
	pointTransformer.setYScale(chartService.getViewManager().addUserYScale(model.getPreferedYScale()));
	
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
public void prepare() {}
/**
 * PrivateChartService constructor comment.
 */
public void setChartService(ChartService service) {
	chartService.getViewManager().removeUserXScale(dataModel.getPreferedXScale());
	chartService.getViewManager().removeUserYScale(dataModel.getPreferedYScale());

	chartService= service;
	dataModel.setChartService(this);
	chartService.getChartStyleManager().getStyleForSerie(dataModel);

	pointTransformer.setXScale(chartService.getViewManager().addUserXScale(dataModel.getPreferedXScale()));
	pointTransformer.setYScale(chartService.getViewManager().addUserYScale(dataModel.getPreferedYScale()));
}
/**
 * Insert the method's description here.
 * Creation date: (11/27/00 15:02:04)
 */
public int transform() {
	if (dataModel.getPointCount()==0) return FAILED;
	pointTransformer.transform(dataModel.getPointIterator());
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
