package com.cosylab.gui.components.r2.chart;

/**
 * Insert the type's description here.
 * Creation date: (8.8.2002 23:59:56)
 * @author: 
 */
public class FancyChartArea extends DefaultChartArea {
/**
 * FancyChartArea constructor comment.
 */
public FancyChartArea() {
	super();
}
/**
 * FancyChartArea constructor comment.
 * @param service com.cosylab.gui.chart.ChartService
 */
public FancyChartArea(ChartService service) {
	super(service);
}
public void addDataModel(ChartDataModel model) {
	ChartDataService c;
	DefaultPointTransformer t=null;
	models.add(c= new AutoscaleDataService(model,chartService,t=new DefaultPointTransformer()));
	model2service.put(model, c);
	t.setChartRectangle(chartRectangle);
}
public void drawData(Chartics chartics, ChartUpdateRequest request) {
	if (models.size()==0) return;

	//boolean force= request!=ChartUpdateRequest.UPDATE_SERIE;

	java.util.Iterator it= models.iterator();
	ChartDataService s;
	while (it.hasNext()) {
		(s=(ChartDataService)it.next()).prepare();
	}

	it= models.iterator();
	while (it.hasNext()) {
		if (ChartDataService.DONE==(s=(ChartDataService)it.next()).transform())
			chartics.drawGraph(s.getPointTransformer().pointIterator(),s.getDataModel().getChartStyle());
	}
}
}
