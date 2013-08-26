package com.cosylab.gui.components.r2.chart;

/**
 * Insert the type's description here.
 * Creation date: (11/26/00 17:50:44)
 * @author: 
 */
public class DefaultChartArea implements ChartArea {
	protected java.util.ArrayList models = new java.util.ArrayList(1);
	protected ChartService chartService;
	java.util.HashMap model2service = new java.util.HashMap();
	protected java.awt.Rectangle chartRectangle= new java.awt.Rectangle();
	protected java.awt.Dimension size= new java.awt.Dimension(10,10);
/**
 * ChartArea constructor comment.
 */
public DefaultChartArea() {
	super();
}
/**
 * ChartArea constructor comment.
 */
public DefaultChartArea(ChartService service) {
	this();
	setChartService(service);
}
public void addDataModel(ChartDataModel model) {
	ChartDataService c;
	DefaultPointTransformer t=null;
	models.add(c= new DefaultDataService(model,chartService,t=new DefaultPointTransformer()));
	model2service.put(model, c);
	t.setChartRectangle(chartRectangle);
}
/**
 * Insert the method's description here.
 * Creation date: (11/27/00 11:50:10)
 * @param g java.awt.Graphics
 */
public void drawData(Chartics chartics, ChartUpdateRequest request) {
	if (models.size()==0) return;

	//boolean force= request!=ChartUpdateRequest.UPDATE_SERIE;

	java.util.Iterator it= models.iterator();
	ChartDataService s;
	while (it.hasNext()) {
		(s=(ChartDataService)it.next()).prepare();
		if (ChartDataService.DONE==s.transform())
			chartics.drawGraph(s.getPointTransformer().pointIterator(),s.getDataModel().getChartStyle());
	}
}
/**
 * Rectangle bounded with horizontal and vertical scale, inside size.
 * @return java.awt.Dimension
 */
public java.awt.Rectangle getChartRectangle() {
	return chartRectangle;
}
/**
 * Insert the method's description here.
 * Creation date: (11/26/00 18:04:36)
 * @return si.ijs.anka.databush.utilities.ChartService
 */
public ChartService getChartService() {
	return chartService;
}
/**
 * Insert the method's description here.
 * Creation date: (11/27/00 15:34:11)
 * @return si.ijs.anka.databush.utilities.ChartDataModel
 * @param index int
 */
public ChartDataModel getDataModel(int index) {
	return ((ChartDataService)models.get(index)).getDataModel();
}
/**
 * Returns number of contained data models in this chart area.
 * @return int
 */
public int getDataModelCount() {
	return models.size();
}
/**
 * Insert the method's description here.
 * Creation date: (28.1.2002 15:56:59)
 * @return com.cosylab.gui.chart.ChartDataModel[]
 */
public com.cosylab.gui.components.r2.chart.ChartDataModel[] getDataModels() {
	ChartDataModel[] m= new ChartDataModel[models.size()];
	models.toArray(m);
	return m;
}
/**
 * Insert the method's description here.
 * Creation date: (11/27/00 15:29:08)
 * @return si.ijs.anka.databush.utilities.Point
 */
public Point getMaxValue() {
	Point p= new Point(0.0,Double.MIN_VALUE),pp;
	java.util.Iterator it= models.iterator();
	while (it.hasNext()) if ((pp=((ChartDataService)it.next()).getPointTransformer().getMaxValue()).y>p.y) p=pp;
	return p;
}
/**
 * Insert the method's description here.
 * Creation date: (11/27/00 15:29:08)
 * @return si.ijs.anka.databush.utilities.Point
 */
public Point getMinValue() {
	Point p= new Point(0.0,Double.MAX_VALUE),pp;
	java.util.Iterator it= models.iterator();
	while (it.hasNext()) if ((pp=((ChartDataService)it.next()).getPointTransformer().getMinValue()).y<p.y) p=pp;
	return p;
}
/**
 * Outer dimension of chart= ChartRectangle + margins.
 * @return java.awt.Dimension
 */
public java.awt.Dimension getSize() {
	return size;
}
/**
 * Insert the method's description here.
 * Creation date: (11/26/00 17:56:54)
 * @param model si.ijs.anka.databush.utilities.ChartDataModel
 */
public void removeDataModel(ChartDataModel model) {
	models.remove(model2service.get(model));
	model2service.remove(model);
	chartService.getChartStyleManager().returnStyle(model);
	chartService.getViewManager().removeUserXScale(model.getPreferedXScale());
	chartService.getViewManager().removeUserYScale(model.getPreferedYScale());
}
/**
 * Insert the method's description here.
 * Creation date: (11/26/00 18:04:36)
 * @param newChartService si.ijs.anka.databush.utilities.ChartService
 */
public void setChartService(ChartService newChartService) {

	ChartDataService s;

	chartService = newChartService;

	java.util.Iterator it= models.iterator();
	while (it.hasNext()) {
		s= (ChartDataService)it.next();
		s.setChartService(chartService);
	}

}
/**
 * Sets the size of area, which chart can use.
 * @param newDrawSize java.awt.Dimension
 */
protected void setRectangle(java.awt.Rectangle rec) {
	chartRectangle=rec;
	java.util.Iterator it= models.iterator();
	while (it.hasNext()) ((ChartDataService)it.next()).getPointTransformer().setChartRectangle(chartRectangle);
}
/**
 * Sets the size of area, which chart can use.
 * @param newDrawSize java.awt.Dimension
 */
public void setSize(java.awt.Dimension newDrawSize) {
	chartRectangle= new java.awt.Rectangle(newDrawSize);
	size= newDrawSize;
//	System.out.println(size);
//	System.out.println(chartRectangle);

	setRectangle(new java.awt.Rectangle(5,5,size.width-10,size.height-10));
}
}
