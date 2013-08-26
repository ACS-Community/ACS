package com.cosylab.gui.components.r2.chart;

/**
 * Insert the type's description here.
 * Creation date: (31/1/01 10:49:13)
 * @author: 
 */
public class TrendChartArea extends DefaultChartArea {
	protected Chartics im = new Chartics();
	protected double lastTimestamp;
/**
 * TrendChartArea constructor comment.
 */
public TrendChartArea() {
	super();
}
/**
 * TrendChartArea constructor comment.
 * @param service si.ijs.kgb.chart.ChartService
 */
public TrendChartArea(ChartService service) {
	super(service);
}
public void addDataModel(ChartDataModel model) {
	ChartDataService c;
	TrendPointTransformer t=null;
	models.add(c= new DefaultDataService(model,chartService,t=new TrendPointTransformer()));
	model2service.put(model, c);
	t.setChartRectangle(chartRectangle);
}
/**
 * Insert the method's description here.
 * Creation date: (11/27/00 11:50:10)
 * @param g java.awt.Graphics
 */
public void drawData(Chartics chartics, ChartUpdateRequest request) {

	if (models.size() == 0)
		return;

	//boolean force= request!=ChartUpdateRequest.UPDATE_SERIE;

	java.util.Iterator it= models.iterator();

	if (im.getImage() == null) {
		im.setImage(
			new java.awt.image.BufferedImage(
				size.width,
				size.height,
				java.awt.image.BufferedImage.TYPE_INT_RGB));
		im.setBackground(chartics.getBackground());
		im.getGraphics().setColor(im.getBackground());
		im.getGraphics().fillRect(0, 0, size.width, size.height);
		im.getGraphics().setColor(chartics.getGraphics().getColor());

//		ChartProperties.getChartProperties().setProperty(ChartProperties.DEBUG_PROPERTY,"true");
		
		im.getGraphics().translate(chartRectangle.x,chartRectangle.y);

		if (ChartProperties.getChartProperties().isDebugGraphics())	{
			im.getGraphics().drawString("New Image",(int)(size.width/2.0)-20,(int)(size.height/2.0));
		}	

/*		double l=xScale.getLength();
		xScale.min= lastTimestamp - l;
		xScale.max= lastTimestamp;
		while (it.hasNext())
			 ((ChartDataService) it.next()).getPointTransformer().setXScale(xScale);
*/
	}

	it= models.iterator();
	ChartDataService s= null;

	int inc= 0;
	int delta= 0;

	while (it.hasNext()) {
		(s= (ChartDataService) it.next()).prepare();
		if (ChartDataService.DONE!=s.transform())
			continue;
		if ((delta=
			s.getPointTransformer().getLastIntPoint().x+1-chartRectangle.width-inc)
			> 0) {
			im.shift(delta-chartRectangle.x,-chartRectangle.y,size.width-chartRectangle.x-delta,size.height+chartRectangle.y,-delta,0);
// !Workaround for 1.4.0, this line should be
//			im.shift(delta-chartRectangle.x,-chartRectangle.y,size.width-chartRectangle.x-delta,size.height,-delta,0);
			im.getGraphics().translate(-delta, 0);
			inc += delta;
		}

		im.drawGraph(
			s.getPointTransformer().pointIterator(),
			s.getDataModel().getChartStyle());

//		im.getGraphics().drawRect(s.getPointTransformer().getLastIntPoint().x,s.getPointTransformer().getLastIntPoint().y,10,10);
		
//		if (lastTimestamp < ((TrendPointTransformer) s.getPointTransformer()).getLastPoint().x)
//			lastTimestamp= ((TrendPointTransformer) s.getPointTransformer()).getLastPoint().x;

	}
//	System.out.println("Inc "+inc);
	if (inc>0) {
		im.getGraphics().translate(inc, 0);
	}


/*	xScale.min=lastTimestamp-xScale.getLength();
	xScale.max=lastTimestamp;
*/

	chartics.getGraphics().drawImage(im.getImage(), 0, 0, null);

/*	chartics.getGraphics().setColor(java.awt.Color.red);
	chartics.getGraphics().drawRect(chartRectangle.x+2,chartRectangle.y,chartRectangle.width-3,chartRectangle.height-2);
	chartics.getGraphics().drawLine(chartRectangle.x,chartRectangle.y,chartRectangle.x+chartRectangle.width,chartRectangle.y+chartRectangle.height);
	chartics.getGraphics().drawLine(chartRectangle.x,chartRectangle.y+chartRectangle.height,chartRectangle.x+chartRectangle.width,chartRectangle.y);
	chartics.getGraphics().setColor(java.awt.Color.black);*/
}
/**
 * Insert the method's description here.
 * Creation date: (11/27/00 11:49:26)
 * @param newDrawSize java.awt.Dimension
 */
public void setSize(java.awt.Dimension newDrawSize) {
	im.setImage(null);
	size= newDrawSize;
	setRectangle(new java.awt.Rectangle(0,5,size.width-5,size.height-10));
}
}
