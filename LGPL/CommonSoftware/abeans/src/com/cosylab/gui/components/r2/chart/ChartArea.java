package com.cosylab.gui.components.r2.chart;

/**
 * Chart area interface describes container that holds chart models and draws them on chart area.
 * Interface receives chart relevant information through ChartSerivce, which is set to the chart area, when is added
 * to the chart.
 */
public interface ChartArea {
/**
 * Chart user can add chart model to this chart area.
 * @param model <code>si.ijs.anka.databush.utilities.ChartDataModel</code> model tobe added.
 */
public void addDataModel(ChartDataModel model);
/**
 * Thsi chart ares draws contined chart series to provided graphic context.
 * Pixel with coordinate (0,0) is located in top left corner. 
 * @param g <code>java.awt.Graphics</code> graphic context on which chart series to be drawn.
 */
public void drawData(Chartics chartics, ChartUpdateRequest request);
/**
 * Returns rectangle within which chart is drawn. Coordinats x and y are indicate 
 * rectangle location. Rectangle must fit in to reagon determined by size.
 * @return <code>java.awt.Rectangle</code> reagion whitin size this ChartArea draws chart.
 */
public java.awt.Rectangle getChartRectangle();
/**
 * Getter for ChartService.
 * @return si.ijs.anka.databush.utilities.ChartService
 */
public ChartService getChartService();
/**
 * Returns chart model at <code>index</code> position.
 * @return <code>com.cosylab.gui.chart.ChartDataModel</code> model at <code>index</code> position
 * @param index int
 */
public ChartDataModel getDataModel(int index);
/**
 * Number of contained data models
 * @return <code>int</code> number ofcontained data models
 */
int getDataModelCount();
/**
 * Returns array of contained data models.
 * @return com.cosylab.gui.chart.ChartDataModel[]
 */
ChartDataModel[] getDataModels();
/**
 * Returns maximal point on current chart.
 * @return <code>com.cosylab.gui.chart.Point</code> maximal drawn point
 */
public Point getMaxValue();
/**
 * Returns minimal point on current chart.
 * @return <code>com.cosylab.gui.chart.Point</code> minimal drawn point
 */
public Point getMinValue();
/**
 * Getter for sizeset to chartarea by chart.
 * @return <code>java.awt.Dimension</code> size set to chart area by chart
 */
public java.awt.Dimension getSize();
/**
 * Removes chart modelfrom this chart area.
 * @param model si.ijs.anka.databush.utilities.ChartDataModel
 */
public void removeDataModel(ChartDataModel model);
/**
 * Chart sets chart  service to this chart area.
 * @param newChartService <code>con.cosylab.gui.chart.ChartService</code>
 */
public void setChartService(ChartService newChartService);
/**
 * Chart uses this method to set size of avaliable space to  the chart.
 * @param newDrawSize <code>java.awt.Dimension</code>
 */
public void setSize(java.awt.Dimension newDrawSize);
}
