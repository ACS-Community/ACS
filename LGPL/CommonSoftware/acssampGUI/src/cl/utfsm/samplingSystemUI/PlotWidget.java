package cl.utfsm.samplingSystemUI;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Date;

import javax.swing.Timer;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.JFreeChart;
import org.jfree.data.time.Millisecond;
import org.jfree.data.time.TimeSeries;
import org.jfree.data.time.TimeSeriesCollection;
import org.jfree.chart.ChartPanel;

import alma.acs.util.UTCUtility;

public class PlotWidget extends SamplingWidget {

	// Data Handling Attributes
	private TimeSeriesCollection dataset = null;
	private TimeSeries xy = null;
	private long samples=0;
	private int position;
	private ArrayList<TimeSeries> dataArray;
	
	// UI Attributes
	private static final long serialVersionUID = 4823621192367385664L;
	private ChartPanel chartPanel = null;
	private JFreeChart chart;

	public PlotWidget() {
		super();
		initialize();
	}
	
	public PlotWidget(int position) {
		super();
		initialize();
		this.position = position;
	}

	/**
	 * This method initializes this
	 * 
	 */
	private void initialize(){
		
		dataArray = new ArrayList<TimeSeries>();
        // Creating the Chart
        
        if( position == 0){
        	xy = new TimeSeries("a", Millisecond.class);
        	dataArray.add(xy);
        	dataset = new TimeSeriesCollection( xy );
        	chart = ChartFactory.createTimeSeriesChart(
        			null,			// Chart Title
        			"Time UTC [s]", 		// Domain axis label
        			"Property Unit",// Range axis label
        			dataset,		// Data
        			true,			// include legend
        			true,			// tooltips
        			false			// urls
            	);
        	chart.setAntiAlias( false );
        	
        	//TODO: Quitar el autoajuste de los ejes, y poder seter un rango fijo, pero en funcion del primer elemento
        	//chart.getXYPlot().getDomainAxis().setAutoRange(false);
    		//chartPanel.getChart().getXYPlot().getDomainAxis().setRange(0, 1000000000);
        	
        	chartPanel = new ChartPanel(chart, true);
        	chartPanel.setMinimumSize(new java.awt.Dimension(500, 400));
        	
        	
        	//Adding Widgets and Composing the UI
        	this.setLayout(new GridBagLayout());
        	GridBagConstraints gbc = new GridBagConstraints();
        	gbc.anchor = GridBagConstraints.NORTH;
        	gbc.ipadx = 10;
        	gbc.weightx = 2;
        	this.add(chartPanel, gbc);
        }
        
        ActionListener rendering = new ActionListener() {
    		public void actionPerformed(ActionEvent evt) {
    			chart.setNotify(true);
    			chart.setNotify(false);
    		}
    	};
    	new Timer(1000/4, rendering).start();
	}
	
	public void initializeNewPosition(int position){
		if( position != 0 ){
			xy = new TimeSeries("b", Millisecond.class);
			dataArray.add(xy);
			dataset.addSeries( xy );
		}
	}

	public void updateValues(long time, double value, int position) {
		samples++;
		dataArray.get(position).add( new Millisecond( new Date(UTCUtility.utcOmgToJava(time) ) ), value);
	}

	public void setValues(String component, String property, int position) {
		dataArray.get(position).setKey(component + "->" + property);
		chart.setNotify(true);
		chart.setNotify(false);
	}

	public void setComponentAvailable(boolean tmp, String reason, int position) {
	}
	
	public void resetSampleCount() {
		//TODO: Erase data from the Chart.
	}
	
	
	

}
