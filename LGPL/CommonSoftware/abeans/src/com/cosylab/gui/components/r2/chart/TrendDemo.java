package com.cosylab.gui.components.r2.chart;

/**
 * This is chart demo that present sine function.
 */
public class TrendDemo extends javax.swing.JFrame {
	private BaseChart ivjChart = null;
	private javax.swing.JPanel ivjJFrameContentPane = null;
	private TrendChartArea trend;
/**
 * TrendDemo constructor.
 */
public TrendDemo() {
	super();
	initialize();
}
/**
 * TrendDemo constructor that takes title as an argument.
 * @param title java.lang.String
 */
public TrendDemo(String title) {
	super(title);
}
/**
 * Return the Chart property value.
 * @return si.ijs.kgb.chart.BaseChart
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private BaseChart getChart() {
	if (ivjChart == null) {
		try {
			ivjChart = new com.cosylab.gui.components.r2.chart.BaseChart();
			ivjChart.setName("Chart");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjChart;
}
/**
 * Return the JFrameContentPane property value.
 * @return javax.swing.JPanel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JPanel getJFrameContentPane() {
	if (ivjJFrameContentPane == null) {
		try {
			ivjJFrameContentPane = new javax.swing.JPanel();
			ivjJFrameContentPane.setName("JFrameContentPane");
			ivjJFrameContentPane.setLayout(new java.awt.BorderLayout());
			getJFrameContentPane().add(getChart(), "Center");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJFrameContentPane;
}
/**
 * Called whenever the part throws an exception.
 * @param exception java.lang.Throwable
 */
private void handleException(java.lang.Throwable exception) {

	/* Uncomment the following lines to print uncaught exceptions to stdout */
	System.out.println("--------- UNCAUGHT EXCEPTION ---------");
	exception.printStackTrace(System.out);
}
/**
 * Initialize the class.
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void initialize() {
	try {
		// user code begin {1}
		getChart().setChartArea(trend= new HistoryTrendChartArea());
		getChart().getViewManager().setXScale(new Interval(0.0,100.0));
		getChart().getViewManager().setYScale(new Interval(-1.0,3.0));
		SinusTrendModel s;
		trend.addDataModel(s=new SinusTrendModel());
		s.startAutomaticUpdate();
		// user code end
		setName("TrendDemo");
		setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
		setSize(319, 167);
		setContentPane(getJFrameContentPane());
	} catch (java.lang.Throwable ivjExc) {
		handleException(ivjExc);
	}
	// user code begin {2}
	// user code end
}
/**
 * main entrypoint - starts the part when it is run as an application
 * @param args java.lang.String[]
 */
public static void main(java.lang.String[] args) {
	try {
//		ChartProperties.getChartProperties().setProperty(ChartProperties.DEBUG_PROPERTY,"true");
		TrendDemo aTrendDemo;
		aTrendDemo = new TrendDemo();
		aTrendDemo.addWindowListener(new java.awt.event.WindowAdapter() {
			public void windowClosing(java.awt.event.WindowEvent e) {
				System.exit(0);
			};
		});
		aTrendDemo.show();
		java.awt.Insets insets = aTrendDemo.getInsets();
		aTrendDemo.setSize(aTrendDemo.getWidth() + insets.left + insets.right, aTrendDemo.getHeight() + insets.top + insets.bottom);
		aTrendDemo.setVisible(true);
	} catch (Throwable exception) {
		System.err.println("Exception occurred in main() of javax.swing.JFrame");
		exception.printStackTrace(System.out);
	}
}
}
