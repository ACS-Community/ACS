package com.cosylab.gui.components.r2.chart.demo;

import com.cosylab.gui.components.r2.chart.*;
/**
 * Insert the type's description here.
 * Creation date: (7.8.2002 20:37:02)
 * @author: 
 */
public class ChartFrame extends javax.swing.JFrame implements Runnable {
	private javax.swing.JPanel ivjJFrameContentPane = null;
	private BaseChart ivjBaseChart = null;
	IvjEventHandler ivjEventHandler = new IvjEventHandler();
	private javax.swing.JLabel ivjJLabel1 = null;
	private javax.swing.JLabel ivjJLabel2 = null;
	private javax.swing.JPanel ivjJPanel1 = null;
	private javax.swing.JSlider ivjUpdaterSlider = null;
	private boolean alive = true;
	private int timer = 1000;

class IvjEventHandler implements java.awt.event.WindowListener, javax.swing.event.ChangeListener {
		public void stateChanged(javax.swing.event.ChangeEvent e) {
			if (e.getSource() == ChartFrame.this.getUpdaterSlider()) 
				connEtoM1(e);
			if (e.getSource() == ChartFrame.this.getUpdaterSlider()) 
				connEtoC3(e);
		};
		public void windowActivated(java.awt.event.WindowEvent e) {
			if (e.getSource() == ChartFrame.this) 
				connEtoC1(e);
		};
		public void windowClosed(java.awt.event.WindowEvent e) {
			if (e.getSource() == ChartFrame.this) 
				connEtoC2(e);
		};
		public void windowClosing(java.awt.event.WindowEvent e) {};
		public void windowDeactivated(java.awt.event.WindowEvent e) {};
		public void windowDeiconified(java.awt.event.WindowEvent e) {};
		public void windowIconified(java.awt.event.WindowEvent e) {};
		public void windowOpened(java.awt.event.WindowEvent e) {};
	};
/**
 * ChartFrame constructor comment.
 */
public ChartFrame() {
	super();
	initialize();
}
/**
 * ChartFrame constructor comment.
 * @param title java.lang.String
 */
public ChartFrame(String title) {
	super(title);
	initialize();
}
/**
 * Comment
 */
public synchronized void chartFrame_WindowActivated() {
	notify();
}
/**
 * Comment
 */
public void chartFrame_WindowClosed() {
	alive=false;
}
/**
 * connEtoC1:  (ChartFrame.window.windowActivated(java.awt.event.WindowEvent) --> ChartFrame.chartFrame_WindowActivated()V)
 * @param arg1 java.awt.event.WindowEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC1(java.awt.event.WindowEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		this.chartFrame_WindowActivated();
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoC2:  (ChartFrame.window.windowClosed(java.awt.event.WindowEvent) --> ChartFrame.chartFrame_WindowClosed()V)
 * @param arg1 java.awt.event.WindowEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC2(java.awt.event.WindowEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		this.chartFrame_WindowClosed();
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoC3:  (UpdaterSlider.change.stateChanged(javax.swing.event.ChangeEvent) --> ChartFrame.updaterSlider_StateChanged()V)
 * @param arg1 javax.swing.event.ChangeEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC3(javax.swing.event.ChangeEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		this.updaterSlider_StateChanged();
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoM1:  (UpdaterSlider.change.stateChanged(javax.swing.event.ChangeEvent) --> JLabel2.text)
 * @param arg1 javax.swing.event.ChangeEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoM1(javax.swing.event.ChangeEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		getJLabel2().setText(String.valueOf(getUpdaterSlider().getValue()));
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * Return the BaseChart1 property value.
 * @return com.cosylab.gui.chart.BaseChart
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
public com.cosylab.gui.components.r2.chart.BaseChart getBaseChart() {
	if (ivjBaseChart == null) {
		try {
			ivjBaseChart = new com.cosylab.gui.components.r2.chart.BaseChart();
			ivjBaseChart.setName("BaseChart");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjBaseChart;
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
			getJFrameContentPane().add(getJPanel1(), "South");
			getJFrameContentPane().add(getBaseChart(), "Center");
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
 * Return the JLabel1 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel1() {
	if (ivjJLabel1 == null) {
		try {
			ivjJLabel1 = new javax.swing.JLabel();
			ivjJLabel1.setName("JLabel1");
			ivjJLabel1.setText("updates per s");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel1;
}
/**
 * Return the JLabel2 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel2() {
	if (ivjJLabel2 == null) {
		try {
			ivjJLabel2 = new javax.swing.JLabel();
			ivjJLabel2.setName("JLabel2");
			ivjJLabel2.setText("1");
			ivjJLabel2.setMaximumSize(new java.awt.Dimension(21, 14));
			ivjJLabel2.setPreferredSize(new java.awt.Dimension(21, 14));
			ivjJLabel2.setMinimumSize(new java.awt.Dimension(21, 14));
			ivjJLabel2.setHorizontalAlignment(javax.swing.SwingConstants.RIGHT);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel2;
}
/**
 * Return the JPanel1 property value.
 * @return javax.swing.JPanel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JPanel getJPanel1() {
	if (ivjJPanel1 == null) {
		try {
			ivjJPanel1 = new javax.swing.JPanel();
			ivjJPanel1.setName("JPanel1");
			ivjJPanel1.setLayout(new java.awt.GridBagLayout());

			java.awt.GridBagConstraints constraintsUpdaterSlider = new java.awt.GridBagConstraints();
			constraintsUpdaterSlider.gridx = 0; constraintsUpdaterSlider.gridy = 0;
			constraintsUpdaterSlider.fill = java.awt.GridBagConstraints.HORIZONTAL;
			constraintsUpdaterSlider.weightx = 1.0;
			constraintsUpdaterSlider.insets = new java.awt.Insets(11, 11, 11, 11);
			getJPanel1().add(getUpdaterSlider(), constraintsUpdaterSlider);

			java.awt.GridBagConstraints constraintsJLabel1 = new java.awt.GridBagConstraints();
			constraintsJLabel1.gridx = 2; constraintsJLabel1.gridy = 0;
			constraintsJLabel1.insets = new java.awt.Insets(11, 4, 11, 11);
			getJPanel1().add(getJLabel1(), constraintsJLabel1);

			java.awt.GridBagConstraints constraintsJLabel2 = new java.awt.GridBagConstraints();
			constraintsJLabel2.gridx = 1; constraintsJLabel2.gridy = 0;
			constraintsJLabel2.insets = new java.awt.Insets(4, 4, 4, 4);
			getJPanel1().add(getJLabel2(), constraintsJLabel2);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJPanel1;
}
/**
 * Return the UpdaterSlider property value.
 * @return javax.swing.JSlider
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JSlider getUpdaterSlider() {
	if (ivjUpdaterSlider == null) {
		try {
			ivjUpdaterSlider = new javax.swing.JSlider();
			ivjUpdaterSlider.setName("UpdaterSlider");
			ivjUpdaterSlider.setPaintLabels(false);
			ivjUpdaterSlider.setPaintTicks(true);
			ivjUpdaterSlider.setValue(1);
			ivjUpdaterSlider.setMajorTickSpacing(10);
			ivjUpdaterSlider.setSnapToTicks(true);
			ivjUpdaterSlider.setMinimum(0);
			ivjUpdaterSlider.setMinorTickSpacing(1);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjUpdaterSlider;
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
 * Initializes connections
 * @exception java.lang.Exception The exception description.
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void initConnections() throws java.lang.Exception {
	// user code begin {1}
	// user code end
	getUpdaterSlider().addChangeListener(ivjEventHandler);
	this.addWindowListener(ivjEventHandler);
}
/**
 * Initialize the class.
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void initialize() {
	try {
		// user code begin {1}
		// user code end
		setName("ChartFrame");
		setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
		setSize(430, 255);
		setTitle("Chart Frame");
		setContentPane(getJFrameContentPane());
		initConnections();
	} catch (java.lang.Throwable ivjExc) {
		handleException(ivjExc);
	}
	// user code begin {2}
	new Thread(this).start();
	// user code end
}
/**
 * main entrypoint - starts the part when it is run as an application
 * @param args java.lang.String[]
 */
public static void main(java.lang.String[] args) {
	try {
		ChartFrame aChartFrame;
		aChartFrame = new ChartFrame();
		aChartFrame.addWindowListener(new java.awt.event.WindowAdapter() {
			public void windowClosing(java.awt.event.WindowEvent e) {
				System.exit(0);
			};
		});
		aChartFrame.show();
		java.awt.Insets insets = aChartFrame.getInsets();
		aChartFrame.setSize(aChartFrame.getWidth() + insets.left + insets.right, aChartFrame.getHeight() + insets.top + insets.bottom);
		aChartFrame.setVisible(true);
	} catch (Throwable exception) {
		System.err.println("Exception occurred in main() of javax.swing.JFrame");
		exception.printStackTrace(System.out);
	}
}
/**
 * Insert the method's description here.
 * Creation date: (8.8.2002 1:13:03)
 */
public synchronized void run() {
	while (alive) {
		
		if (isVisible()) getBaseChart().updateChart(ChartUpdateRequest.UPDATE_ALL);

//		synchronized (this) {
			try {
				wait(timer);
			} catch (InterruptedException ex) {
				ex.printStackTrace();
			}
//		}
	}
//	System.out.println("Chart Frame closed");
	new Thread() {
		public void run() {
			System.gc();
		}
	}.start();
}
/**
 * Comment
 */
public synchronized void updaterSlider_StateChanged() {
	timer= getUpdaterSlider().getValue()==0 ? 0 : 1000/getUpdaterSlider().getValue();
	notify();
}
}
