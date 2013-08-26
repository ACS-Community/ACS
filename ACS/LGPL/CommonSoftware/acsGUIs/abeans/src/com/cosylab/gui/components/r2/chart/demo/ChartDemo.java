package com.cosylab.gui.components.r2.chart.demo;

import com.cosylab.gui.components.r2.chart.*;
/**
 * Insert the type's description here.
 * Creation date: (7.8.2002 20:35:01)
 * @author: 
 */
public class ChartDemo extends javax.swing.JPanel {
	private javax.swing.JLabel ivjJLabel1 = null;
	private javax.swing.JScrollPane ivjJScrollPane1 = null;
	private javax.swing.JTextArea ivjTextArea = null;
	private javax.swing.JButton ivjDefaultChartButton = null;
	IvjEventHandler ivjEventHandler = new IvjEventHandler();
	private javax.swing.JButton ivjSimpleTrendButton = null;
	private javax.swing.JButton ivjHistoryTrendButton = null;
	private javax.swing.JButton ivjFancyChartButton = null;

class IvjEventHandler implements java.awt.event.ActionListener, java.awt.event.MouseListener {
		public void actionPerformed(java.awt.event.ActionEvent e) {
			if (e.getSource() == ChartDemo.this.getDefaultChartButton()) 
				connEtoC1(e);
			if (e.getSource() == ChartDemo.this.getSimpleTrendButton()) 
				connEtoC2(e);
			if (e.getSource() == ChartDemo.this.getHistoryTrendButton()) 
				connEtoC3(e);
			if (e.getSource() == ChartDemo.this.getFancyChartButton()) 
				connEtoC4(e);
		};
		public void mouseClicked(java.awt.event.MouseEvent e) {};
		public void mouseEntered(java.awt.event.MouseEvent e) {
			if (e.getSource() == ChartDemo.this.getDefaultChartButton()) 
				connEtoM1(e);
			if (e.getSource() == ChartDemo.this.getSimpleTrendButton()) 
				connEtoM2(e);
			if (e.getSource() == ChartDemo.this.getHistoryTrendButton()) 
				connEtoM3(e);
			if (e.getSource() == ChartDemo.this.getFancyChartButton()) 
				connEtoM4(e);
		};
		public void mouseExited(java.awt.event.MouseEvent e) {};
		public void mousePressed(java.awt.event.MouseEvent e) {};
		public void mouseReleased(java.awt.event.MouseEvent e) {};
	};
/**
 * ChartDemoPanel constructor comment.
 */
public ChartDemo() {
	super();
	initialize();
}
/**
 * ChartDemoPanel constructor comment.
 * @param layout java.awt.LayoutManager
 */
public ChartDemo(java.awt.LayoutManager layout) {
	super(layout);
}
/**
 * ChartDemoPanel constructor comment.
 * @param layout java.awt.LayoutManager
 * @param isDoubleBuffered boolean
 */
public ChartDemo(java.awt.LayoutManager layout, boolean isDoubleBuffered) {
	super(layout, isDoubleBuffered);
}
/**
 * ChartDemoPanel constructor comment.
 * @param isDoubleBuffered boolean
 */
public ChartDemo(boolean isDoubleBuffered) {
	super(isDoubleBuffered);
}
/**
 * connEtoC1:  (DefaultChartButton.action.actionPerformed(java.awt.event.ActionEvent) --> ChartDemo.startDefaultChart()V)
 * @param arg1 java.awt.event.ActionEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC1(java.awt.event.ActionEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		this.startDefaultChart();
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoC2:  (SimpleTrendButton.action.actionPerformed(java.awt.event.ActionEvent) --> ChartDemo.startSimpleTrendChart()V)
 * @param arg1 java.awt.event.ActionEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC2(java.awt.event.ActionEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		this.startSimpleTrendChart();
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoC3:  (HistoryTrendButton.action.actionPerformed(java.awt.event.ActionEvent) --> ChartDemo.startHistoryTrendChart()V)
 * @param arg1 java.awt.event.ActionEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC3(java.awt.event.ActionEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		this.startHistoryTrendChart();
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoC4:  (FancyChartButton.action.actionPerformed(java.awt.event.ActionEvent) --> ChartDemo.startFancyChart()V)
 * @param arg1 java.awt.event.ActionEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC4(java.awt.event.ActionEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		this.startFancyChart();
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoM1:  (DefaultChartButton.mouse.mouseEntered(java.awt.event.MouseEvent) --> TextArea.text)
 * @param arg1 java.awt.event.MouseEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoM1(java.awt.event.MouseEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		getTextArea().setText(this.descDefaultChart());
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoM2:  (SimpleTrendButton.mouse.mouseEntered(java.awt.event.MouseEvent) --> TextArea.text)
 * @param arg1 java.awt.event.MouseEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoM2(java.awt.event.MouseEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		getTextArea().setText(this.descSimpleTrendChart());
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoM3:  (HistoryTrendButton.mouse.mouseEntered(java.awt.event.MouseEvent) --> TextArea.text)
 * @param arg1 java.awt.event.MouseEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoM3(java.awt.event.MouseEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		getTextArea().setText(this.dendTrendChart());
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoM4:  (FancyChartButton.mouse.mouseEntered(java.awt.event.MouseEvent) --> TextArea.text)
 * @param arg1 java.awt.event.MouseEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoM4(java.awt.event.MouseEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		getTextArea().setText(this.descFancyChart());
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * Comment
 */
public java.lang.String dendTrendChart() {
	return "Trend Chart\n\nFancy branch of charts with additional features.";
}
/**
 * Comment
 */
public java.lang.String descDefaultChart() {
	return "Default Chart\n\nChart as comes by default.\nSinusoidal functions are recalculated each update for each point on X axis.";
}
/**
 * Comment
 */
public java.lang.String descFancyChart() {
	return "Fancy Chart\n\nMore sophysticated chart with avaliable fancy features.";
}
/**
 * Comment
 */
public java.lang.String descSimpleTrendChart() {
	return "Simple Trend Chart\n\nMinimalistic trend chart for heavy duty performance charting.";
}
/**
 * Return the JButton1 property value.
 * @return javax.swing.JButton
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JButton getDefaultChartButton() {
	if (ivjDefaultChartButton == null) {
		try {
			ivjDefaultChartButton = new javax.swing.JButton();
			ivjDefaultChartButton.setName("DefaultChartButton");
			ivjDefaultChartButton.setPreferredSize(new java.awt.Dimension(160, 25));
			ivjDefaultChartButton.setText("Default Chart...");
			ivjDefaultChartButton.setMinimumSize(new java.awt.Dimension(160, 25));
			ivjDefaultChartButton.setMaximumSize(new java.awt.Dimension(160, 25));
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjDefaultChartButton;
}
/**
 * Return the FancyChartButton property value.
 * @return javax.swing.JButton
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JButton getFancyChartButton() {
	if (ivjFancyChartButton == null) {
		try {
			ivjFancyChartButton = new javax.swing.JButton();
			ivjFancyChartButton.setName("FancyChartButton");
			ivjFancyChartButton.setPreferredSize(new java.awt.Dimension(160, 25));
			ivjFancyChartButton.setText("Fancy Chart...");
			ivjFancyChartButton.setMinimumSize(new java.awt.Dimension(160, 25));
			ivjFancyChartButton.setMaximumSize(new java.awt.Dimension(160, 25));
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjFancyChartButton;
}
/**
 * Return the JButton3 property value.
 * @return javax.swing.JButton
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JButton getHistoryTrendButton() {
	if (ivjHistoryTrendButton == null) {
		try {
			ivjHistoryTrendButton = new javax.swing.JButton();
			ivjHistoryTrendButton.setName("HistoryTrendButton");
			ivjHistoryTrendButton.setPreferredSize(new java.awt.Dimension(160, 25));
			ivjHistoryTrendButton.setText("Trend Chart...");
			ivjHistoryTrendButton.setMinimumSize(new java.awt.Dimension(160, 25));
			ivjHistoryTrendButton.setMaximumSize(new java.awt.Dimension(160, 25));
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjHistoryTrendButton;
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
			ivjJLabel1.setFont(new java.awt.Font("Times New Roman", 3, 48));
			ivjJLabel1.setText("Chart Demo");
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
 * Return the JScrollPane1 property value.
 * @return javax.swing.JScrollPane
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JScrollPane getJScrollPane1() {
	if (ivjJScrollPane1 == null) {
		try {
			ivjJScrollPane1 = new javax.swing.JScrollPane();
			ivjJScrollPane1.setName("JScrollPane1");
			getJScrollPane1().setViewportView(getTextArea());
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJScrollPane1;
}
/**
 * Return the JButton2 property value.
 * @return javax.swing.JButton
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JButton getSimpleTrendButton() {
	if (ivjSimpleTrendButton == null) {
		try {
			ivjSimpleTrendButton = new javax.swing.JButton();
			ivjSimpleTrendButton.setName("SimpleTrendButton");
			ivjSimpleTrendButton.setPreferredSize(new java.awt.Dimension(160, 25));
			ivjSimpleTrendButton.setText("Simple Trend Chart...");
			ivjSimpleTrendButton.setMinimumSize(new java.awt.Dimension(160, 25));
			ivjSimpleTrendButton.setMaximumSize(new java.awt.Dimension(160, 25));
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjSimpleTrendButton;
}
/**
 * Return the TextArea property value.
 * @return javax.swing.JTextArea
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextArea getTextArea() {
	if (ivjTextArea == null) {
		try {
			ivjTextArea = new javax.swing.JTextArea();
			ivjTextArea.setName("TextArea");
			ivjTextArea.setLineWrap(true);
			ivjTextArea.setText("Interactive Help\n\nMove mouse over buttons for help.\n\nClick on button for new chart.");
			ivjTextArea.setBackground(new java.awt.Color(222,222,255));
			ivjTextArea.setBounds(0, 0, 160, 120);
			ivjTextArea.setMargin(new java.awt.Insets(11, 11, 11, 11));
			ivjTextArea.setEditable(false);
			ivjTextArea.setEnabled(true);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextArea;
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
	getDefaultChartButton().addMouseListener(ivjEventHandler);
	getDefaultChartButton().addActionListener(ivjEventHandler);
	getSimpleTrendButton().addMouseListener(ivjEventHandler);
	getSimpleTrendButton().addActionListener(ivjEventHandler);
	getHistoryTrendButton().addMouseListener(ivjEventHandler);
	getHistoryTrendButton().addActionListener(ivjEventHandler);
	getFancyChartButton().addMouseListener(ivjEventHandler);
	getFancyChartButton().addActionListener(ivjEventHandler);
}
/**
 * Initialize the class.
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void initialize() {
	try {
		// user code begin {1}
		// user code end
		setName("ChartDemo");
		setLayout(new java.awt.GridBagLayout());
		setSize(301, 375);

		java.awt.GridBagConstraints constraintsJLabel1 = new java.awt.GridBagConstraints();
		constraintsJLabel1.gridx = 0; constraintsJLabel1.gridy = 0;
		constraintsJLabel1.insets = new java.awt.Insets(12, 4, 20, 4);
		add(getJLabel1(), constraintsJLabel1);

		java.awt.GridBagConstraints constraintsDefaultChartButton = new java.awt.GridBagConstraints();
		constraintsDefaultChartButton.gridx = 0; constraintsDefaultChartButton.gridy = 1;
		constraintsDefaultChartButton.insets = new java.awt.Insets(4, 4, 4, 4);
		add(getDefaultChartButton(), constraintsDefaultChartButton);

		java.awt.GridBagConstraints constraintsSimpleTrendButton = new java.awt.GridBagConstraints();
		constraintsSimpleTrendButton.gridx = 0; constraintsSimpleTrendButton.gridy = 3;
		constraintsSimpleTrendButton.insets = new java.awt.Insets(4, 4, 4, 4);
		add(getSimpleTrendButton(), constraintsSimpleTrendButton);

		java.awt.GridBagConstraints constraintsHistoryTrendButton = new java.awt.GridBagConstraints();
		constraintsHistoryTrendButton.gridx = 0; constraintsHistoryTrendButton.gridy = 4;
		constraintsHistoryTrendButton.insets = new java.awt.Insets(4, 4, 4, 4);
		add(getHistoryTrendButton(), constraintsHistoryTrendButton);

		java.awt.GridBagConstraints constraintsJScrollPane1 = new java.awt.GridBagConstraints();
		constraintsJScrollPane1.gridx = 0; constraintsJScrollPane1.gridy = 5;
		constraintsJScrollPane1.fill = java.awt.GridBagConstraints.BOTH;
		constraintsJScrollPane1.weightx = 1.0;
		constraintsJScrollPane1.weighty = 1.0;
		constraintsJScrollPane1.insets = new java.awt.Insets(15, 4, 4, 4);
		add(getJScrollPane1(), constraintsJScrollPane1);

		java.awt.GridBagConstraints constraintsFancyChartButton = new java.awt.GridBagConstraints();
		constraintsFancyChartButton.gridx = 0; constraintsFancyChartButton.gridy = 2;
		constraintsFancyChartButton.insets = new java.awt.Insets(4, 4, 4, 4);
		add(getFancyChartButton(), constraintsFancyChartButton);
		initConnections();
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
		javax.swing.JFrame frame = new javax.swing.JFrame();
		ChartDemo aChartDemo;
		aChartDemo = new ChartDemo();
		frame.setTitle("Chart Demo");
		frame.setContentPane(aChartDemo);
		frame.setSize(aChartDemo.getSize());
		frame.addWindowListener(new java.awt.event.WindowAdapter() {
			public void windowClosing(java.awt.event.WindowEvent e) {
				System.exit(0);
			};
		});
		frame.show();
		java.awt.Insets insets = frame.getInsets();
		frame.setSize(frame.getWidth() + insets.left + insets.right, frame.getHeight() + insets.top + insets.bottom);
		frame.setVisible(true);
	} catch (Throwable exception) {
		System.err.println("Exception occurred in main() of javax.swing.JPanel");
		exception.printStackTrace(System.out);
	}
}
/**
 * Comment
 */
public void startDefaultChart() {
	ChartFrame fr= new ChartFrame("Default Chart Frame");
	int s= 1+(int)(Math.random()*9.0);
	com.cosylab.gui.components.r2.chart.demo.FunctionDataModel m;
	for (int i=0; i<s; i++) {
		m=new com.cosylab.gui.components.r2.chart.demo.FunctionDataModel(new SinusFunction(Math.random()*10.0,Math.random()*3.0,0.0,Math.random()*3.14),Math.random()*0.5,Math.random()*10.0);
		m.setChartStyle(new PlainChartStyle(PlainChartStyle.LINE_PLAIN,new java.awt.Color((int)(255.0*Math.random()),(int)(255.0*Math.random()),(int)(255.0*Math.random())),PlainChartStyle.SYMBOL_NONE,0,java.awt.Color.black));
		fr.getBaseChart().getChartArea().addDataModel(m);
	}
	fr.show();
}
/**
 * Comment
 */
public void startFancyChart() {
	ChartFrame fr= new ChartFrame("Fancy Chart Frame");

// init chart
	DefaultChartDecorator dec= new DefaultChartDecorator();
	dec.applyFlavor(fr.getBaseChart(),new FancyChartFlavor());

	int s= 1+(int)(Math.random()*9.0);
	com.cosylab.gui.components.r2.chart.demo.FunctionDataModel m;
	for (int i=0; i<s; i++) {
		m=new com.cosylab.gui.components.r2.chart.demo.FunctionDataModel(new SinusFunction(Math.random()*10.0,Math.random()*3.0,0.0,Math.random()*3.14),Math.random()*0.5,Math.random()*10.0);
		m.setChartStyle(new PlainChartStyle(PlainChartStyle.LINE_PLAIN,new java.awt.Color((int)(255.0*Math.random()),(int)(255.0*Math.random()),(int)(255.0*Math.random())),PlainChartStyle.SYMBOL_NONE,0,java.awt.Color.black));
		fr.getBaseChart().getChartArea().addDataModel(m);
	}
	fr.show();
}
/**
 * Comment
 */
public void startHistoryTrendChart() {
	ChartFrame fr= new ChartFrame("Trend Chart Frame");
// init chart
	DefaultChartDecorator dec= new DefaultChartDecorator();
	dec.applyFlavor(fr.getBaseChart(),new FancyTimeTrendFlavor());

	fr.getBaseChart().getViewManager().setXScale(new Interval(0.0,100.0));
	fr.getBaseChart().getViewManager().setYScale(new Interval(0.0,10.0));
	
	int s= 1+(int)(Math.random()*9.0);
//	int s= 1;
	com.cosylab.gui.components.r2.chart.demo.RandomTrendDataModel m;
	for (int i=0; i<s; i++) {
		m=new com.cosylab.gui.components.r2.chart.demo.RandomTrendDataModel(0.001+3.0*Math.random(),1+(int)(2*Math.random()),0.0,10.0*(Math.random()-0.5),1.0+8.0*Math.random(),0.0);
		m.setValueRange(m.getStartValue()>5.0 ? 10.0-m.getStartValue() : m.getStartValue());
		m.setPositionAdvance(3.0/(double)m.getPointsPerUpdate());
		fr.getBaseChart().getChartArea().addDataModel(m);
	}
	fr.show();
}
/**
 * Comment
 */
public void startSimpleTrendChart() {
	ChartFrame fr= new ChartFrame("Simple Trend Chart Frame");
// init chart
	DefaultChartDecorator dec= new DefaultChartDecorator();
	dec.applyFlavor(fr.getBaseChart(),new SimpleTrendFlavor());

	fr.getBaseChart().getViewManager().setXScale(new Interval(0.0,100.0));
	fr.getBaseChart().getViewManager().setYScale(new Interval(0.0,10.0));
	
	int s= 1+(int)(Math.random()*9.0);
//	int s= 1;
	com.cosylab.gui.components.r2.chart.demo.RandomTrendDataModel m;
	for (int i=0; i<s; i++) {
		m=new com.cosylab.gui.components.r2.chart.demo.RandomTrendDataModel(0.001+3.0*Math.random(),1+(int)(2*Math.random()),0.0,10.0*(Math.random()-0.5),1.0+8.0*Math.random(),0.0);
		m.setValueRange(m.getStartValue()>5.0 ? 10.0-m.getStartValue() : m.getStartValue());
		m.setPositionAdvance(3.0/(double)m.getPointsPerUpdate());
		fr.getBaseChart().getChartArea().addDataModel(m);
	}
	fr.show();
}
}
