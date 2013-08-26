package si.ijs.acs.objectexplorer;

import java.awt.Color;
import java.awt.event.InputEvent;
import java.util.List;

import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;

import si.ijs.acs.objectexplorer.engine.Operation;
import si.ijs.acs.objectexplorer.engine.RemoteCall;
import si.ijs.acs.objectexplorer.engine.RemoteResponse;

import com.cosylab.gui.components.r2.DataFormatter;
import com.cosylab.gui.components.r2.SmartPanel;
import com.cosylab.gui.components.r2.SmartTextPane;
import com.cosylab.util.CircularArrayList;
/**
 * Insert the type's description here.
 * Creation date: (11/2/00 8:17:28 PM)
 * @author: Miha Kadunc
 */
public class RemoteResponseWindow extends JFrame implements OperationInvocator, RemoteResponseCallbackListener {

	/**
	 * Creation date: (23.10.2001 22:42:35)
	 * @author: 
	 */
	class OETrendDataModel extends com.cosylab.gui.components.r2.chart.AbstractDataModel {
		private RemoteResponseWindow rrWindow = null;
		private int pos = -1;

		/**
		 * OETrendDataModel constructor comment.
		 */
		public OETrendDataModel(RemoteResponseWindow parent) {
			super();
			rrWindow = parent;
			setPreferedXScale(new com.cosylab.gui.components.r2.chart.Interval(0,1000));
			setPreferedYScale(new com.cosylab.gui.components.r2.chart.Interval(0,1000));
		}
		/**
		 * Insert the method's description here.
		 * Creation date: (23.10.2001 22:42:35)
		 * @return si.ijs.anka.databush.utilities.PointIterator
		 */
		public com.cosylab.gui.components.r2.chart.PointIterator getPointIterator() {
			pos = -1;
			return this;
		}
		/**
		 * Creation date: (25.10.2001 21:00:57)
		 * @return si.ijs.kgb.chart.Interval
		 */
		public com.cosylab.gui.components.r2.chart.Interval getXBounds() {
			double min = getX(0);
			double max = getX(size() - 1);
			double ovr = (max - min) / 100;
			return new com.cosylab.gui.components.r2.chart.Interval(min - ovr, max + ovr);
		}
		/**
		 * Creation date: (25.10.2001 21:00:57)
		 * @return si.ijs.kgb.chart.Interval
		 */
		public com.cosylab.gui.components.r2.chart.Interval getYBounds() {
			com.cosylab.gui.components.r2.chart.Interval ival=rrWindow.getChartYBounds();
			return ival;
		}
		/**
		 * Insert the method's description here.
		 * Creation date: (23.10.2001 22:42:35)
		 * @return boolean
		 */
		public synchronized boolean hasNext() {
			return (pos < size()-1);
		}
		/**
		 * Insert the method's description here.
		 * Creation date: (23.10.2001 22:42:35)
		 * @return si.ijs.anka.databush.utilities.Point
		 */
		public synchronized com.cosylab.gui.components.r2.chart.Point next() {
	        pos++;
	        return (new com.cosylab.gui.components.r2.chart.Point(getX(pos),getY(pos)));
		}
		private double getX(int index){
	        return rrWindow.getChartX(index);
	    }
		private double getY(int index){
 			return rrWindow.getChartY(index);
	    }
		private int size(){
	        return rrWindow.getChartSize();
	    }
		public void updateChartData(){
	        getPreferedXScale().set(getXBounds());
	        getPreferedYScale().set(getYBounds());
	        pointCount=size();
	        super.updateChartData();
	    }
		public void reloadChartData(){
	        getPreferedXScale().set(getXBounds());
	        getPreferedYScale().set(getYBounds());
	        pointCount=size();
	        super.reloadChartData();
	    }
		public String toString(){
	        return " size:"+size()+" x:"+getX(0)+" y:"+getY(0);
		}
	}
	private JPanel ivjJFrameContentPane = null;
	private JPanel ivjJPanel1 = null;
	private JLabel ivjJLabel1 = null;
	private JLabel ivjJLabel2 = null;
	private JScrollPane ivjJScrollPane1 = null;
	private JLabel ivjmessageField = null;
	private SmartTextPane ivjReportArea = null;
	private JLabel ivjJLabel3 = null;
	IvjEventHandler ivjEventHandler = new IvjEventHandler();
	private JButton ivjJButton1 = null;
	private JLabel ivjJLabel4 = null;
	private JTextField ivjJTextField1 = null;
	private JList ivjJList1 = null;
	private JScrollPane ivjJScrollPane2 = null;
	private JSplitPane ivjJSplitPane1 = null;
	private JTabbedPane ivjResultPanel = null;
	private JPanel ivjTextPanel = null;
	private com.cosylab.gui.components.r2.chart.BaseChart ivjTrend = null;
	private JCheckBoxMenuItem ivjJCheckBoxMenuItem1 = null;
	//BEANS
	private ReporterBean reporter = null;
	private RemoteResponse rr = null;
	private NotificationBean notifier = null;
	private StringBuffer minimText = new StringBuffer();
	private int reportLength = -1;
	private int minimTextReportCount = 0;
	private int maxLines = 500;
//CHART
	private java.util.ArrayList numberIndexes = new java.util.ArrayList();
	private List<double[]> chartData = new CircularArrayList<double[]>();
	private int selectedChartValue = -1;
	private int selectedChartXValue = -1;
	private double[] mins=null;
	private double[] maxs=null;
//OTHER    
	private boolean enabled = true;
	private boolean editing = false;
	private boolean destroyed = false;
	private JScrollPane ivjJScrollPane3 = null;
	private JScrollPane ivjJScrollPane4 = null;
	private JSplitPane ivjJSplitPane2 = null;
	private SmartTextPane ivjoperationResultArea = null;
	private JList ivjoperationsList = null;
	private OETrendDataModel model = null;
	private JLabel ivjJLabel5 = null;
	private JLabel ivjJLabel6 = null;
	private JList ivjJList2 = null;
	private JPanel ivjJPanel3 = null;
	private JScrollPane ivjJScrollPane5 = null;
	private SmartPanel ivjOperations = null;
	private SmartPanel ivjTrendPanel = null;
	private boolean disposeOnDestroy=false;
	
	private volatile boolean textOutputTabSelected = false;

	private Style redStyle = null;
	private Style blackStyle = null;

	private Style redStyleOP = null;
	private Style blackStyleOP = null;

class IvjEventHandler implements java.awt.event.ActionListener, java.awt.event.FocusListener, java.awt.event.KeyListener, java.awt.event.MouseListener, java.awt.event.WindowListener, javax.swing.event.ListSelectionListener {
		public void actionPerformed(java.awt.event.ActionEvent e) {
			if (e.getSource() == RemoteResponseWindow.this.getJButton1()) 
				connEtoC1(e);
			if (e.getSource() == RemoteResponseWindow.this.getJTextField1()) 
				connEtoC2(e);
			if (e.getSource() == RemoteResponseWindow.this.getJCheckBoxMenuItem1()) 
				connEtoC7(e);
		};
		public void focusGained(java.awt.event.FocusEvent e) {};
		public void focusLost(java.awt.event.FocusEvent e) {
			if (e.getSource() == RemoteResponseWindow.this.getJTextField1()) 
				connEtoC3(e);
		};
		public void keyPressed(java.awt.event.KeyEvent e) {
			if (e.getSource() == RemoteResponseWindow.this.getJTextField1()) 
				connEtoC4(e);
		};
		public void keyReleased(java.awt.event.KeyEvent e) {};
		public void keyTyped(java.awt.event.KeyEvent e) {};
		public void mouseClicked(java.awt.event.MouseEvent e) {
			if (e.getSource() == RemoteResponseWindow.this.getoperationsList()) 
				connEtoC8(e);
		};
		public void mouseEntered(java.awt.event.MouseEvent e) {};
		public void mouseExited(java.awt.event.MouseEvent e) {};
		public void mousePressed(java.awt.event.MouseEvent e) {};
		public void mouseReleased(java.awt.event.MouseEvent e) {};
		public void valueChanged(javax.swing.event.ListSelectionEvent e) {
			if (e.getSource() == RemoteResponseWindow.this.getJList1()) 
				connPtoP1SetTarget();
			if (e.getSource() == RemoteResponseWindow.this.getJList2()) 
				connPtoP2SetTarget();
		};
		public void windowActivated(java.awt.event.WindowEvent e) {};
		public void windowClosed(java.awt.event.WindowEvent e) {
			setDestroyed(true);
			if (rr!=null) rr.getInvocation().requestDestroy();
		};
		public void windowClosing(java.awt.event.WindowEvent e) {};
		public void windowDeactivated(java.awt.event.WindowEvent e) {};
		public void windowDeiconified(java.awt.event.WindowEvent e) {
			if (e.getSource() == RemoteResponseWindow.this) 
				connEtoC5(e);
		};
		public void windowIconified(java.awt.event.WindowEvent e) {};
		public void windowOpened(java.awt.event.WindowEvent e) {};
	};
/**
 * CallMethodDialog constructor comment.
 */
public RemoteResponseWindow() {
	super();
	initialize();
}
/**
 * CallMethodDialog constructor comment.
 * @param title java.lang.String
 */
public RemoteResponseWindow(RemoteResponse rr,NotificationBean notifier, ReporterBean reporter) {
	super();
	if (rr!=null) this.rr=rr;
	else throw(new NullPointerException("RemoteResponse in RemoteResponseWindow"));
	this.notifier=notifier;
	this.reporter=reporter;
	initialize();
	setTitle("["+rr.getInvocation().getInvocationRequest().getSN()+"] "+rr.getInvocation().getName());
	getTrendPanel().setName("Trend for "+rr.getInvocation().getName());
	getTrendPanel().setShortName("Trend");
	getOperations().setName("Operations for "+rr.getInvocation().getName());
	getOperations().setShortName("Operations");
	getJList1().setModel(new javax.swing.DefaultListModel());
	getJList2().setModel(new javax.swing.DefaultListModel());
	fillTrendList(rr);
}
/**
 * Creation date: (25.10.2001 0:12:55)
 * @param response si.ijs.acs.objectexplorer.engine.RemoteResponse
 */
private void checkChartPointsSize() {
	while (chartData.size()>maxLines){
		chartData.remove(0);			
	}
	
}
/**
 * connEtoC1:  (JButton1.action.actionPerformed(java.awt.event.ActionEvent) --> RemoteResponseWindow.jButton1_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
 * @param arg1 java.awt.event.ActionEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC1(java.awt.event.ActionEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		this.jButton1_ActionPerformed(arg1);
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoC2:  (JTextField1.action.actionPerformed(java.awt.event.ActionEvent) --> RemoteResponseWindow.jTextField1_ActionPerformed()V)
 * @param arg1 java.awt.event.ActionEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC2(java.awt.event.ActionEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		this.jTextField1_ActionPerformed();
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoC3:  (JTextField1.focus.focusLost(java.awt.event.FocusEvent) --> RemoteResponseWindow.jTextField1_FocusLost()V)
 * @param arg1 java.awt.event.FocusEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC3(java.awt.event.FocusEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		this.jTextField1_FocusLost();
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoC4:  (JTextField1.key.keyPressed(java.awt.event.KeyEvent) --> RemoteResponseWindow.jTextField1_FocusGained()V)
 * @param arg1 java.awt.event.KeyEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC4(java.awt.event.KeyEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		this.jTextField1_FocusGained();
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoC5:  (RemoteResponseWindow.window.windowDeiconified(java.awt.event.WindowEvent) --> RemoteResponseWindow.remoteResponseWindow_WindowDeiconified(Ljava.awt.event.WindowEvent;)V)
 * @param arg1 java.awt.event.WindowEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC5(java.awt.event.WindowEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		this.remoteResponseWindow_WindowDeiconified(arg1);
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoC6:  (RemoteResponseWindow.initialize() --> RemoteResponseWindow.remoteResponseWindow_Initialize()V)
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC6() {
	try {
		// user code begin {1}
		// user code end
		this.remoteResponseWindow_Initialize();
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoC7:  (JCheckBoxMenuItem1.action.actionPerformed(java.awt.event.ActionEvent) --> RemoteResponseWindow.jCheckBoxMenuItem1_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
 * @param arg1 java.awt.event.ActionEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC7(java.awt.event.ActionEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		this.jCheckBoxMenuItem1_ActionPerformed(arg1);
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoC8:  (operationsList.mouse.mouseClicked(java.awt.event.MouseEvent) --> RemoteResponseWindow.jList2_MouseClicked(Ljava.awt.event.MouseEvent;)V)
 * @param arg1 java.awt.event.MouseEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC8(java.awt.event.MouseEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		this.jList2_MouseClicked(arg1);
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connPtoP1SetTarget:  (JList1.selectedIndex <--> RemoteResponseWindow.selectedChartValue)
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connPtoP1SetTarget() {
	/* Set the target from the source */
	try {
		this.setSelectedChartValue(getJList1().getSelectedIndex());
		// user code begin {1}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connPtoP2SetTarget:  (JList2.selectedIndex <--> RemoteResponseWindow.selectedChartXValue)
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connPtoP2SetTarget() {
	/* Set the target from the source */
	try {
		this.setSelectedChartXValue(getJList2().getSelectedIndex());
		// user code begin {1}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * Insert the method's description here.
 * Creation date: (2/25/2001 5:21:43 PM)
 */
public void disable() {
  setTitle(getTitle()+" -- Invocation Destroyed");
  getReportArea().append("\nINVOCATION WAS DESTROYED");
  getoperationsList().setEnabled(false);
  setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
  setDestroyed(true);
  if (disposeOnDestroy) dispose();
}
/**
 * Creation date: (23.10.2001 22:33:35)
 * @param rr si.ijs.acs.objectexplorer.engine.RemoteResponse
 */
void fillTrendList(RemoteResponse rr) {
	getTrend().setXAxis(new com.cosylab.gui.components.r2.chart.DefaultTimeXAxis());
	Object[] values = rr.getData();
	String[] names = rr.getDataNames();
	((DefaultListModel) getJList1().getModel()).addElement("[time]");
	((DefaultListModel) getJList2().getModel()).addElement("[time]");
	int length = 0;
	for (int i = 0; i < values.length; i++) {
		//if (values[i] != null && (Number.class).isAssignableFrom(values[i].getClass())) {
		if (values[i] != null && ((Number.class).isAssignableFrom(values[i].getClass()) || 
		   (values[i].getClass().isArray() && (Number.class).isAssignableFrom(values[i].getClass().getComponentType())))) {
	        	String name=names[i];
	        	if (values[i] instanceof Long) name=name+ " / 1000";
			else if (values[i].getClass().isArray()) name=name+"[]";
			((DefaultListModel) getJList1().getModel()).addElement(name);
			((DefaultListModel) getJList2().getModel()).addElement(name);
			numberIndexes.add(new Integer(i));
			if(values[i].getClass().isArray()) {
				java.util.ArrayList numberSubIndexes = new java.util.ArrayList();
				for(int j = 0; j < ((Object [])values[i]).length; j++) {
 					if(((Object [])values[i])[j] != null && ((Number.class).isAssignableFrom(((Object [])values[i])[j].getClass()))) {
	        				name=names[i]+"["+j+"]";
	        				if (((Object [])values[i])[j] instanceof Long) name=name+ " / 1000";
						((DefaultListModel) getJList1().getModel()).addElement(name);
						((DefaultListModel) getJList2().getModel()).addElement(name);
						numberSubIndexes.add(new Integer[]{i,j});
					}
				}
				numberIndexes.add(numberSubIndexes);
				length += numberSubIndexes.size();
			}
		}
	}
	length += numberIndexes.size() + 1;
	mins=new double[length];
	maxs=new double[length];
	for (int i = 0; i < mins.length; i++){
		mins[i]=Double.MAX_VALUE;
		maxs[i]=Double.MIN_VALUE;
	}    
	model = new OETrendDataModel(this);
}
/**
 * Creation date: (23.10.2001 19:58:56)
 * @return boolean
 */
public int getChartSize() {
	return (chartData.size());
}
/**
 * Creation date: (23.10.2001 19:58:56)
 * @return boolean
 */
public double getChartX(int index) {
	if (selectedChartXValue==-1) return 0;
	return ((double[])chartData.get(index))[selectedChartXValue];
}
/**
 * Creation date: (23.10.2001 19:58:56)
 * @return boolean
 */
public double getChartY(int index) {
	if (selectedChartValue==-1) return 0;
	return ((double[])chartData.get(index))[selectedChartValue];
}
/**
 * Creation date: (23.10.2001 19:58:56)
 * @return boolean
 */
public com.cosylab.gui.components.r2.chart.Interval getChartYBounds() {
	return new com.cosylab.gui.components.r2.chart.Interval(mins[selectedChartValue],maxs[selectedChartValue]);
}
/**
 * Return the JButton1 property value.
 * @return javax.swing.JButton
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JButton getJButton1() {
	if (ivjJButton1 == null) {
		try {
			ivjJButton1 = new javax.swing.JButton();
			ivjJButton1.setName("JButton1");
			ivjJButton1.setText("Disable output");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJButton1;
}
/**
 * Return the JCheckBoxMenuItem1 property value.
 * @return javax.swing.JCheckBoxMenuItem
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JCheckBoxMenuItem getJCheckBoxMenuItem1() {
	if (ivjJCheckBoxMenuItem1 == null) {
		try {
			ivjJCheckBoxMenuItem1 = new javax.swing.JCheckBoxMenuItem();
			ivjJCheckBoxMenuItem1.setName("JCheckBoxMenuItem1");
			ivjJCheckBoxMenuItem1.setText("Expand results");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJCheckBoxMenuItem1;
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
			ivjJFrameContentPane.setLayout(new java.awt.CardLayout());
			ivjJFrameContentPane.setBackground(java.awt.SystemColor.control);
			ivjJFrameContentPane.setForeground(java.awt.Color.orange);
			getJFrameContentPane().add(getJPanel1(), getJPanel1().getName());
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
			ivjJLabel1.setText("Message:");
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
			ivjJLabel2.setText("Serial Number:");
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
 * Return the JLabel3 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel3() {
	if (ivjJLabel3 == null) {
		try {
			ivjJLabel3 = new javax.swing.JLabel();
			ivjJLabel3.setName("JLabel3");
			ivjJLabel3.setText("");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel3;
}
/**
 * Return the JLabel4 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel4() {
	if (ivjJLabel4 == null) {
		try {
			ivjJLabel4 = new javax.swing.JLabel();
			ivjJLabel4.setName("JLabel4");
			ivjJLabel4.setText("max. no. of results:");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel4;
}
/**
 * Return the JLabel5 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel5() {
	if (ivjJLabel5 == null) {
		try {
			ivjJLabel5 = new javax.swing.JLabel();
			ivjJLabel5.setName("JLabel5");
			ivjJLabel5.setText("Y values");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel5;
}
/**
 * Return the JLabel6 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel6() {
	if (ivjJLabel6 == null) {
		try {
			ivjJLabel6 = new javax.swing.JLabel();
			ivjJLabel6.setName("JLabel6");
			ivjJLabel6.setText("X values");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel6;
}
/**
 * Return the JList1 property value.
 * @return javax.swing.JList
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JList getJList1() {
	if (ivjJList1 == null) {
		try {
			ivjJList1 = new javax.swing.JList();
			ivjJList1.setName("JList1");
			ivjJList1.setBounds(0, 0, 145, 200);
			ivjJList1.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJList1;
}
/**
 * Return the JList2 property value.
 * @return javax.swing.JList
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JList getJList2() {
	if (ivjJList2 == null) {
		try {
			ivjJList2 = new javax.swing.JList();
			ivjJList2.setName("JList2");
			ivjJList2.setBounds(0, 0, 49, 228);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJList2;
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

			java.awt.GridBagConstraints constraintsJLabel1 = new java.awt.GridBagConstraints();
			constraintsJLabel1.gridx = 0; constraintsJLabel1.gridy = 1;
			constraintsJLabel1.fill = java.awt.GridBagConstraints.BOTH;
			constraintsJLabel1.ipadx = 5;
			constraintsJLabel1.ipady = 5;
			constraintsJLabel1.insets = new java.awt.Insets(0, 15, 0, 0);
			getJPanel1().add(getJLabel1(), constraintsJLabel1);

			java.awt.GridBagConstraints constraintsJLabel2 = new java.awt.GridBagConstraints();
			constraintsJLabel2.gridx = 0; constraintsJLabel2.gridy = 0;
			constraintsJLabel2.insets = new java.awt.Insets(0, 15, 0, 0);
			getJPanel1().add(getJLabel2(), constraintsJLabel2);

			java.awt.GridBagConstraints constraintsmessageField = new java.awt.GridBagConstraints();
			constraintsmessageField.gridx = 1; constraintsmessageField.gridy = 1;
			constraintsmessageField.gridwidth = 2;
			constraintsmessageField.fill = java.awt.GridBagConstraints.BOTH;
			constraintsmessageField.weightx = 1.0;
			constraintsmessageField.insets = new java.awt.Insets(0, 4, 0, 8);
			getJPanel1().add(getmessageField(), constraintsmessageField);

			java.awt.GridBagConstraints constraintsJLabel3 = new java.awt.GridBagConstraints();
			constraintsJLabel3.gridx = 1; constraintsJLabel3.gridy = 0;
			constraintsJLabel3.fill = java.awt.GridBagConstraints.BOTH;
			constraintsJLabel3.insets = new java.awt.Insets(0, 5, 0, 4);
			getJPanel1().add(getJLabel3(), constraintsJLabel3);

			java.awt.GridBagConstraints constraintsJButton1 = new java.awt.GridBagConstraints();
			constraintsJButton1.gridx = 3; constraintsJButton1.gridy = 1;
			constraintsJButton1.insets = new java.awt.Insets(4, 4, 4, 4);
			getJPanel1().add(getJButton1(), constraintsJButton1);

			java.awt.GridBagConstraints constraintsJTextField1 = new java.awt.GridBagConstraints();
			constraintsJTextField1.gridx = 3; constraintsJTextField1.gridy = 0;
			constraintsJTextField1.fill = java.awt.GridBagConstraints.HORIZONTAL;
			constraintsJTextField1.insets = new java.awt.Insets(4, 4, 4, 4);
			getJPanel1().add(getJTextField1(), constraintsJTextField1);

			java.awt.GridBagConstraints constraintsJLabel4 = new java.awt.GridBagConstraints();
			constraintsJLabel4.gridx = 2; constraintsJLabel4.gridy = 0;
			constraintsJLabel4.anchor = java.awt.GridBagConstraints.EAST;
			constraintsJLabel4.insets = new java.awt.Insets(4, 4, 4, 4);
			getJPanel1().add(getJLabel4(), constraintsJLabel4);

			java.awt.GridBagConstraints constraintsResultPanel = new java.awt.GridBagConstraints();
			constraintsResultPanel.gridx = 0; constraintsResultPanel.gridy = 2;
			constraintsResultPanel.gridwidth = 4;
			constraintsResultPanel.fill = java.awt.GridBagConstraints.BOTH;
			constraintsResultPanel.weightx = 1.0;
			constraintsResultPanel.weighty = 1.0;
			constraintsResultPanel.insets = new java.awt.Insets(4, 4, 4, 4);
			getJPanel1().add(getResultPanel(), constraintsResultPanel);
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
 * Return the JPanel3 property value.
 * @return javax.swing.JPanel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JPanel getJPanel3() {
	if (ivjJPanel3 == null) {
		try {
			ivjJPanel3 = new javax.swing.JPanel();
			ivjJPanel3.setName("JPanel3");
			ivjJPanel3.setPreferredSize(new java.awt.Dimension(200, 490));
			ivjJPanel3.setLayout(new java.awt.GridBagLayout());
			ivjJPanel3.setMinimumSize(new java.awt.Dimension(200, 272));
			ivjJPanel3.setMaximumSize(new java.awt.Dimension(200, 2147483647));

			java.awt.GridBagConstraints constraintsJScrollPane2 = new java.awt.GridBagConstraints();
			constraintsJScrollPane2.gridx = 0; constraintsJScrollPane2.gridy = 2;
			constraintsJScrollPane2.fill = java.awt.GridBagConstraints.BOTH;
			constraintsJScrollPane2.weightx = 1.0;
			constraintsJScrollPane2.weighty = 1.0;
			constraintsJScrollPane2.ipadx = 126;
			constraintsJScrollPane2.ipady = 92;
			getJPanel3().add(getJScrollPane2(), constraintsJScrollPane2);

			java.awt.GridBagConstraints constraintsJScrollPane5 = new java.awt.GridBagConstraints();
			constraintsJScrollPane5.gridx = 0; constraintsJScrollPane5.gridy = 4;
			constraintsJScrollPane5.fill = java.awt.GridBagConstraints.BOTH;
			constraintsJScrollPane5.weightx = 1.0;
			constraintsJScrollPane5.weighty = 1.0;
			constraintsJScrollPane5.ipadx = 126;
			constraintsJScrollPane5.ipady = 92;
			getJPanel3().add(getJScrollPane5(), constraintsJScrollPane5);

			java.awt.GridBagConstraints constraintsJLabel5 = new java.awt.GridBagConstraints();
			constraintsJLabel5.gridx = 0; constraintsJLabel5.gridy = 1;
			constraintsJLabel5.insets = new java.awt.Insets(4, 4, 4, 4);
			getJPanel3().add(getJLabel5(), constraintsJLabel5);

			java.awt.GridBagConstraints constraintsJLabel6 = new java.awt.GridBagConstraints();
			constraintsJLabel6.gridx = 0; constraintsJLabel6.gridy = 3;
			constraintsJLabel6.insets = new java.awt.Insets(4, 4, 4, 4);
			getJPanel3().add(getJLabel6(), constraintsJLabel6);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJPanel3;
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
			getJScrollPane1().setViewportView(getReportArea());
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
 * Return the JScrollPane2 property value.
 * @return javax.swing.JScrollPane
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JScrollPane getJScrollPane2() {
	if (ivjJScrollPane2 == null) {
		try {
			ivjJScrollPane2 = new javax.swing.JScrollPane();
			ivjJScrollPane2.setName("JScrollPane2");
			getJScrollPane2().setViewportView(getJList1());
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJScrollPane2;
}
/**
 * Return the JScrollPane3 property value.
 * @return javax.swing.JScrollPane
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JScrollPane getJScrollPane3() {
	if (ivjJScrollPane3 == null) {
		try {
			ivjJScrollPane3 = new javax.swing.JScrollPane();
			ivjJScrollPane3.setName("JScrollPane3");
			getJScrollPane3().setViewportView(getoperationsList());
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJScrollPane3;
}
/**
 * Return the JScrollPane4 property value.
 * @return javax.swing.JScrollPane
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JScrollPane getJScrollPane4() {
	if (ivjJScrollPane4 == null) {
		try {
			ivjJScrollPane4 = new javax.swing.JScrollPane();
			ivjJScrollPane4.setName("JScrollPane4");
			getJScrollPane4().setViewportView(getoperationResultArea());
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJScrollPane4;
}
/**
 * Return the JScrollPane5 property value.
 * @return javax.swing.JScrollPane
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JScrollPane getJScrollPane5() {
	if (ivjJScrollPane5 == null) {
		try {
			ivjJScrollPane5 = new javax.swing.JScrollPane();
			ivjJScrollPane5.setName("JScrollPane5");
			getJScrollPane5().setViewportView(getJList2());
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJScrollPane5;
}
/**
 * Return the JSplitPane1 property value.
 * @return javax.swing.JSplitPane
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JSplitPane getJSplitPane1() {
	if (ivjJSplitPane1 == null) {
		try {
			ivjJSplitPane1 = new javax.swing.JSplitPane(javax.swing.JSplitPane.HORIZONTAL_SPLIT);
			ivjJSplitPane1.setName("JSplitPane1");
			getJSplitPane1().add(getTrend(), "right");
			getJSplitPane1().add(getJPanel3(), "left");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJSplitPane1;
}
/**
 * Return the JSplitPane2 property value.
 * @return javax.swing.JSplitPane
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JSplitPane getJSplitPane2() {
	if (ivjJSplitPane2 == null) {
		try {
			ivjJSplitPane2 = new javax.swing.JSplitPane(javax.swing.JSplitPane.HORIZONTAL_SPLIT);
			ivjJSplitPane2.setName("JSplitPane2");
			getJSplitPane2().add(getJScrollPane3(), "left");
			getJSplitPane2().add(getJScrollPane4(), "right");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJSplitPane2;
}
/**
 * Return the JTextField1 property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getJTextField1() {
	if (ivjJTextField1 == null) {
		try {
			ivjJTextField1 = new javax.swing.JTextField();
			ivjJTextField1.setName("JTextField1");
			ivjJTextField1.setText("500");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJTextField1;
}
/**
 * Creation date: (25.10.2001 0:01:47)
 * @return int
 */
public int getMaxLines() {
	return maxLines;
}
/**
 * Return the messageField property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getmessageField() {
	if (ivjmessageField == null) {
		try {
			ivjmessageField = new javax.swing.JLabel();
			ivjmessageField.setName("messageField");
			ivjmessageField.setText("");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjmessageField;
}
/**
 * Return the operationResultArea property value.
 * @return com.cosylab.gui.components.SmartTextPane
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private SmartTextPane getoperationResultArea() {
	if (ivjoperationResultArea == null) {
		try {
			ivjoperationResultArea = new SmartTextPane();
			ivjoperationResultArea.setName("operationResultArea");
			ivjoperationResultArea.setBounds(0, 0, 11, 6);
			// user code begin {1}

			// default style 
			blackStyleOP = ivjoperationResultArea.getLogicalStyle();
			
			// Makes text red
		    redStyleOP = ivjoperationResultArea.addStyle("Red", null);
		    StyleConstants.setForeground(redStyleOP, Color.red);

		    // user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjoperationResultArea;
}
/**
 * Return the Operations property value.
 * @return si.ijs.acs.objectexplorer.SmartPanel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private SmartPanel getOperations() {
	if (ivjOperations == null) {
		try {
			ivjOperations = new SmartPanel();
			ivjOperations.setName("Operations");
			ivjOperations.setLayout(getOperationsCardLayout());
			getOperations().add(getJSplitPane2(), getJSplitPane2().getName());
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjOperations;
}
/**
 * Return the OperationsCardLayout property value.
 * @return java.awt.CardLayout
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private java.awt.CardLayout getOperationsCardLayout() {
	java.awt.CardLayout ivjOperationsCardLayout = null;
	try {
		/* Create part */
		ivjOperationsCardLayout = new java.awt.CardLayout();
		ivjOperationsCardLayout.setVgap(4);
		ivjOperationsCardLayout.setHgap(4);
	} catch (java.lang.Throwable ivjExc) {
		handleException(ivjExc);
	};
	return ivjOperationsCardLayout;
}
/**
 * Return the operationsList property value.
 * @return javax.swing.JList
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JList getoperationsList() {
	if (ivjoperationsList == null) {
		try {
			ivjoperationsList = new javax.swing.JList();
			ivjoperationsList.setName("operationsList");
			ivjoperationsList.setBounds(0, 0, 183, 232);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjoperationsList;
}
/**
 * Return the ReportArea property value.
 * @return com.cosylab.gui.components.SmartTextArea
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private SmartTextPane getReportArea() {
	if (ivjReportArea == null) {
		try {
			ivjReportArea = new SmartTextPane();
			ivjReportArea.setName("ReportArea");
			ivjReportArea.setMaxLines(1000);
			ivjReportArea.setLocation(0, 0);
			// user code begin {1}
			// default style 
			blackStyle = ivjReportArea.getLogicalStyle();

			// Makes text red
		    redStyle = ivjReportArea.addStyle("Red", null);
		    StyleConstants.setForeground(redStyle, Color.red);

			//ivjReportArea.setEnabled(false);
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjReportArea;
}
/**
 * Return the ResultPanel property value.
 * @return javax.swing.JTabbedPane
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTabbedPane getResultPanel() {
	if (ivjResultPanel == null) {
		try {
			ivjResultPanel = new javax.swing.JTabbedPane();
			ivjResultPanel.setName("ResultPanel");
			ivjResultPanel.insertTab("Text output", null, getTextPanel(), null, 0);
			ivjResultPanel.insertTab("Trend", null, getTrendPanel(), null, 1);
			ivjResultPanel.insertTab("Operations", null, getOperations(), null, 2);
			// set trend as default
			ivjResultPanel.setSelectedIndex(1);
			// text output switch
			ivjResultPanel.addChangeListener(new ChangeListener() {
		        // This method is called whenever the selected tab changes
		        public void stateChanged(ChangeEvent evt) {
		            JTabbedPane pane = (JTabbedPane)evt.getSource();
		    
		            // Get current tab
		            textOutputTabSelected = (pane.getSelectedIndex() == 0);
		        }
		    });
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjResultPanel;
}
/**
 * Creation date: (25.10.2001 21:59:04)
 * @return int
 */
public int getSelectedChartValue() {
	return selectedChartValue;
}
/**
 * Insert the method's description here.
 * Creation date: (6.2.2002 23:42:48)
 * @return int
 */
public int getSelectedChartXValue() {
	return selectedChartXValue;
}
/**
 * Return the TextPanel property value.
 * @return javax.swing.JPanel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JPanel getTextPanel() {
	if (ivjTextPanel == null) {
		try {
			ivjTextPanel = new javax.swing.JPanel();
			ivjTextPanel.setName("TextPanel");
			ivjTextPanel.setLayout(getTextPanelCardLayout());
			getTextPanel().add(getJScrollPane1(), getJScrollPane1().getName());
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTextPanel;
}
/**
 * Return the TextPanelCardLayout property value.
 * @return java.awt.CardLayout
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private java.awt.CardLayout getTextPanelCardLayout() {
	java.awt.CardLayout ivjTextPanelCardLayout = null;
	try {
		/* Create part */
		ivjTextPanelCardLayout = new java.awt.CardLayout();
		ivjTextPanelCardLayout.setVgap(4);
		ivjTextPanelCardLayout.setHgap(4);
	} catch (java.lang.Throwable ivjExc) {
		handleException(ivjExc);
	};
	return ivjTextPanelCardLayout;
}
/**
 * Return the Trend property value.
 * @return com.cosylab.gui.chart.BaseChart
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private com.cosylab.gui.components.r2.chart.BaseChart getTrend() {
	if (ivjTrend == null) {
		try {
			ivjTrend = new com.cosylab.gui.components.r2.chart.BaseChart();
			ivjTrend.setName("Trend");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTrend;
}
/**
 * Return the TrendPanel property value.
 * @return si.ijs.acs.objectexplorer.SmartPanel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private SmartPanel getTrendPanel() {
	if (ivjTrendPanel == null) {
		try {
			ivjTrendPanel = new SmartPanel();
			ivjTrendPanel.setName("TrendPanel");
			ivjTrendPanel.setToolTipText("Trend");
			ivjTrendPanel.setLayout(getTrendPanelCardLayout());
			getTrendPanel().add(getJSplitPane1(), getJSplitPane1().getName());
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjTrendPanel;
}
/**
 * Return the TrendPanelCardLayout property value.
 * @return java.awt.CardLayout
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private java.awt.CardLayout getTrendPanelCardLayout() {
	java.awt.CardLayout ivjTrendPanelCardLayout = null;
	try {
		/* Create part */
		ivjTrendPanelCardLayout = new java.awt.CardLayout();
		ivjTrendPanelCardLayout.setVgap(4);
		ivjTrendPanelCardLayout.setHgap(4);
	} catch (java.lang.Throwable ivjExc) {
		handleException(ivjExc);
	};
	return ivjTrendPanelCardLayout;
}
/**
 * Called whenever the part throws an exception.
 * @param exception java.lang.Throwable
 */
private void handleException(java.lang.Throwable exception) {

	/* Uncomment the following lines to print uncaught exceptions to stdout */
	 System.out.println("--------- UNCAUGHT EXCEPTION in RemoteResponseWindow ---------");
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
	getJButton1().addActionListener(ivjEventHandler);
	getJTextField1().addActionListener(ivjEventHandler);
	getJTextField1().addFocusListener(ivjEventHandler);
	getJTextField1().addKeyListener(ivjEventHandler);
	this.addWindowListener(ivjEventHandler);
	getJCheckBoxMenuItem1().addActionListener(ivjEventHandler);
	getJList1().addListSelectionListener(ivjEventHandler);
	getJList2().addListSelectionListener(ivjEventHandler);
	getoperationsList().addMouseListener(ivjEventHandler);
	connPtoP1SetTarget();
	connPtoP2SetTarget();
}
/**
 * Initialize the class.
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void initialize() {
	try {
		// user code begin {1}
		// user code end
		setName("RemoteResponseWindow");
		setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE);
		setSize(600, 333);
		setTitle("");
		setContentPane(getJFrameContentPane());
		initConnections();
		connEtoC6();
	} catch (java.lang.Throwable ivjExc) {
		handleException(ivjExc);
	}
	// user code begin {2}
	// user code end
}
/**
 * invocationDestroyed method comment.
 */
public void invocationDestroyed(si.ijs.acs.objectexplorer.engine.Invocation invocation) {
	
}
/**
 * invokeOperation method comment.
 */
public void invokeOperation(si.ijs.acs.objectexplorer.engine.Operation op, java.lang.Object[] params) {
		if (op !=null){
			if (op.isInvocation()) {
				op.invokeAsync(params,new OERemoteResponseCallback(this,ReporterBean.raID));
			}
			else {
			  RemoteCall ret=op.invoke(params);
			  SmartTextPane resultArea = getoperationResultArea();
			  try
				{
					boolean errorResponse = ret.isErrorResponse(); 
					if (errorResponse)
					{
						// needed since carent does not point always to the end
						resultArea.setCaretPosition(resultArea.getText().length());
						resultArea.setLogicalStyle(redStyleOP);
					}
					
					resultArea.append(ReporterBean.toString(ret, reporter.isExpand() | errorResponse)+"\n");

					if (errorResponse)
						resultArea.append("\n");
				}
				finally
				{
				    resultArea.setLogicalStyle(blackStyleOP);
				}
			  
			}
		}
}
/**
 * Insert the method's description here.
 * Creation date: (3/27/2001 10:41:16 PM)
 * @return boolean
 */
public boolean isDestroyed() {
	return destroyed;
}
/**
 * Creation date: (23.10.2001 19:58:56)
 * @return boolean
 */
public boolean isExpand() {
	return reporter.isExpand();
	//return getJCheckBoxMenuItem1().isSelected();
}
/**
 * Comment
 */
public void jButton1_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	if (enabled) {
		getJButton1().setText("Enable output");
		enabled=false;
	}
	else {
		getJButton1().setText("Disable output");
		enabled=true;
	}
}
/**
 * Comment
 */
public void jCheckBoxMenuItem1_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	reportLength=-1;
	return;
}
/**
 * Comment
 */
public void jList2_MouseClicked(java.awt.event.MouseEvent mouseEvent) {
	//2010-02-12 panta@naoj
	//if ((mouseEvent.getClickCount() == 2)
	if ((mouseEvent.getClickCount() == 1)
		&& (mouseEvent.getModifiers() == InputEvent.BUTTON1_MASK)) {
		if (getoperationsList()
			.locationToIndex(new java.awt.Point(10, mouseEvent.getY()))
			== -1)
			return;
		Operation op = (Operation) getoperationsList().getSelectedValue();
		boolean doParams = false;
		boolean[] mask = op.getMask();
		if (op.getParameterTypes().length != 0) {
			for (int i = 0; i < mask.length; i++) {
				if (mask[i]) {
					doParams = true;
					break;
				}
			}
		}
		if (doParams)
			new CallMethodDialog(op, this, true, notifier, this).setVisible(true);
		else
			invokeOperation(op, new Object[op.getParameterTypes().length]);
	}
	return;
}
/**
 * Comment
 */
public void jTextField1_ActionPerformed() {
  if (editing && getJTextField1().getText()!="") { 
	  maxLines=0;
	  try {
		  int lines = Integer.parseInt(getJTextField1().getText());
		  if (lines>0)
			  if (lines < Integer.MAX_VALUE) maxLines=lines;
			  else maxLines=Integer.MAX_VALUE;
		  else maxLines=1;
	  }
	  catch (NumberFormatException e) {
		  maxLines=reportLength;
	  }
	  getJTextField1().setText(Integer.toString(maxLines));
	  //java.awt.Font font=getJTextField1().getFont();
	  getJTextField1().setBackground(getJPanel1().getBackground()); //setFont(new java.awt.Font(font.getFontName(),font.BOLD,font.getSize()));
	  getReportArea().setMaxLines(maxLines * (reportLength-1));
	  checkChartPointsSize();
	  editing=false;
  }
}
/**
 * un-BOLD-enes the lines textfield
 */
public void jTextField1_FocusGained() {
	if (!editing) {
	   //java.awt.Font font=getJTextField1().getFont();
	   getJTextField1().setBackground(getReportArea().getBackground()); // Font(new java.awt.Font(font.getFontName(),font.PLAIN,font.getSize()));
	   editing=true;
	}
}
/**
 * Comment
 */
public void jTextField1_FocusLost() {
  if (editing) {
	 getJTextField1().setText(Integer.toString(getReportArea().getMaxLines()));
	 //java.awt.Font font=getJTextField1().getFont();
	 getJTextField1().setBackground(getJPanel1().getBackground());
	 editing=false;
  }
}
/**
 * main entrypoint - starts the part when it is run as an application
 * @param args java.lang.String[]
 */
public static void main(java.lang.String[] args) {
	try {
		CallMethodDialog aCallMethodDialog;
		aCallMethodDialog = new CallMethodDialog();
		aCallMethodDialog.addWindowListener(new java.awt.event.WindowAdapter() {
			public void windowClosing(java.awt.event.WindowEvent e) {
				System.exit(0);
			};
		});
		aCallMethodDialog.setVisible(true);
	} catch (Throwable exception) {
		System.err.println("Exception occurred in main() of javax.swing.JFrame");
		exception.printStackTrace(System.out);
	}
}
/**
 * Creation date: (25.10.2001 0:12:55)
 * @param response si.ijs.acs.objectexplorer.engine.RemoteResponse
 */
private void processChartValues(RemoteResponse response) {
	Object[] data = response.getData();
	int length = 0;
	for (int i = 0; i < numberIndexes.size(); i++) {
		if (numberIndexes.get(i) instanceof java.util.ArrayList)
			length += ((java.util.ArrayList)numberIndexes.get(i)).size();
	}
	length += numberIndexes.size()+1;
	double[] newNumbers=new double[length];
	newNumbers[0]=(double)response.getTimestamp()/1000.0;
	maxs[0]=newNumbers[0];
	// skip first element (time)
	int k = 1;
	for (int i = 0; i < numberIndexes.size(); i++) {
		if (numberIndexes.get(i) instanceof java.util.ArrayList) {
			for (int j = 0; j < ((java.util.ArrayList)numberIndexes.get(i)).size(); j++) {
				int numIndex=((Number[])((java.util.ArrayList)numberIndexes.get(i)).get(j))[0].intValue();
				int numSubIndex=((Number[])((java.util.ArrayList)numberIndexes.get(i)).get(j))[1].intValue();
				double val=0;
				if (((Object [])data[numIndex])[numSubIndex] instanceof Long) {
					val=((Long)((Object [])data[numIndex])[numSubIndex]).longValue()/1000;
				}
				else val=((Number)((Object [])data[numIndex])[numSubIndex]).doubleValue();
		
				newNumbers[k]= val;
				if (val<mins[k]) mins[k]=val;
				if (val>maxs[k]) maxs[k]=val;
				k++;
			}
		}
		else {
			int numIndex=((Number)numberIndexes.get(i)).intValue();
			double val=0;
			if (data[numIndex].getClass().isArray()) {
				//Have to handle this.
				//Plotting all the subpoints is an alternative.
			}
			else if (data[numIndex] instanceof Long) {
				val=((Long)data[numIndex]).longValue()/1000;
			}
			else val=((Number)data[numIndex]).doubleValue();
		
			// skip first element (time)
			newNumbers[k]= val;
			if (val<mins[k]) mins[k]=val;
			if (val>maxs[k]) maxs[k]=val;
			k++;
		}
	}
	chartData.add(newNumbers);
	if (chartData.size()>maxLines) chartData.remove(0);
	if ((selectedChartValue!=-1) && (selectedChartXValue!=-1)){
		model.reloadChartData();
	}
	else {
		mins[0]=((double[])chartData.get(0))[0];
	}
}
/**
 * Creation date: (25.10.2001 0:09:50)
 * @return java.lang.String
 * @param response si.ijs.acs.objectexplorer.engine.RemoteResponse
 */
private static String processResponse(RemoteResponse response, boolean expand) {
		return("\n"+ReporterBean.toString(response, expand)+"\n");
}
/**
 * Comment
 */
public void remoteResponseWindow_Initialize() {
	getReportArea().getPopup().add(getJCheckBoxMenuItem1());
	getJTextField1().setText(Integer.toString(maxLines));
	if (rr!=null && rr.getInvocation().isControllable()){
		DefaultListModel model=new DefaultListModel();
		Operation[] ops=rr.getInvocation().getOperations();
		for (int i = 0; i < ops.length; i++){
		   model.addElement(ops[i]);
		}
		getoperationsList().setModel(model);
		getoperationsList().revalidate();
	}
	
	// allow window to be closed, since it is not controllable
	if (rr!=null && !rr.getInvocation().isControllable())
	{
		setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
	}
	return;
}
/**
 * Comment
 */
public void remoteResponseWindow_WindowDeiconified(java.awt.event.WindowEvent windowEvent) {
	getReportArea().append(minimText.toString());
	minimText=new StringBuffer();
	minimTextReportCount=0;
	return;
}
/**
 * Method unpacks the RemoteResponse data and writes it into the ResultArea
 *
 * Creation date: (2/25/00 12:44:55 PM)
 *
 *  
 */
public void reportRemoteResponse(RemoteResponse response) {
	getJLabel3().setText(String.valueOf(response.getSequenceNumber()));
	getmessageField().setText(response.getName());
	if (enabled) {
		
	    processChartValues(response);
		
	    // process text output only if tab is enabled
	    if (textOutputTabSelected)
	    {
			boolean errorResponse = response.isErrorResponse(); 
	
			String resultString = processResponse(response, errorResponse | isExpand());
	
			if (reportLength==-1) {
		        reportLength=DataFormatter.getLineCount(resultString);
				editing=true;
				jTextField1_ActionPerformed();
			}
	
			if (getState() == java.awt.Frame.ICONIFIED) {
				minimText.append(resultString);
				if (minimTextReportCount > maxLines) {
					minimText.delete(0, resultString.length());
				}
				else minimTextReportCount++;
			}
			else
			{
				SmartTextPane resultArea = getReportArea();
				try
				{
					if (errorResponse)
					{
						// needed since carent does not point always to the end
						resultArea.setCaretPosition(resultArea.getText().length());
						resultArea.setLogicalStyle(redStyle);
					}
					
					resultArea.append(resultString);
		
					if (errorResponse)
						resultArea.append("\n");
				}
				finally
				{
				    resultArea.setLogicalStyle(blackStyle);
				}
			}
	    }
	}
		
}
/**
 * responseReceived method comment.
 */
public void responseReceived(si.ijs.acs.objectexplorer.engine.RemoteResponse response) {
	SmartTextPane resultArea = getoperationResultArea();

	try
	{
		boolean errorResponse = response.isErrorResponse(); 
		if (errorResponse)
		{
			// needed since carent does not point always to the end
			resultArea.setCaretPosition(resultArea.getText().length());
			resultArea.setLogicalStyle(redStyleOP);
		}
				
		resultArea.append(ReporterBean.toString(response, reporter.isExpand() | errorResponse) + "\n");

		if (errorResponse)
			resultArea.append("\n");
	}
	finally
	{
	    resultArea.setLogicalStyle(blackStyleOP);
	}

}
/**
 * Insert the method's description here.
 * Creation date: (3/27/2001 10:41:16 PM)
 * @param newDestroyed boolean
 */
public void setDestroyed(boolean newDestroyed) {
	destroyed = newDestroyed;
}
/**
 * Insert the method's description here.
 * Creation date: (11.2.2002 19:56:30)
 * @param newDisposeOnDestroy boolean
 */
public void setDisposeOnDestroy(boolean newDisposeOnDestroy) {
	disposeOnDestroy = newDisposeOnDestroy;
	if (disposeOnDestroy && destroyed) dispose();
}
/**
 * Creation date: (25.10.2001 21:59:04)
 * @param newSelectedChartValue int
 */
public synchronized void setSelectedChartValue(int index) {
	if (index != selectedChartValue) {
		if ((selectedChartValue == -1) && (selectedChartXValue != -1)) {
			model.setChartService(getTrend().getChartArea().getChartService());
			getTrend().getChartArea().addDataModel(model);
		}
		else {
		selectedChartValue = index;
		model.reloadChartData();
		}
		selectedChartValue = index;
	}
}
/**
 * Insert the method's description here.
 * Creation date: (6.2.2002 23:42:48)
 * @param newSelectedChartXValue int
 */
public synchronized void setSelectedChartXValue(int newSelectedChartXValue) {
	if (newSelectedChartXValue != selectedChartXValue) {
		if ((selectedChartXValue == -1) && (selectedChartValue != -1)) {
			model.setChartService(getTrend().getChartArea().getChartService());
			getTrend().getChartArea().addDataModel(model);
		} else {
			selectedChartXValue = newSelectedChartXValue;
			model.reloadChartData();
		}
		selectedChartXValue=newSelectedChartXValue;
	}
}
}
