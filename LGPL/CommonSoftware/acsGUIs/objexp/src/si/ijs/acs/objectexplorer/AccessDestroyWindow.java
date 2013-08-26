package si.ijs.acs.objectexplorer;

/**
 * Insert the type's description here.
 * Creation date: (28.6.2001 0:46:37)
 * @author: Miha Kadunc
 */
public class AccessDestroyWindow extends javax.swing.JDialog {
	private javax.swing.JButton ivjJButton1 = null;
	private javax.swing.JPanel ivjJFrameContentPane = null;
	private javax.swing.JLabel ivjJLabel1 = null;
	IvjEventHandler ivjEventHandler = new IvjEventHandler();
	private int errorCount=0;

class IvjEventHandler implements java.awt.event.ActionListener {
		public void actionPerformed(java.awt.event.ActionEvent e) {
			if (e.getSource() == AccessDestroyWindow.this.getJButton1()) 
				connEtoC1(e);
		};
	};
/**
 * AccessDestroyer constructor comment.
 */
public AccessDestroyWindow() {
	super();
	initialize();
}
/**
 * AccessDestroyer constructor comment.
 * @param treehandler si.ijs.acs.objectexplorer.TreeHandlerBean
 */
public AccessDestroyWindow(javax.swing.JFrame owner)
{
	super(owner,true);
	initialize();
	setLocation(owner.getX()+(owner.getWidth()-getWidth())/2,owner.getY()+(owner.getHeight()-getHeight())/2);
}
/**
 * Insert the method's description here.
 * Creation date: (29.6.2001 23:24:43)
 */
public void addError() {
	errorCount++;
	if (errorCount==1) 	getJLabel1().setText("Error occured");
	else getJLabel1().setText(errorCount+" errors occured");
}
/**
 * connEtoC1:  (JButton1.action.actionPerformed(java.awt.event.ActionEvent) --> AccessDestroyWindow.terminate()V)
 * @param arg1 java.awt.event.ActionEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC1(java.awt.event.ActionEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		this.terminate();
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
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
			ivjJButton1.setToolTipText("Click this if OE doesn\'t close after some time");
			ivjJButton1.setText("Terminate");
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
 * Return the JFrameContentPane property value.
 * @return javax.swing.JPanel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JPanel getJFrameContentPane() {
	if (ivjJFrameContentPane == null) {
		try {
			ivjJFrameContentPane = new javax.swing.JPanel();
			ivjJFrameContentPane.setName("JFrameContentPane");
			ivjJFrameContentPane.setLayout(new java.awt.GridBagLayout());

			java.awt.GridBagConstraints constraintsJButton1 = new java.awt.GridBagConstraints();
			constraintsJButton1.gridx = 0; constraintsJButton1.gridy = 1;
			constraintsJButton1.insets = new java.awt.Insets(0, 0, 6, 0);
			getJFrameContentPane().add(getJButton1(), constraintsJButton1);

			java.awt.GridBagConstraints constraintsJLabel1 = new java.awt.GridBagConstraints();
			constraintsJLabel1.gridx = 0; constraintsJLabel1.gridy = 0;
			constraintsJLabel1.fill = java.awt.GridBagConstraints.BOTH;
			constraintsJLabel1.weightx = 1.0;
			constraintsJLabel1.weighty = 1.0;
			constraintsJLabel1.insets = new java.awt.Insets(0, 0, 0, 2);
			getJFrameContentPane().add(getJLabel1(), constraintsJLabel1);
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
			ivjJLabel1.setText("Please wait...");
			ivjJLabel1.setFont(new java.awt.Font("dialog", 1, 12));
			ivjJLabel1.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
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
 * Called whenever the part throws an exception.
 * @param exception java.lang.Throwable
 */
private void handleException(java.lang.Throwable exception) {

	/* Uncomment the following lines to print uncaught exceptions to stdout */
	 System.out.println("--------- UNCAUGHT EXCEPTION in AccessDestroyWindow---------");
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
}
/**
 * Initialize the class.
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void initialize() {
	try {
		// user code begin {1}
		// user code end
		setName("AccessDestroyer");
		setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE);
		setResizable(false);
		setVisible(false);
		setModal(true);
		setSize(273, 85);
		setTitle("Destroying Remote Access");
		setContentPane(getJFrameContentPane());
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
		AccessDestroyWindow aAccessDestroyer;
		aAccessDestroyer = new AccessDestroyWindow();
		aAccessDestroyer.addWindowListener(new java.awt.event.WindowAdapter() {
			public void windowClosing(java.awt.event.WindowEvent e) {
				System.exit(0);
			};
		});
		aAccessDestroyer.show();
		java.awt.Insets insets = aAccessDestroyer.getInsets();
		aAccessDestroyer.setSize(aAccessDestroyer.getWidth() + insets.left + insets.right, aAccessDestroyer.getHeight() + insets.top + insets.bottom);
		aAccessDestroyer.setVisible(true);
	} catch (Throwable exception) {
		System.err.println("Exception occurred in main() of javax.swing.JFrame");
		exception.printStackTrace(System.out);
	}
}
/**
 * Insert the method's description here.
 * Creation date: (28.6.2001 1:17:05)
 */
public void terminate() {
  dispose();
}
/**
 * Insert the method's description here.
 * Creation date: (28.6.2001 1:17:05)
 */
public void terminate(boolean checkErrors) {
  if (checkErrors && errorCount>0) {
		  setTitle("Remote access destroyed");
		  getJButton1().setToolTipText("Close this window");
		  getJButton1().setText("Close");
		  errorCount=0;		  
  }
  else terminate(); 
}
}
