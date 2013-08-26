package si.ijs.acs.objectexplorer;

import com.cosylab.gui.components.r2.SmartTextPane;

/**
 * Insert the type's description here.
 * Creation date: (9/26/98 3:07:37 PM)
 * @author: Miha Kadunc
 *
 *   edited:  |     by:     |  for the reason of:
 *________________________________________________________________________
 * 07.05.2001 | Miha Kadunc | Making TextArea selectable and non-editable
 *            |             | and changing the look a bit
 * 
 */
public class ErrorDialog extends javax.swing.JDialog {
	IvjEventHandler ivjEventHandler = new IvjEventHandler();
	private javax.swing.JButton ivjJButton1 = null;
	private javax.swing.JPanel ivjJDialogContentPane = null;
	private javax.swing.JScrollPane ivjJScrollPane1 = null;



class IvjEventHandler implements java.awt.event.ActionListener {
		public void actionPerformed(java.awt.event.ActionEvent e) {
			if (e.getSource() == ErrorDialog.this.getJButton1()) 
				connEtoM1(e);
		};
	};
/**
 * Constructor
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
public ErrorDialog() {
	super();
	initialize();
}
/**
 * ErrorDialog constructor comment.
 * @param owner java.awt.Frame
 * @param title java.lang.String
 * @param modal boolean
 */
public ErrorDialog(java.awt.Frame owner, String title, String message) {
	super(owner, true);
	initialize();
	setTitle(title);
	ivjJTextPane1.setText(message);
	setLocation(owner.getX()+(owner.getWidth()-getWidth())/2,owner.getY()+(owner.getHeight()-getHeight())/2);
	show();
	repaint();
}
/**
 * connEtoM1:  (JButton1.action.actionPerformed(java.awt.event.ActionEvent) --> ErrorDialog.dispose()V)
 * @param arg1 java.awt.event.ActionEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoM1(java.awt.event.ActionEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		this.dispose();
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
			ivjJButton1.setText("OK");
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
 * Return the JDialogContentPane property value.
 * @return javax.swing.JPanel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JPanel getJDialogContentPane() {
	if (ivjJDialogContentPane == null) {
		try {
			ivjJDialogContentPane = new javax.swing.JPanel();
			ivjJDialogContentPane.setName("JDialogContentPane");
			ivjJDialogContentPane.setLayout(new java.awt.GridBagLayout());

			java.awt.GridBagConstraints constraintsJButton1 = new java.awt.GridBagConstraints();
			constraintsJButton1.gridx = 0; constraintsJButton1.gridy = 1;
			constraintsJButton1.ipadx = 50;
			constraintsJButton1.insets = new java.awt.Insets(6, 0, 6, 0);
			getJDialogContentPane().add(getJButton1(), constraintsJButton1);

			java.awt.GridBagConstraints constraintsJScrollPane1 = new java.awt.GridBagConstraints();
			constraintsJScrollPane1.gridx = 0; constraintsJScrollPane1.gridy = 0;
			constraintsJScrollPane1.fill = java.awt.GridBagConstraints.BOTH;
			constraintsJScrollPane1.weightx = 1.0;
			constraintsJScrollPane1.weighty = 1.0;
			constraintsJScrollPane1.insets = new java.awt.Insets(6, 6, 6, 4);
			getJDialogContentPane().add(getJScrollPane1(), constraintsJScrollPane1);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJDialogContentPane;
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
			ivjJScrollPane1.setHorizontalScrollBarPolicy(javax.swing.JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
			getJScrollPane1().setViewportView(getJTextPane1());
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
 * Called whenever the part throws an exception.
 * @param exception java.lang.Throwable
 */
private void handleException(Throwable exception) {

	/* Uncomment the following lines to print uncaught exceptions to stdout */
	 System.out.println("--------- UNCAUGHT EXCEPTION in Error Dialog ---------");
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
		setName("ErrorDialog");
		setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
		setSize(467, 164);
		setContentPane(getJDialogContentPane());
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
		ErrorDialog aErrorDialog;
		aErrorDialog = new ErrorDialog();
		aErrorDialog.setModal(true);
		aErrorDialog.addError("Tole je testni error\nkva pa zdej?");
		aErrorDialog.addWindowListener(new java.awt.event.WindowAdapter() {
			public void windowClosing(java.awt.event.WindowEvent e) {
				System.exit(0);
			};
		});
		java.awt.Insets insets = aErrorDialog.getInsets();
		aErrorDialog.setSize(aErrorDialog.getWidth() + insets.left + insets.right, aErrorDialog.getHeight() + insets.top + insets.bottom);
		aErrorDialog.show();
	} catch (Throwable exception) {
		System.err.println("Exception occurred in main() of javax.swing.JDialog");
		exception.printStackTrace(System.out);
	}
}
	private SmartTextPane ivjJTextPane1 = null;/**
 * ErrorDialog constructor comment.
 * @param owner java.awt.Frame
 * @param title java.lang.String
 * @param modal boolean
 */
public void addError(final String message) {
	java.awt.EventQueue.invokeLater(new Runnable() {
		public void run() {
			ErrorDialog.this.getJTextPane1().setText(getJTextPane1().getText()+"\n"+message);
			repaint();
		};
	});
}/**
 * Return the JTextPane1 property value.
 * @return javax.swing.JTextPane
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextPane getJTextPane1() {
	if (ivjJTextPane1 == null) {
		try {
			ivjJTextPane1 = new SmartTextPane();
			ivjJTextPane1.setName("JTextPane1");
			ivjJTextPane1.setBounds(0, 0, 319, 44);
			ivjJTextPane1.setEditable(false);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJTextPane1;
};
}
