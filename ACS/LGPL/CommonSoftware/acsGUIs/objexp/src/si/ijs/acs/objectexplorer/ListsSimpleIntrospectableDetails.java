package si.ijs.acs.objectexplorer;

import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;

import com.cosylab.gui.components.r2.SmartPanel;
/**
 * Insert the type's description here.
 * Creation date: (3.2.2002 14:46:22)
 * @author: 
 */
public class ListsSimpleIntrospectableDetails extends SmartPanel {
	private javax.swing.JList ivjattributesList = null;
	private javax.swing.JCheckBox ivjJCheckBox1 = null;
	private javax.swing.JLabel ivjJLabel2 = null;
	private javax.swing.JLabel ivjJLabel3 = null;
	private javax.swing.JLabel ivjJLabel4 = null;
	private javax.swing.JPanel ivjJPanel1 = null;
	private javax.swing.JScrollPane ivjJScrollPane2 = null;
	private javax.swing.JScrollPane ivjJScrollPane3 = null;
	private javax.swing.JLabel ivjObjectNameLabel = null;
	private javax.swing.JList ivjoperationsList = null;
	private ListsHandlerBean listshandler=null;
	IvjEventHandler ivjEventHandler = new IvjEventHandler();

class IvjEventHandler implements java.awt.event.ActionListener, java.awt.event.KeyListener, java.awt.event.MouseListener, java.awt.event.MouseMotionListener {
		public void actionPerformed(java.awt.event.ActionEvent e) {
			if (e.getSource() == ListsSimpleIntrospectableDetails.this.getJCheckBox1()) 
				connEtoC1(e);
		};
		public void keyPressed(java.awt.event.KeyEvent e) {};
		public void keyReleased(java.awt.event.KeyEvent e) {};
		public void keyTyped(java.awt.event.KeyEvent e) {
			if (e.getSource() == ListsSimpleIntrospectableDetails.this.getoperationsList()) 
				connEtoC4(e);
			if (e.getSource() == ListsSimpleIntrospectableDetails.this.getattributesList()) 
				connEtoC5(e);
		};
		public void mouseClicked(java.awt.event.MouseEvent e) {
			if (e.getSource() == ListsSimpleIntrospectableDetails.this.getoperationsList()) 
				connEtoC2(e);
			if (e.getSource() == ListsSimpleIntrospectableDetails.this.getattributesList()) 
				connEtoC3(e);
		};
		public void mouseDragged(java.awt.event.MouseEvent e) {};
		public void mouseEntered(java.awt.event.MouseEvent e) {};
		public void mouseExited(java.awt.event.MouseEvent e) {
			if (e.getSource() == ListsSimpleIntrospectableDetails.this.getoperationsList()) 
				connEtoC8(e);
			if (e.getSource() == ListsSimpleIntrospectableDetails.this.getattributesList()) 
				connEtoC9(e);
		};
		public void mouseMoved(java.awt.event.MouseEvent e) {
			if (e.getSource() == ListsSimpleIntrospectableDetails.this.getoperationsList()) 
				connEtoC6(e);
			if (e.getSource() == ListsSimpleIntrospectableDetails.this.getattributesList()) 
				connEtoC7(e);
		};
		public void mousePressed(java.awt.event.MouseEvent e) {};
		public void mouseReleased(java.awt.event.MouseEvent e) {};
	};
/**
 * ListsSimpleIntrospectableDetails constructor comment.
 */
public ListsSimpleIntrospectableDetails() {
	super();
	initialize();
}
/**
 * ListsSimpleIntrospectableDetails constructor comment.
 * @param layout java.awt.LayoutManager
 */
public ListsSimpleIntrospectableDetails(ListsHandlerBean listshandler) {
	super();
	initialize();
	this.listshandler=listshandler;
}
/**
 * Insert the method's description here.
 * Creation date: (3.2.2002 15:32:11)
 */
public void attributesAction() {
	if (listshandler!=null) {
		listshandler.clickedItem(getattributesList().getSelectedValue());
	}
}
/**
 * Comment
 */
public void attributesList_KeyTyped(java.awt.event.KeyEvent keyEvent) {
	if (keyEvent.getModifiers() == KeyEvent.VK_ENTER) {
		attributesAction();
	}
	return;
}
/**
 * Comment
 */
public void attributesList_MouseClicked(java.awt.event.MouseEvent mouseEvent) {
	if (mouseEvent.getModifiers()==InputEvent.BUTTON1_MASK) {
		if (getattributesList().locationToIndex(new java.awt.Point(10,mouseEvent.getY()))==-1) return;
		attributesAction();
	}
	return;
}
/**
 * Comment
 */
public void attributesList_MouseExited(java.awt.event.MouseEvent mouseEvent) {
	getattributesList().clearSelection();
	return;
}
/**
 * Comment
 */
public void attributesList_MouseMoved(java.awt.event.MouseEvent mouseEvent) {
	int index=getattributesList().locationToIndex(mouseEvent.getPoint());
	if (index>-1) getattributesList().setSelectedIndex(index);
	else getattributesList().clearSelection();
	return;
}
/**
 * Called whenever the part throws an exception.
 * @param exception java.lang.Throwable
 */
public void clear() {
	setDeviceText("");
	setAttributesModel(new javax.swing.DefaultListModel());
	setOperationsModel(new javax.swing.DefaultListModel());
}
/**
 * Comment
 */
private void clickedSpecial() {
	listshandler.setSpecial(getJCheckBox1().isSelected());
}
/**
 * connEtoC1:  (JCheckBox1.action.actionPerformed(java.awt.event.ActionEvent) --> ListsSimpleIntrospectableDetails.clickedSpecial()V)
 * @param arg1 java.awt.event.ActionEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC1(java.awt.event.ActionEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		this.clickedSpecial();
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoC2:  (operationsList.mouse.mouseClicked(java.awt.event.MouseEvent) --> ListsSimpleIntrospectableDetails.operationsList_MouseClicked(Ljava.awt.event.MouseEvent;)V)
 * @param arg1 java.awt.event.MouseEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC2(java.awt.event.MouseEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		this.operationsList_MouseClicked(arg1);
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoC3:  (attributesList.mouse.mouseClicked(java.awt.event.MouseEvent) --> ListsSimpleIntrospectableDetails.attributesList_MouseClicked(Ljava.awt.event.MouseEvent;)V)
 * @param arg1 java.awt.event.MouseEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC3(java.awt.event.MouseEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		this.attributesList_MouseClicked(arg1);
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoC4:  (operationsList.key.keyTyped(java.awt.event.KeyEvent) --> ListsSimpleIntrospectableDetails.operationsList_KeyTyped(Ljava.awt.event.KeyEvent;)V)
 * @param arg1 java.awt.event.KeyEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC4(java.awt.event.KeyEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		this.operationsList_KeyTyped(arg1);
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoC5:  (attributesList.key.keyTyped(java.awt.event.KeyEvent) --> ListsSimpleIntrospectableDetails.attributesList_KeyTyped(Ljava.awt.event.KeyEvent;)V)
 * @param arg1 java.awt.event.KeyEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC5(java.awt.event.KeyEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		this.attributesList_KeyTyped(arg1);
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoC6:  (operationsList.mouseMotion.mouseMoved(java.awt.event.MouseEvent) --> ListsSimpleIntrospectableDetails.operationsList_MouseMoved(Ljava.awt.event.MouseEvent;)V)
 * @param arg1 java.awt.event.MouseEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC6(java.awt.event.MouseEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		this.operationsList_MouseMoved(arg1);
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoC7:  (attributesList.mouseMotion.mouseMoved(java.awt.event.MouseEvent) --> ListsSimpleIntrospectableDetails.attributesList_MouseMoved(Ljava.awt.event.MouseEvent;)V)
 * @param arg1 java.awt.event.MouseEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC7(java.awt.event.MouseEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		this.attributesList_MouseMoved(arg1);
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoC8:  (operationsList.mouse.mouseExited(java.awt.event.MouseEvent) --> ListsSimpleIntrospectableDetails.operationsList_MouseExited(Ljava.awt.event.MouseEvent;)V)
 * @param arg1 java.awt.event.MouseEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC8(java.awt.event.MouseEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		this.operationsList_MouseExited(arg1);
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoC9:  (attributesList.mouse.mouseExited(java.awt.event.MouseEvent) --> ListsSimpleIntrospectableDetails.attributesList_MouseExited(Ljava.awt.event.MouseEvent;)V)
 * @param arg1 java.awt.event.MouseEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC9(java.awt.event.MouseEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		this.attributesList_MouseExited(arg1);
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connPtoP1SetTarget:  (attributesList.this <--> operationsList.nextFocusableComponent)
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connPtoP1SetTarget() {
	/* Set the target from the source */
	try {
		getoperationsList().setNextFocusableComponent(getattributesList());
		// user code begin {1}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * Return the attributesList property value.
 * @return javax.swing.JList
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JList getattributesList() {
	if (ivjattributesList == null) {
		try {
			ivjattributesList = new javax.swing.JList();
			ivjattributesList.setName("attributesList");
			ivjattributesList.setBounds(0, 0, 76, 254);
			ivjattributesList.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjattributesList;
}
/**
 * Return the JCheckBox1 property value.
 * @return javax.swing.JCheckBox
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JCheckBox getJCheckBox1() {
	if (ivjJCheckBox1 == null) {
		try {
			ivjJCheckBox1 = new javax.swing.JCheckBox();
			ivjJCheckBox1.setName("JCheckBox1");
			ivjJCheckBox1.setText("Show special operations and attributes");
			ivjJCheckBox1.setSelected(true);
			ivjJCheckBox1.setHorizontalAlignment(javax.swing.SwingConstants.RIGHT);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJCheckBox1;
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
			ivjJLabel2.setText("Operations");
			ivjJLabel2.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
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
			ivjJLabel3.setText("Attributes");
			ivjJLabel3.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
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
			ivjJLabel4.setText("Object:");
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

			java.awt.GridBagConstraints constraintsJLabel4 = new java.awt.GridBagConstraints();
			constraintsJLabel4.gridx = 0; constraintsJLabel4.gridy = 0;
			constraintsJLabel4.fill = java.awt.GridBagConstraints.BOTH;
			constraintsJLabel4.insets = new java.awt.Insets(0, 4, 0, 5);
			getJPanel1().add(getJLabel4(), constraintsJLabel4);

			java.awt.GridBagConstraints constraintsJCheckBox1 = new java.awt.GridBagConstraints();
			constraintsJCheckBox1.gridx = 2; constraintsJCheckBox1.gridy = 0;
			constraintsJCheckBox1.fill = java.awt.GridBagConstraints.HORIZONTAL;
			constraintsJCheckBox1.anchor = java.awt.GridBagConstraints.WEST;
			constraintsJCheckBox1.insets = new java.awt.Insets(0, 0, 0, 5);
			getJPanel1().add(getJCheckBox1(), constraintsJCheckBox1);

			java.awt.GridBagConstraints constraintsObjectNameLabel = new java.awt.GridBagConstraints();
			constraintsObjectNameLabel.gridx = 1; constraintsObjectNameLabel.gridy = 0;
			constraintsObjectNameLabel.fill = java.awt.GridBagConstraints.BOTH;
			constraintsObjectNameLabel.weightx = 1.0;
			constraintsObjectNameLabel.insets = new java.awt.Insets(4, 4, 4, 4);
			getJPanel1().add(getObjectNameLabel(), constraintsObjectNameLabel);
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
 * Return the JScrollPane2 property value.
 * @return javax.swing.JScrollPane
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JScrollPane getJScrollPane2() {
	if (ivjJScrollPane2 == null) {
		try {
			ivjJScrollPane2 = new javax.swing.JScrollPane();
			ivjJScrollPane2.setName("JScrollPane2");
			getJScrollPane2().setViewportView(getoperationsList());
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
			getJScrollPane3().setViewportView(getattributesList());
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
 * Return the ObjectNameLabel property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getObjectNameLabel() {
	if (ivjObjectNameLabel == null) {
		try {
			ivjObjectNameLabel = new javax.swing.JLabel();
			ivjObjectNameLabel.setName("ObjectNameLabel");
			ivjObjectNameLabel.setText("NAME");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjObjectNameLabel;
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
			ivjoperationsList.setBounds(0, 0, 160, 120);
			ivjoperationsList.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
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
 * Called whenever the part throws an exception.
 * @param exception java.lang.Throwable
 */
private void handleException(java.lang.Throwable exception) {

	/* Uncomment the following lines to print uncaught exceptions to stdout */
	 System.out.println("--------- UNCAUGHT EXCEPTION in ListsSimpleIntrospectableDetails ---------");
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
	getJCheckBox1().addActionListener(ivjEventHandler);
	getoperationsList().addMouseListener(ivjEventHandler);
	getattributesList().addMouseListener(ivjEventHandler);
	getoperationsList().addKeyListener(ivjEventHandler);
	getattributesList().addKeyListener(ivjEventHandler);
	getoperationsList().addMouseMotionListener(ivjEventHandler);
	getattributesList().addMouseMotionListener(ivjEventHandler);
	connPtoP1SetTarget();
}
/**
 * Initialize the class.
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void initialize() {
	try {
		// user code begin {1}
		// user code end
		setName("ListsSimpleIntrospectableDetails");
		setLayout(new java.awt.GridBagLayout());
		setSize(599, 456);

		java.awt.GridBagConstraints constraintsJPanel1 = new java.awt.GridBagConstraints();
		constraintsJPanel1.gridx = 0; constraintsJPanel1.gridy = 0;
		constraintsJPanel1.gridwidth = 2;
		constraintsJPanel1.fill = java.awt.GridBagConstraints.BOTH;
		constraintsJPanel1.weightx = 1.0;
		constraintsJPanel1.insets = new java.awt.Insets(4, 4, 4, 4);
		add(getJPanel1(), constraintsJPanel1);

		java.awt.GridBagConstraints constraintsJLabel2 = new java.awt.GridBagConstraints();
		constraintsJLabel2.gridx = 0; constraintsJLabel2.gridy = 1;
		constraintsJLabel2.insets = new java.awt.Insets(4, 4, 4, 4);
		add(getJLabel2(), constraintsJLabel2);

		java.awt.GridBagConstraints constraintsJLabel3 = new java.awt.GridBagConstraints();
		constraintsJLabel3.gridx = 1; constraintsJLabel3.gridy = 1;
		constraintsJLabel3.insets = new java.awt.Insets(4, 4, 4, 4);
		add(getJLabel3(), constraintsJLabel3);

		java.awt.GridBagConstraints constraintsJScrollPane2 = new java.awt.GridBagConstraints();
		constraintsJScrollPane2.gridx = 0; constraintsJScrollPane2.gridy = 2;
		constraintsJScrollPane2.fill = java.awt.GridBagConstraints.BOTH;
		constraintsJScrollPane2.weightx = 1.0;
		constraintsJScrollPane2.weighty = 1.0;
		constraintsJScrollPane2.insets = new java.awt.Insets(4, 4, 4, 4);
		add(getJScrollPane2(), constraintsJScrollPane2);

		java.awt.GridBagConstraints constraintsJScrollPane3 = new java.awt.GridBagConstraints();
		constraintsJScrollPane3.gridx = 1; constraintsJScrollPane3.gridy = 2;
		constraintsJScrollPane3.fill = java.awt.GridBagConstraints.BOTH;
		constraintsJScrollPane3.weightx = 1.0;
		constraintsJScrollPane3.weighty = 1.0;
		constraintsJScrollPane3.insets = new java.awt.Insets(4, 4, 4, 4);
		add(getJScrollPane3(), constraintsJScrollPane3);
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
		ListsSimpleIntrospectableDetails aListsSimpleIntrospectableDetails;
		aListsSimpleIntrospectableDetails = new ListsSimpleIntrospectableDetails();
		frame.setContentPane(aListsSimpleIntrospectableDetails);
		frame.setSize(aListsSimpleIntrospectableDetails.getSize());
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
 * Insert the method's description here.
 * Creation date: (3.2.2002 15:32:11)
 */
public void operationsAction() {
	if (listshandler!=null) {
		listshandler.clickedItem(getoperationsList().getSelectedValue());
	}
}
/**
 * Comment
 */
public void operationsList_KeyTyped(java.awt.event.KeyEvent keyEvent) {
	if (keyEvent.getModifiers() == KeyEvent.VK_ENTER) {
		operationsAction();
	}
	return;
}
/**
 * Comment
 */
public void operationsList_MouseClicked(java.awt.event.MouseEvent mouseEvent) {
	if (mouseEvent.getModifiers()==InputEvent.BUTTON1_MASK) {
		if (getoperationsList().locationToIndex(new java.awt.Point(10,mouseEvent.getY()))==-1) return;
		operationsAction();
	}
	return;
}
/**
 * Comment
 */
public void operationsList_MouseExited(java.awt.event.MouseEvent mouseEvent) {
	getoperationsList().clearSelection();
	return;
}
/**
 * Comment
 */
public void operationsList_MouseMoved(java.awt.event.MouseEvent mouseEvent) {
	int index=getoperationsList().locationToIndex(mouseEvent.getPoint());
	if (index>-1) getoperationsList().setSelectedIndex(getoperationsList().locationToIndex(mouseEvent.getPoint()));
	else getoperationsList().clearSelection();
	return;
}
/**
 * Insert the method's description here.
 * Creation date: (3.2.2002 15:32:11)
 */
void setAttributesModel(javax.swing.ListModel model) {
	getattributesList().setModel(model);
}
/**
 * Insert the method's description here.
 * Creation date: (3.2.2002 15:32:11)
 */
void setDeviceText(String text) {
	getObjectNameLabel().setText(text);
}
/**
 * Insert the method's description here.
 * Creation date: (3.2.2002 15:32:11)
 */
void setOperationsModel(javax.swing.ListModel model) {
	getoperationsList().setModel(model);
}
}
