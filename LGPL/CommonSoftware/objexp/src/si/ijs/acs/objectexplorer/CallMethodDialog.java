package si.ijs.acs.objectexplorer;

/**
 * CallMethodDialog enables user to input the parameters needed to invoke a
 * method on the selected object.
 *
 * Given an instance of Operation, it generates appropriate input fields and 
 * allows the user to input data into them
 *
 * Creation date: (11/2/00 8:17:28 PM)
 * @author: Miha Kadunc
 */
import javax.swing.*;
import si.ijs.acs.objectexplorer.engine.*;

import java.awt.GridBagConstraints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.lang.reflect.Array;
import java.text.ParseException;

public class CallMethodDialog extends JDialog {
	private JLabel ivjJLabel1 = null;
	private JLabel ivjJLabel2 = null;
	private JLabel ivjJLabel3 = null;
	private Operation op=null;
	private int index=0;
	private JPanel ivjJPanel1 = null;
	private Object[] parameterFields=new Object[20];
	NotificationBean notifier=null;
	private JButton ivjJButton1 = null;
	IvjEventHandler ivjEventHandler = new IvjEventHandler();
	private JPanel ivjJPanel2 = null;
	private OperationInvocator invocator=null;
	private JButton ivjJButton2 = null;
	private int height=0;
	private int gridy=0;
	private JLabel ivjErrorLabel = null;
	private Converter converter = null;

class IvjEventHandler implements java.awt.event.ActionListener {
		public void actionPerformed(java.awt.event.ActionEvent e) {
			if (e.getSource() == CallMethodDialog.this.getJButton1()) 
				connEtoC1();
			if (e.getSource() == CallMethodDialog.this.getJButton2()) 
				connEtoM1(e);
		};
	};
/**
 * CallMethodDialog constructor comment.
 */
public CallMethodDialog() {
	super();
	initialize();
}
/**
 * CallMethodDialog constructor comment.
 * @param title java.lang.String
 */
public CallMethodDialog(Operation op, JFrame frame,boolean modal,NotificationBean notifier,OperationInvocator invocator) {
	super(frame,modal);
	int x=frame.getX()+frame.getWidth()/2;
	int y=frame.getY()+frame.getHeight()/2;
	if (op!=null) this.op=op;
	else throw(new NullPointerException("Operation in CallMethodDialog"));
	
	// do the conversion 
	this.converter = ObjectExplorer.getConverter(op.getIntrospectable());
	if (this.converter != null && !this.converter.acceptInverseConvert(op.getName()))
		this.converter = null;
	
	this.notifier=notifier;
	this.invocator=invocator;
	initialize();
	setSize(600, height+110);
	setLocation(x-300, y-height/2-55);
	setTitle(op.getIntrospectable().getName()+" "+op.toString());
}
/**
 * Insert the method's description here.
 * Creation date: (11/14/00 8:29:22 PM)
 * @throws ParseException 
 */
private Object arrayConvert(String str, Class type) throws ParseException {
  if (type.isArray()) {
	String[] values=readRows(str);
	int length=values.length;

	Object retVal=Array.newInstance(type.getComponentType(),length);
	for(int i=0;i<length;i++) {
		try
		{
			Array.set(retVal,i,stringConvert(values[i],type.getComponentType()));
		}
		catch(ParseException parseExc)
		{
			throw parseExc;
		}
	}
	return retVal;
  }
  return null;
}
/**
 * connEtoC1:  (JButton1.action. --> CallMethodDialog.invokeClicked()V)
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC1() {
	try {
		// user code begin {1}
		// user code end
		this.invokeClicked();
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoM1:  (JButton2.action.actionPerformed(java.awt.event.ActionEvent) --> CallMethodDialog.dispose()V)
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
 * Insert the method's description here.
 * Creation date: (11/14/00 4:50:59 PM)
 */
private void generateParameterFields(DataType[] types, String[] names, boolean[] mask) {
	if ((types != null) && (names != null) && (mask != null)) {
		for (int j = 0; j < types.length; j++) {
			if (mask[j]) {
		       generateSingleField(types[j],names[j]);
			}
		}
	}
}
/**
 * Insert the method's description here.
 * Creation date: (11/20/00 5:08:19 PM)
 */
private void generateSingleField(DataType type, String name) {
	gridy++;
	java.awt.GridBagConstraints con =
		new java.awt.GridBagConstraints(
			2,
			gridy,
			1,
			1,
			1,
			1,
			java.awt.GridBagConstraints.CENTER,
			java.awt.GridBagConstraints.HORIZONTAL,
			new java.awt.Insets(2, 6, 2, 8),
			0,
			0);
	java.awt.GridBagConstraints con1 =
		new java.awt.GridBagConstraints(
			0,
			gridy,
			1,
			1,
			0,
			1,
			java.awt.GridBagConstraints.CENTER,
			java.awt.GridBagConstraints.BOTH,
			new java.awt.Insets(2, 8, 2, 6),
			0,
			0);
	java.awt.GridBagConstraints con2 =
		new java.awt.GridBagConstraints(
			1,
			gridy,
			1,
			1,
			0,
			1,
			java.awt.GridBagConstraints.CENTER,
			java.awt.GridBagConstraints.BOTH,
			new java.awt.Insets(2, 6, 2, 6),
			0,
			0);
	getJPanel1().add(getNameLabel(index, name), con1);
	String typeName = type.toString();
	JComponent tempField = null;
	if ((typeName.equals("boolean"))
		|| (typeName.equals("class java.lang.Boolean"))) {
		getJPanel1().add(getTypeLabel(index, typeName), con2);
		tempField = getParameterBox(index);
	} else
		// msekoran: added !type.isArray condition - big decimal/string array case
		if ((type.isPrimitive())
			|| ((typeName.indexOf("java.lang.") > -1)
				&& (typeName.indexOf("java.lang.Object") == -1) && !type.isArray())) {
			getJPanel1().add(getTypeLabel(index, typeName), con2);
			tempField = getParameterField(index);
		} else
			if (type.isArray()) {
				typeName = "Array of " + type.getComponentType().toString();
				getJPanel1().add(getTypeLabel(index, typeName), con2);
				if ((type.getComponentType().isPrimitive())
					|| ((typeName.indexOf("java.lang.") > -1)
						&& (typeName.indexOf("java.lang.Object") == -1))) {
					tempField = getParameterArea(index);
					JScrollPane spane = new JScrollPane();
					spane.setAutoscrolls(true);
					spane.setViewportView(tempField);
					spane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
					spane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
					spane.setEnabled(true);
					spane.setVisible(true);
					con.fill = GridBagConstraints.BOTH;
					con.weighty = 1;
					//THIS IS WHERE WE ADD TO PANEL
					getJPanel1().add(spane, con);
					index++;
					return;
				} else {
					height = height + 30;
					for (int i = 0; i < 2; i++) {
						DataType comp = type.getComponentType();
						comp.setElement(type.getElement());
						generateSingleField(comp, "value " + (i + 1));
					}
					return;
				}
			} else {
				getJPanel1().add(getTypeLabel(index, typeName), con2);
				unpackReadableType(type);
				return;
			}
	//THIS IS WHERE WE ADD TO PANEL
	getJPanel1().add(tempField, con);
	parameterFields[index++] = tempField;
}
/**
 * Return the ErrorLabel property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getErrorLabel() {
	if (ivjErrorLabel == null) {
		try {
			ivjErrorLabel = new javax.swing.JLabel();
			ivjErrorLabel.setName("ErrorLabel");
			ivjErrorLabel.setText("");
			ivjErrorLabel.setVisible(false);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjErrorLabel;
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
			ivjJButton1.setText("Invoke");
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
 * Return the JButton2 property value.
 * @return javax.swing.JButton
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JButton getJButton2() {
	if (ivjJButton2 == null) {
		try {
			ivjJButton2 = new javax.swing.JButton();
			ivjJButton2.setName("JButton2");
			ivjJButton2.setText("Cancel");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJButton2;
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
			ivjJLabel1.setText("Name");
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
 * Return the JLabel2 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel2() {
	if (ivjJLabel2 == null) {
		try {
			ivjJLabel2 = new javax.swing.JLabel();
			ivjJLabel2.setName("JLabel2");
			ivjJLabel2.setText("Type");
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
			ivjJLabel3.setText("Value");
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
			constraintsJLabel1.gridx = 0; constraintsJLabel1.gridy = 0;
			constraintsJLabel1.fill = java.awt.GridBagConstraints.HORIZONTAL;
			constraintsJLabel1.insets = new java.awt.Insets(4, 10, 4, 5);
			getJPanel1().add(getJLabel1(), constraintsJLabel1);

			java.awt.GridBagConstraints constraintsJLabel2 = new java.awt.GridBagConstraints();
			constraintsJLabel2.gridx = 1; constraintsJLabel2.gridy = 0;
			constraintsJLabel2.fill = java.awt.GridBagConstraints.HORIZONTAL;
			constraintsJLabel2.insets = new java.awt.Insets(4, 5, 4, 5);
			getJPanel1().add(getJLabel2(), constraintsJLabel2);

			java.awt.GridBagConstraints constraintsJLabel3 = new java.awt.GridBagConstraints();
			constraintsJLabel3.gridx = 2; constraintsJLabel3.gridy = 0;
			constraintsJLabel3.fill = java.awt.GridBagConstraints.HORIZONTAL;
			constraintsJLabel3.weightx = 1.0;
			constraintsJLabel3.insets = new java.awt.Insets(4, 5, 4, 10);
			getJPanel1().add(getJLabel3(), constraintsJLabel3);
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
 * Return the JPanel1 property value.
 * @return javax.swing.JPanel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JPanel getJPanel2() {
	if (ivjJPanel2 == null) {
		try {
			ivjJPanel2 = new javax.swing.JPanel();
			ivjJPanel2.setName("JPanel2");
			ivjJPanel2.setLayout(new java.awt.GridBagLayout());

			java.awt.GridBagConstraints constraintsJButton1 = new java.awt.GridBagConstraints();
			constraintsJButton1.gridx = 0; constraintsJButton1.gridy = 2;
			constraintsJButton1.anchor = java.awt.GridBagConstraints.EAST;
			constraintsJButton1.weightx = 1.0;
			constraintsJButton1.insets = new java.awt.Insets(8, 0, 8, 5);
			getJPanel2().add(getJButton1(), constraintsJButton1);

			java.awt.GridBagConstraints constraintsJButton2 = new java.awt.GridBagConstraints();
			constraintsJButton2.gridx = 1; constraintsJButton2.gridy = 2;
			constraintsJButton2.anchor = java.awt.GridBagConstraints.WEST;
			constraintsJButton2.weightx = 1.0;
			constraintsJButton2.insets = new java.awt.Insets(8, 5, 8, 0);
			getJPanel2().add(getJButton2(), constraintsJButton2);

			java.awt.GridBagConstraints constraintsJPanel1 = new java.awt.GridBagConstraints();
			constraintsJPanel1.gridx = 0; constraintsJPanel1.gridy = 0;
			constraintsJPanel1.gridwidth = 2;
			constraintsJPanel1.fill = java.awt.GridBagConstraints.BOTH;
			constraintsJPanel1.weightx = 1.0;
			constraintsJPanel1.weighty = 1.0;
			constraintsJPanel1.insets = new java.awt.Insets(4, 4, 4, 4);
			getJPanel2().add(getJPanel1(), constraintsJPanel1);

			java.awt.GridBagConstraints constraintsJPanelC = new java.awt.GridBagConstraints();
			constraintsJPanelC.gridx = 0; constraintsJPanelC.gridy = 0;
			constraintsJPanelC.gridwidth = 2;
			constraintsJPanelC.fill = java.awt.GridBagConstraints.BOTH;
			constraintsJPanelC.weightx = 1.0;
			constraintsJPanelC.weighty = 1.0;
			constraintsJPanelC.insets = new java.awt.Insets(4, 4, 4, 4);
			getJPanel2().add(getConfirmationPanel(), constraintsJPanelC);

			java.awt.GridBagConstraints constraintsErrorLabel = new java.awt.GridBagConstraints();
			constraintsErrorLabel.gridx = 0; constraintsErrorLabel.gridy = 1;
			constraintsErrorLabel.gridwidth = 2;
			constraintsErrorLabel.fill = java.awt.GridBagConstraints.HORIZONTAL;
			constraintsErrorLabel.insets = new java.awt.Insets(4, 4, 4, 4);
			getJPanel2().add(getErrorLabel(), constraintsErrorLabel);
			// user code begin {1}

			// do the conversion
			DataType[] types = op.getParameterTypes();
			
			// yatagai : as a confirmation dialog
			boolean hasParams = (types.length > 0);
			getJPanel1().setVisible(hasParams);
			getConfirmationPanel().setVisible(!hasParams);
			
			if (converter != null)
				types = converter.getInverseConvertParameterTypes(op.getName(), types);
			
			generateParameterFields(types,op.getParameterNames(),op.getMask());
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJPanel2;
}

private JPanel getConfirmationPanel() {
	JPanel panel = new JPanel();
	JLabel label = new JLabel("Are you sure to invoke \"" + op.toString() + "\" ?");
	JCheckBox cb = new JCheckBox("Never ask me again");
	cb.addActionListener(new ActionListener(){
		@Override
		public void actionPerformed(ActionEvent e) {
			boolean selected = ((JCheckBox)e.getSource()).isSelected();
			((ListsHandlerBean)invocator).setConfirmed(selected);
		}
	});
	panel.add(label);
	panel.add(cb);
	return panel;
}
/**
 * Insert the method's description here.
 * Creation date: (11/3/00 1:12:23 AM)
 */
private JLabel getNameLabel(int n,String inName) {
	JLabel tempLabel= new javax.swing.JLabel();
		try {
			tempLabel.setName("OENameLabel"+n);
			tempLabel.setText(inName);
			tempLabel.setFont(new java.awt.Font("dialog", 1, 12));
			tempLabel.setForeground(getJPanel1().getForeground());
		} catch (java.lang.Throwable ivjExc) {
			handleException(ivjExc);
		}
	return tempLabel;
}
/**
 * Insert the method's description here.
 * Creation date: (11/14/00 6:06:42 PM)
 */
private javax.swing.JTextPane getParameterArea(int n) {
  try {
	if (parameterFields.length>=n) {
	  if (parameterFields[n]==null) {
	    JTextPane tempTextArea = new JTextPane();
		tempTextArea.setName("TextArea"+n);
	    tempTextArea.setEditable(true);
	    tempTextArea.setEnabled(true);
	    tempTextArea.setRequestFocusEnabled(true);
	    tempTextArea.setVisible(true);
		parameterFields[n]=tempTextArea;
		height=height+7*25;
	  }
	  return ((javax.swing.JTextPane)parameterFields[n]);
	}
	return null;
  }
  catch (java.lang.Throwable ivjExc) {
	handleException(ivjExc);
	return null;
  } 
}
/**
 * Insert the method's description here.
 * Creation date: (11/14/00 6:11:38 PM)
 */
private javax.swing.JCheckBox getParameterBox(int n) {
	try {
		if (parameterFields.length > n) {
			if (parameterFields[n] == null) {
				JCheckBox tempCheckBox = new JCheckBox();
				tempCheckBox.setName("TextArea" + n);
				tempCheckBox.setText("false");
				tempCheckBox.addActionListener(new java.awt.event.ActionListener() {
					public void actionPerformed(java.awt.event.ActionEvent event) {
						JCheckBox box = ((JCheckBox) event.getSource());
						if (box.isSelected()) box.setText("true");
						else box.setText("false");
					}
				});
				tempCheckBox.setForeground(getJPanel1().getForeground());
				tempCheckBox.setBackground(getJPanel1().getBackground());
				parameterFields[n] = tempCheckBox;
	            height=height+30;
			}
			return ((javax.swing.JCheckBox) parameterFields[n]);
		}
		return null;
	} catch (java.lang.Throwable ivjExc) {
		handleException(ivjExc);
		return null;
	}
}
/**
 * Insert the method's description here.
 * Creation date: (11/20/00 5:08:19 PM)
 */
private javax.swing.JComboBox getParameterCombo(int n,String[] values) {
  try {
	if (parameterFields.length>=n) {
	  if (parameterFields[n]==null) {
	    JComboBox tempCombo = new JComboBox(values);
		tempCombo.setName("ComboBox"+n);
		parameterFields[n]=tempCombo;
		height=height+30;
	  }
	  return ((javax.swing.JComboBox)parameterFields[n]);
	}
	return null;
  }
  catch (java.lang.Throwable ivjExc) {
	handleException(ivjExc);
	return null;
  } 
}
/**
 * Insert the method's description here.
 * Creation date: (11/3/00 1:32:36 AM)
 */
private javax.swing.JTextField getParameterField(int n) {
  try {
	if (parameterFields.length>n) {
	  if (parameterFields[n]==null) {
	    JTextField tempTextField = new JTextField();
		tempTextField.setName("TextField"+n);
		tempTextField.setMinimumSize(new java.awt.Dimension(100,20));
		height=height+30;
	    return tempTextField;
	  }
	  else return (JTextField)parameterFields[n];
	}
	return null;
  }
  catch (java.lang.Throwable ivjExc) {
	handleException(ivjExc);
	return null;
  } 
}
/**
 * Insert the method's description here.
 * Creation date: (11/3/00 1:31:05 AM)
 */
private JLabel getTypeLabel(int n,String inType) {
	JLabel tempLabel= new javax.swing.JLabel();
		try {
			tempLabel.setName("OETypeLabel"+n);
			tempLabel.setText(inType);
			tempLabel.setForeground(getJPanel1().getForeground());
		} catch (java.lang.Throwable ivjExc) {
			handleException(ivjExc);
		}
	return tempLabel;
}
/**
 * Called whenever the part throws an exception.
 * @param exception java.lang.Throwable
 */
private void handleException(java.lang.Throwable exception) {
  notifier.reportError("Error in parameters ",exception, true, false);

  //if (exception instanceof Throwable)
  //    ((Throwable)exception).printStackTrace();

  getErrorLabel().setText("Error: "+exception);
  getErrorLabel().setVisible(true);
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
	getJButton2().addActionListener(ivjEventHandler);
}
/**
 * Initialize the class.
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void initialize() {
	try {
		// user code begin {1}
		// user code end
		setName("CallMethodDialog");
		setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
		setSize(581, 142);
		setTitle("");
		setContentPane(getJPanel2());
		initConnections();
	} catch (java.lang.Throwable ivjExc) {
		handleException(ivjExc);
	}
	// user code begin {2}
	// user code end
}
/**
 * Insert the method's description here.
 * Creation date: (11/14/00 7:17:28 PM)
 */
private void invokeClicked() {
	Object[] params;   
	try {

	/*
	System.out.println("ParameterTypes, length = "+op.getParameterTypes().length);
	for (int i = 0; i < op.getParameterTypes().length; i++)
	    System.out.println("\t"+op.getParameterTypes()[i]);
	System.out.println();

	System.out.println("Mask, length = "+op.getMask().length);
	for (int i = 0; i < op.getMask().length; i++)
	    System.out.println("\t"+op.getMask()[i]);
	System.out.println();

	System.out.println("ParameterFields, length = "+parameterFields.length);
	for (int i = 0; i < parameterFields.length && parameterFields[i] != null; i++)
	    System.out.println("\t"+parameterFields[i].getClass().getName());
	System.out.println();
	*/

		// do the conversion
		DataType[] types = op.getParameterTypes();
		if (converter != null)
			types = converter.getInverseConvertParameterTypes(op.getName(), types);

		index = 0;
		params = readParameters(types, op.getMask(), parameterFields);

	} catch (Throwable e) {
		handleException(e);
		return;
	}
	
	// do the conversion
	if (converter != null)
		converter.inverseConvert(op.getName(), params);

	invocator.invokeOperation(op, params);
	dispose();
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
 * Insert the method's description here.
 * Creation date: (11/14/00 7:28:44 PM)
 * @return java.lang.Object[]
 * @throws ParseException 
 */
private Object[] readParameters(DataType[] types, boolean[] mask, Object[] pFields) throws ParseException {
		if ((types != null) && (mask != null)) {
			Object[] params = new Object[types.length];
			for (int j = 0; j < types.length; j++) {
				if (mask[j]) {
					try
					{
						params[j]=readSingleParameter(types[j]);
					}
					catch(ParseException parseExc){
						throw parseExc;
					}
				}
			}
			return params;
		} else
			throw (new NullPointerException("readParameters"));
}
/**
 * Insert the method's description here.
 * Creation date: (11/14/00 8:29:22 PM)
 */
private Object readReadableType(DataType type) {
	if (!type.isInterface()) {
		if(type.getType() == DataStruct.class) {
			DataStruct ds = (DataStruct) type.getElement();
			java.util.Set<String> keys = ds.keySet();
			boolean[] mask = new boolean[ds.size()];
			DataType[] types = new DataType[ds.size()];
			int i = 0;
			for(String key : keys) {
				mask[i] = true;
				types[i] = (DataType) ds.get(key);
				i++;
			}
			Object[] o;
			try{
				o = readParameters(types, mask, parameterFields);
			} catch(ParseException parseExc) {
				return null;
			}
			
			DataStruct ds2 = new DataStruct(ds.id());
			i = 0;
			for(String key : keys) {
				ds2.add(key,o[i]);
				i++;
			}
			return ds2;
		} else if (type.getType() == DataEnum.class) {
			DataEnum de = (DataEnum) type.getElement();
			java.util.Set<String> keys = de.keySet();
			String[] types = new String[de.size()];
			keys.toArray(types);
			DataEnum de2 = new DataEnum(de.id());
			for(int i = 0; i < de.size(); i++) {
				de2.add(i,types[i]);
			}
			de2.set(((JComboBox)parameterFields[index]).getSelectedIndex());
			index++;
			return de2;
		}
		else {
			System.out.println(type.getType().getName());
			return null;
		}
	}
	else return null;
}
/**
 * Insert the method's description here.
 * Creation date: (11/14/00 8:29:22 PM)
 */
private String[] readRows(String str) {

  java.util.ArrayList tempList=new java.util.ArrayList();

  // TODO string w/ spaces not supported
  java.util.StringTokenizer st = new java.util.StringTokenizer(str);
  while (st.hasMoreTokens())
	tempList.add(st.nextToken());
  
  String[] retVal=new String[tempList.size()];
  tempList.toArray(retVal);
  
  return retVal;
}
/**
 * Insert the method's description here.
 * Creation date: (11/22/00 1:42:28 PM)
 * @throws ParseException 
 */
private Object readSingleParameter(DataType type) throws ParseException {

    //System.out.println("Reading param of type at index ("+index+"): " + type.getName());

	Object param = null;
	if (type.toString().equals("boolean")) {
		param = new java.lang.Boolean(((JCheckBox) parameterFields[index]).isSelected());
		index = index + 1;
	} else
		if (type.toString().equals("class java.lang.String")) {
			param = ((JTextField) parameterFields[index]).getText();
			index = index + 1;
		} else
			if (type.isPrimitive()) {
				try
				{
					param = stringConvert(((JTextField) parameterFields[index]).getText(), type.getType());
				}
				catch(ParseException parseExc){
					throw parseExc;
				}
				index = index + 1;
			} else
				if (type.isArray()) {
					if ((type.getComponentType().isPrimitive()) || ((type.getName().indexOf("java.lang.") > -1) && (type.getName().indexOf("java.lang.Object") == -1))) {
						param = arrayConvert(((JTextPane)parameterFields[index]).getText(), type.getType());
						index = index + 1;
					} else {
						param = java.lang.reflect.Array.newInstance(type.getComponentType().getType(),2);
						for (int i=0;i<2;i++){
							DataType comp = type.getComponentType();
							comp.setElement(type.getElement());
							java.lang.reflect.Array.set(param,i,readSingleParameter(comp));
						}
					}
				} else {
					param = readReadableType(type);
				}
	return param;
}
/**
 * Insert the method's description here.
 * Creation date: (11/14/00 8:29:22 PM)
 * @throws ParseException 
 */
public static  Object stringConvert(String str,Class type) throws ParseException {
	Object myParam=new Object();
	if (type.toString().equals("int")) {
		try {
            // If the input is valid then it will decode properly
            myParam = new Integer(Integer.decode(str));
        } catch (NumberFormatException nfExc) {
        	throw new ParseException("Not a valid hex, dec, or octal input", 0);
        }
	}
	else if (type.toString().equals("double")) {
	  myParam=new Double(str);
	}
	else if (type.toString().equals("long")) {
		try {
            // If the input is valid then it will decode properly
            myParam = new Long(Long.decode(str));
        } catch (NumberFormatException nfExc) {
        	throw new ParseException("Not a valid hex, dec, or octal input", 0);
        }
	}
	else if (type.toString().equals("short")) {
		try {
            // If the input is valid then it will decode properly
            myParam = new Short(Short.decode(str));
        } catch (NumberFormatException nfExc) {
        	throw new ParseException("Not a valid hex, dec, or octal input", 0);
        }
	}
	else if (type.toString().equals("byte")) {
		try {
            // If the input is valid then it will decode properly
            myParam = new Byte(Byte.decode(str));
        } catch (NumberFormatException nfExc) {
        	throw new ParseException("Not a valid hex, dec, or octal input", 0);
        }
	}
	else if (type.toString().equals("char")) {
	  myParam=new Character(str.charAt(0));
	}
	else if (type.toString().equals("float")) {
	  myParam=new Float(str);
	}
	else if (type.toString().equals("boolean")) {
	  myParam=new Boolean(str);
	}
	else
		myParam = str;
	return myParam;
}
/**
 * Insert the method's description here.
 * Creation date: (11/14/00 8:29:22 PM)
 */
private void unpackReadableType(DataType type) {
	if (!type.isInterface()) {
		java.awt.GridBagConstraints con =
			new java.awt.GridBagConstraints(
				2,
				gridy,
				1,
				1,
				1,
				1,
				java.awt.GridBagConstraints.CENTER,
				java.awt.GridBagConstraints.HORIZONTAL,
				new java.awt.Insets(2, 6, 2, 8),
				0,
				0);
		if(type.getType() == DataStruct.class) {
			DataStruct ds = (DataStruct) type.getElement();
			java.util.Set<String> keys = ds.keySet();
			int i = 0;
			boolean[] mask = new boolean[ds.size()];
			String[] names = new String[ds.size()];
			DataType[] constTypes = new DataType[ds.size()];
			for(String key : keys) {
				mask[i] = true;
				names[i] = key;
				constTypes[i] = (DataType) ds.get(key);
				i++;
			}
			height = height + 40;
			generateParameterFields(constTypes, names, mask);
		} else if(type.getType() == DataEnum.class) {
			DataEnum de = (DataEnum) type.getElement();
			if (de.size() > 0) {
				String[] types = new String[de.size()];
				de.keySet().toArray(types);
				javax.swing.JComboBox tempCombo = getParameterCombo(index, types);
				getJPanel1().add(tempCombo, con);
				index++;
			}
		} else {
			System.out.println(type.getType().getName());
		}
	}
	height = height + 40;
}
}
