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
import java.lang.reflect.Array;

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
 */
private Object arrayConvert(String str, Class type) {
  if (type.isArray()) {
	String[] values=readRows(str);
	int length=values.length;

	Object retVal=Array.newInstance(type.getComponentType(),length);
	for(int i=0;i<length;i++) {
 	    Array.set(retVal,i,stringConvert(values[i],type.getComponentType()));
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
private void generateParameterFields(Class[] types, String[] names, boolean[] mask) {
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
private void generateSingleField(Class type, String name) {
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
						generateSingleField(type.getComponentType(), "value " + (i + 1));
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

			java.awt.GridBagConstraints constraintsErrorLabel = new java.awt.GridBagConstraints();
			constraintsErrorLabel.gridx = 0; constraintsErrorLabel.gridy = 1;
			constraintsErrorLabel.gridwidth = 2;
			constraintsErrorLabel.fill = java.awt.GridBagConstraints.HORIZONTAL;
			constraintsErrorLabel.insets = new java.awt.Insets(4, 4, 4, 4);
			getJPanel2().add(getErrorLabel(), constraintsErrorLabel);
			// user code begin {1}

			// do the conversion
			Class[] types = op.getParameterTypes();
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
		Class[] types = op.getParameterTypes();
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
 */
private Object[] readParameters(Class[] types, boolean[] mask, Object[] pFields) {
		if ((types != null) && (mask != null)) {
			Object[] params = new Object[types.length];
			for (int j = 0; j < types.length; j++) {
				if (mask[j]) {
				  params[j]=readSingleParameter(types[j]);	
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
private Object readReadableType(Class type) {
   try{
	   if (!type.isInterface()) {
			java.lang.reflect.Constructor[] constructors = type.getConstructors();
			if (constructors.length > 0) {
				Class[] constTypes = constructors[0].getParameterTypes();
				Class[] tempConstTypes = null;
				java.lang.reflect.Constructor myConst = constructors[0];
				for (int k = 1; k < constructors.length; k++) {
					tempConstTypes = constructors[k].getParameterTypes();
					if (tempConstTypes.length > constTypes.length) {
						constTypes = tempConstTypes;
						myConst = constructors[k];
					}
				}
				boolean[] mask = new boolean[constTypes.length];
				for (int j = 0; j < constTypes.length; j++) {
					mask[j] = true;
				}
				try{
				  Object myObj = myConst.newInstance(readParameters(constTypes, mask, parameterFields));
 				  return myObj;
				}
				catch (InstantiationException t){
				  return null;
				}
				catch (java.lang.reflect.InvocationTargetException e) {
				  return null;
				}
			} else
				
				if (constructors.length == 0) {
					java.lang.reflect.Field f = null;
					try
					{
						f = type.getField((String)((JComboBox)parameterFields[index]).getSelectedItem());
						index++;
					} catch (Exception e)
					{
					}
					if (f == null || f.getType() != type || !java.lang.reflect.Modifier.isStatic(f.getModifiers())) return null;
					return f.get(null);
				} else return null;
		}
		else return null;
   }
   catch (IllegalAccessException e) {
	  return null;
   }	
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
 */
private Object readSingleParameter(Class type) {

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
				param = stringConvert(((JTextField) parameterFields[index]).getText(), type);
				index = index + 1;
			} else
				if (type.isArray()) {
					if ((type.getComponentType().isPrimitive()) || ((type.getName().indexOf("java.lang.") > -1) && (type.getName().indexOf("java.lang.Object") == -1))) {
						param = arrayConvert(((JTextPane)parameterFields[index]).getText(), type);
						index = index + 1;
					} else {
						param = java.lang.reflect.Array.newInstance(type.getComponentType(),3);
						for (int i=0;i<2;i++){
						  java.lang.reflect.Array.set(param,i,readSingleParameter(type.getComponentType()));
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
 */
public static  Object stringConvert(String str,Class type) {
	Object myParam=new Object();
	if (type.toString().equals("int")) {
	  myParam=new Integer(str);
	}
	else if (type.toString().equals("double")) {
	  myParam=new Double(str);
	}
	else if (type.toString().equals("long")) {
	  myParam=new Long(str);
	}
	else if (type.toString().equals("short")) {
	  myParam=new Short(str);
	}
	else if (type.toString().equals("byte")) {
	  myParam=new Byte(str);
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
private void unpackReadableType(Class type) {
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
		java.lang.reflect.Constructor[] constructors = type.getConstructors();

		if ((constructors != null) && (constructors.length > 0)) {
			Class[] constTypes = constructors[0].getParameterTypes();
			Class[] tempConstTypes = null;
			for (int k = 1; k < constructors.length; k++) {
				tempConstTypes = constructors[k].getParameterTypes();
				if (tempConstTypes.length > constTypes.length)
					constTypes = tempConstTypes;
			}
			boolean[] mask = null;
			String[] names = null;

			if (constTypes.length > 0) {
				mask = new boolean[constTypes.length];
				names = new String[constTypes.length];
				notifier.reportDebug(
					"CallMethodDialog::unpackReadableType",
					"Constructor: " + constTypes.length + " params");
				for (int j = 0; j < constTypes.length; j++) {
					names[j] = "Constructor parameter " + (j + 1);
					mask[j] = true;
				}
			} else {
				java.lang.reflect.Method[] methods = type.getMethods();
				java.util.ArrayList list = new java.util.ArrayList();
				java.util.ArrayList listNames = new java.util.ArrayList();
				for (int i = 0; i < methods.length; i++) {
					if ((methods[i].getName().indexOf("get") == 0)
						&& (methods[i].getParameterTypes().length == 1)) {
						list.add(methods[i].getParameterTypes()[0]);
						listNames.add(methods[i].getName());
					}
				}
				mask = new boolean[list.size()];
				for (int k = 0; k < list.size(); k++) {
					mask[k] = true;
				}
				names = new String[list.size()];
				constTypes = new Class[list.size()];
				list.toArray(constTypes);
				listNames.toArray(names);
			}
			height = height + 40;
			generateParameterFields(constTypes, names, mask);
		} else
			if (constructors.length == 0) {
				java.lang.reflect.Field[] fields = type.getDeclaredFields();
				java.util.ArrayList fieldNames = new java.util.ArrayList();
				for (int j = 0; j < fields.length; j++) {
					if (fields[j].getType() == type)
						fieldNames.add(fields[j].getName());
				}
				if (fieldNames.size() > 0) {
					String[] names = new String[fieldNames.size()];
					fieldNames.toArray(names);
					javax.swing.JComboBox tempCombo = getParameterCombo(index, names);
					getJPanel1().add(tempCombo, con);
					index++;
				}
			}

	}
	height = height + 40;
}
}
