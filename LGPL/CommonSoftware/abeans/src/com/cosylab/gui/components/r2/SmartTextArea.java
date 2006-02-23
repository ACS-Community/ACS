package com.cosylab.gui.components.r2;

/**
 * Creation date: (23.10.2001 19:46:12)
 * @author: 
 */
public class SmartTextArea extends javax.swing.JTextArea {
	private int max_no_lines=500;
	private boolean auto_scroll=true;
	private boolean auto_cut=true;
	private javax.swing.JFileChooser fileChooser=null;
	IvjEventHandler ivjEventHandler = new IvjEventHandler();
	private boolean popupInitialized=false;

class IvjEventHandler implements java.awt.event.ActionListener, java.awt.event.MouseListener {
		public void actionPerformed(java.awt.event.ActionEvent e) {
			if (e.getSource() == SmartTextArea.this.getJMenuItem1()) 
				connEtoC2(e);
		};
		public void mouseClicked(java.awt.event.MouseEvent e) {
			if (e.getSource() == SmartTextArea.this) 
				connEtoC1(e);
		};
		public void mouseEntered(java.awt.event.MouseEvent e) {};
		public void mouseExited(java.awt.event.MouseEvent e) {};
		public void mousePressed(java.awt.event.MouseEvent e) {};
		public void mouseReleased(java.awt.event.MouseEvent e) {};
	};
	private javax.swing.JMenuItem ivjJMenuItem1 = null;
	private javax.swing.JPopupMenu ivjJPopupMenu1 = null;
/**
 * SmartTextArea constructor comment.
 */
public SmartTextArea() {
	super();
	initialize();
}
/**
 * SmartTextArea constructor comment.
 * @param rows int
 * @param columns int
 */
public SmartTextArea(int rows, int columns) {
	super(rows, columns);
}
/**
 * SmartTextArea constructor comment.
 * @param text java.lang.String
 */
public SmartTextArea(String text) {
	super(text);
}
/**
 * SmartTextArea constructor comment.
 * @param text java.lang.String
 * @param rows int
 * @param columns int
 */
public SmartTextArea(String text, int rows, int columns) {
	super(text, rows, columns);
}
/**
 * SmartTextArea constructor comment.
 * @param doc javax.swing.text.Document
 */
public SmartTextArea(javax.swing.text.Document doc) {
	super(doc);
}
/**
 * SmartTextArea constructor comment.
 * @param doc javax.swing.text.Document
 * @param text java.lang.String
 * @param rows int
 * @param columns int
 */
public SmartTextArea(javax.swing.text.Document doc, String text, int rows, int columns) {
	super(doc, text, rows, columns);
}
/**
 * Creation date: (23.10.2001 21:38:33)
 * @param text java.lang.String
 */
public void append(String text) {
  super.append(text);
  textInserted();	
}
/**
 * connEtoC1:  (SmartTextArea.mouse.mouseClicked(java.awt.event.MouseEvent) --> SmartTextArea.showPopup(Ljava.awt.event.MouseEvent;)V)
 * @param arg1 java.awt.event.MouseEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC1(java.awt.event.MouseEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		this.showPopup(arg1);
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoC2:  (JMenuItem1.action.actionPerformed(java.awt.event.ActionEvent) --> SmartTextArea.saveTextToFile()V)
 * @param arg1 java.awt.event.ActionEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoC2(java.awt.event.ActionEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		this.saveTextToFile();
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
 * Creation date: (1.2.2002 16:35:09)
 * @return javax.swing.JFileChooser
 */
public javax.swing.JFileChooser getFileChooser() {
	if (fileChooser==null) fileChooser=new javax.swing.JFileChooser();
	return fileChooser;
}
/**
 * Return the JMenuItem1 property value.
 * @return javax.swing.JMenuItem
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JMenuItem getJMenuItem1() {
	if (ivjJMenuItem1 == null) {
		try {
			ivjJMenuItem1 = new javax.swing.JMenuItem();
			ivjJMenuItem1.setName("JMenuItem1");
			ivjJMenuItem1.setText("Save...");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJMenuItem1;
}
/**
 * Creation date: (23.10.2001 19:58:56)
 * @return int
 */
public int getMaxLines() {
	return max_no_lines;
}

public javax.swing.JPopupMenu getPopup() {
	if (ivjJPopupMenu1 == null) {
		try {
			ivjJPopupMenu1 = new javax.swing.JPopupMenu();
			ivjJPopupMenu1.setName("JPopupMenu1");
			ivjJPopupMenu1.add(getJMenuItem1());
		} catch (java.lang.Throwable ivjExc) {
			handleException(ivjExc);
		}
	}
	return ivjJPopupMenu1;
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
	this.addMouseListener(ivjEventHandler);
	getJMenuItem1().addActionListener(ivjEventHandler);
}
/**
 * Initialize the class.
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void initialize() {
	try {
		// user code begin {1}
		setDisabledTextColor(getForeground());
		// user code end
		setName("SmartTextArea");
		setSize(160, 120);
		setEditable(true);
		initConnections();
	} catch (java.lang.Throwable ivjExc) {
		handleException(ivjExc);
	}
	// user code begin {2}
	// user code end
}
/**
 * Creation date: (23.10.2001 21:41:28)
 * @param text java.lang.String
 * @param pos int
 */
public void insert(String text, int pos) {
	super.insert(text, pos);
	textInserted();	
}
/**
 * Creation date: (23.10.2001 19:58:56)
 * @return boolean
 */
public boolean isAuto_cut() {
	return auto_cut;
}
/**
 * Creation date: (23.10.2001 19:58:56)
 * @return boolean
 */
public boolean isAuto_scroll() {
	return auto_scroll;
}
/**
 * main entrypoint - starts the part when it is run as an application
 * @param args java.lang.String[]
 */
public static void main(java.lang.String[] args) {
	try {
		javax.swing.JFrame frame = new javax.swing.JFrame();
		SmartTextArea aSmartTextArea;
		aSmartTextArea = new SmartTextArea();
		frame.setContentPane(aSmartTextArea);
		frame.setSize(aSmartTextArea.getSize());
		frame.addWindowListener(new java.awt.event.WindowAdapter() {
			public void windowClosing(java.awt.event.WindowEvent e) {
				System.exit(0);
			};
		});
		frame.show();
		java.awt.Insets insets = frame.getInsets();
		frame.setSize(frame.getWidth() + insets.left + insets.right, frame.getHeight() + insets.top + insets.bottom);
		frame.setVisible(true);
		aSmartTextArea.append("TOLE PROBAVAMO\n");
		aSmartTextArea.append("TOLE PROBAVAMO\n");
		aSmartTextArea.append("TOLE PROBAVAMO\n");
	} catch (Throwable exception) {
		System.err.println("Exception occurred in main() of si.ijs.acs.objectexplorer.SmartTextArea");
		exception.printStackTrace(System.out);
	}
}
/**
 * Comment
 */
public void saveTextToFile() {
	getFileChooser().showSaveDialog(this);
	java.io.File file=getFileChooser().getSelectedFile();
	if (file==null) return;
	try{
	if (!file.getParentFile().exists()) {
		file.getParentFile().mkdirs();
	}
	java.io.BufferedWriter bw=new java.io.BufferedWriter(new java.io.FileWriter(file));
	String[] lines=DataFormatter.splitStringByLines(getText());
	for (int i = 0; i < lines.length; i++){
		bw.write(lines[i]);
		bw.newLine();
		System.out.println("written "+lines[i]);
	}
	bw.close();
	System.out.println("written to:"+file+" text:"+getText());
	}
	catch (java.io.IOException e){
		System.out.println("exception while writing "+e+" "+file+" "+getText());
		e.printStackTrace();
	}
	return;
}
/**
 * Creation date: (23.10.2001 19:58:56)
 * @param newAuto_cut boolean
 */
public void setAuto_cut(boolean newAuto_cut) {
	auto_cut = newAuto_cut;
}
/**
 * Creation date: (23.10.2001 19:58:56)
 * @param newAuto_scroll boolean
 */
public void setAuto_scroll(boolean newAuto_scroll) {
	auto_scroll = newAuto_scroll;
}
/**
 * Insert the method's description here.
 * Creation date: (1.2.2002 16:35:09)
 * @param newFileChooser javax.swing.JFileChooser
 */
public void setFileChooser(javax.swing.JFileChooser newFileChooser) {
	fileChooser = newFileChooser;
}
/**
 * Creation date: (23.10.2001 19:58:56)
 * @param newMax_no_lines int
 */
public void setMaxLines(int newMax_no_lines) {
	max_no_lines = newMax_no_lines;
}
/**
 * Comment
 */
public synchronized void showPopup(java.awt.event.MouseEvent mouseEvent) {
    if (mouseEvent.getModifiers() == java.awt.event.MouseEvent.META_MASK) {
        if (getPopup().getComponents().length<2) {
            java.awt.Component p = getParent().getParent().getParent();
            if (p instanceof SmartPanel) {
                javax.swing.JMenuItem[] array=((SmartPanel) p).getNewMenuItems();
                for (int i = 0; i < array.length; i++){
            		getPopup().add(array[i]);
               }
            }
            popupInitialized=true;
        }
        getPopup().show(this, mouseEvent.getX(), mouseEvent.getY());
    }
}
/**
 * Method should be called when text was inserted
 */
public void textInserted() {
  try {
    if (auto_cut && (getLineCount()>max_no_lines)) replaceRange("",0,getLineStartOffset(getLineCount() - max_no_lines + 1));
	if (auto_scroll) setCaretPosition(getDocument().getLength());
  }
  catch (Exception e) {
	  handleException(e);
  }
}
}
