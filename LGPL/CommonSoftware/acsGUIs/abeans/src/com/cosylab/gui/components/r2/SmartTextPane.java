package com.cosylab.gui.components.r2;

import javax.swing.*;
/**
 * Insert the type's description here.
 * Creation date: (15.2.2002 17:42:12)
 * @author: 
 */
public class SmartTextPane extends JTextPane {
	private JFileChooser fileChooser=null;
	private JPopupMenu popup=null;
	private boolean auto_cut=true;
	private boolean auto_scroll=true;
	private int maxLines=1000;
	private boolean popupInitialized=false;
	
/**
 * SmartTextPane constructor comment.
 */
public SmartTextPane() {
	super();
	initialize();
}
/**
 * SmartTextPane constructor comment.
 * @param doc javax.swing.text.StyledDocument
 */
public SmartTextPane(javax.swing.text.StyledDocument doc) {
	super(doc);
	initialize();
}
/**
 * Creation date: (23.10.2001 21:38:33)
 * @param text java.lang.String
 */
public void append(String text) {
    try {
	    if (getText().length()==0) setText(text);
        else super.getDocument().insertString(getText().length(), text, null);
        textInserted();
        return;
    } catch (javax.swing.text.BadLocationException e) {
    }
}
/**
 * Insert the method's description here.
 * Creation date: (15.2.2002 17:43:56)
 * @return javax.swing.JFileChooser
 */
public javax.swing.JFileChooser getFileChooser() {
	if (fileChooser==null) fileChooser=new JFileChooser();
	return fileChooser;
}
/**
 * Insert the method's description here.
 * Creation date: (15.2.2002 17:45:08)
 * @return int
 */
public int getMaxLines() {
	return maxLines;
}
/**
 * Insert the method's description here.
 * Creation date: (18.2.2002 17:58:02)
 * @return javax.swing.JPopupMenu
 */
public javax.swing.JPopupMenu getPopup() {
	if (popup==null){
		popup=new JPopupMenu("Smart text pane");
		JMenuItem saveItem=new JMenuItem("Save");
		saveItem.addActionListener(new java.awt.event.ActionListener(){
			public void actionPerformed(java.awt.event.ActionEvent e){
				saveTextToFile();
			}		
		});
		popup.add(saveItem);
	}
	return popup;
}
/**
 * Insert the method's description here.
 * Creation date: (18.2.2002 17:53:54)
 */
private void initialize() {
	addMouseListener(new java.awt.event.MouseAdapter(){
		public void mouseClicked(java.awt.event.MouseEvent e){
			showPopup(e);
		}
		
	});
	setEditable(false);
}
/**
 * Insert the method's description here.
 * Creation date: (15.2.2002 17:45:08)
 * @return boolean
 */
public boolean isAuto_cut() {
	return auto_cut;
}
/**
 * Insert the method's description here.
 * Creation date: (15.2.2002 17:45:08)
 * @return boolean
 */
public boolean isAuto_scroll() {
	return auto_scroll;
}
/**
 * Insert the method's description here.
 * Creation date: (15.2.2002 17:42:30)
 * @param args java.lang.String[]
 */
public static void main(String[] args) {
  JPanel panel=new JPanel(new java.awt.CardLayout(2,2));
  SmartPanel p=new SmartPanel();
  p.setLayout(new java.awt.CardLayout(2,2));
  SmartTextPane pane=new SmartTextPane();
  pane.setEditable(false);
  pane.append("Krneki\nkaj pa zdej?");
  p.add(pane,"Smart text pane");
  JFrame frame=new JFrame();
  frame.addWindowListener(new java.awt.event.WindowAdapter(){
	  public void windowClosing(java.awt.event.WindowEvent e){
		  System.exit(0);
	  }
  });
  panel.add(p,"panel");
  frame.setContentPane(panel);
  frame.show();	
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
 * Insert the method's description here.
 * Creation date: (15.2.2002 17:45:08)
 * @param newAuto_cut boolean
 */
public void setAuto_cut(boolean newAuto_cut) {
	auto_cut = newAuto_cut;
}
/**
 * Insert the method's description here.
 * Creation date: (15.2.2002 17:45:08)
 * @param newAuto_scroll boolean
 */
public void setAuto_scroll(boolean newAuto_scroll) {
	auto_scroll = newAuto_scroll;
}
/**
 * Insert the method's description here.
 * Creation date: (15.2.2002 17:43:56)
 * @param newFileChooser javax.swing.JFileChooser
 */
public void setFileChooser(javax.swing.JFileChooser newFileChooser) {
	fileChooser = newFileChooser;
}
/**
 * Insert the method's description here.
 * Creation date: (15.2.2002 17:45:08)
 * @param newMaxLines int
 */
public void setMaxLines(int newMaxLines) {
	maxLines = newMaxLines;
}
/**
 * Insert the method's description here.
 * Creation date: (18.2.2002 17:58:02)
 * @param newPopup javax.swing.JPopupMenu
 */
public void setPopup(javax.swing.JPopupMenu newPopup) {
	popup = newPopup;
}
/**
 * Comment
 */
private synchronized void showPopup(java.awt.event.MouseEvent mouseEvent) {
    if (mouseEvent.getModifiers() == java.awt.event.MouseEvent.META_MASK) {
        if (getPopup().getComponents().length<2) {
	        java.awt.Component p = getParent();
	        if (p instanceof JViewport) p=p.getParent().getParent();
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
	if (auto_cut && (DataFormatter.getLineCount(getText())>maxLines)) setText(DataFormatter.cropFromEnd(getText(),maxLines));
	if (auto_scroll) setCaretPosition(getDocument().getLength());
}
}
