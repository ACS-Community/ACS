/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) COSYLAB - Control System Laboratory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package com.cosylab.cdb.browser;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.table.*;
/**
 * This class is responsible for initialising and placing all Components inside the GUI.
 * 
 * @author      Hernan Raffi
 * @version     2.0
 * 
 */
public class Browser extends JFrame implements ActionListener
{
    /**
     *  The instance of the Browser class used during execution.
     */
    private static Browser instance = null;

    /**
     *  The IOR (Input Output reference) string which represents the
     *  DAL access parameters.
     */
    private static String strIOR = null;

    /**
     *  The message text area (Bottom of the GUI)
     */
    private JTextArea messageTextArea;

    /**
     *  JSlit Pane used to separete the CDB tree and the tabbed Pane window.
     */
    private JSplitPane splitPane;

    /**
     *  The location where the separation occurs.
     */
    private final int dividerLocation = 240;

    /**
     *  Text Field that shows the selected tree path. Located on top of the Browsers GUI.
     */
    private JTextField currentLocation;

    /**
     *  Button used to Save Changes to an XML record (located on top of the tabbed pane).
     */
    private final JButton saveChanges = new JButton(" Save Changes to XML record ");

    /**
     *  Button used to reset an XML record (located on top of the tabbed pane).
     */
    private final JButton resetData = new JButton(" Reset Data ");

    /**
     *  Button used to refresh the CDB tree (located on top of the CDB tree).
     */
    private JButton refreshTree = new JButton("   Refrest CDB Tree   ");

    private final String titleCol1 = "ATTRIBUTE NAME";
    private final String titleCol2 = "ATTRIBUTE VALUE";
    /**
     *  Constructor of the Browser class.
     */
    Browser(){
	super("Configuration Database Browser");

 	if(buttonsEnabled()){
 	    setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
 	}

	addWindowListener(new WindowAdapter(){
		public void windowClosing(WindowEvent e){
		    if( buttonsEnabled()){
			String msg1 = "Warning: Node has been modified.";
			String msg2 = "Please save or reset changes before closing the Browser.";
			CDBDialog dialog1 = new CDBDialog(Browser.getInstance(), msg1,msg2);
		    }
		    else{
			System.exit(0);
		    }
		}
		public void windowClosed(WindowEvent ev) {
		    return;
		}
	    });
    }

    /**
     *  Only one instance of the Browser class can be created at any time
     *  during program execution.
     *  @return always the same instance of the Browser class.
     */
    public static Browser getInstance(){
	if(instance == null){
	    instance = new Browser();
	}
	return instance;
    }
    
    /**
     *  Sets up the Graphical User Interface (GUI) of the Browser.
     *  The GUI is divided into three sections: left side, where the CDB Tree will be visible;
     *  right side for the output (tabbed pane) and the bottom for the message text area.
     *  CDB tree (left) and the tabbed panes (right) are separeted by a JSplitPane object.
     *  On top of the GUI there is a location bar (shows the current selected path in th tree).
     */
    public void createGUI(){
	//  'top panel' that will contain all other panels and components.
	JPanel topPanel = new JPanel(new BorderLayout());
	topPanel.setBorder(BorderFactory.createEmptyBorder(5,5,5,5));
	this.getContentPane().add(topPanel, BorderLayout.CENTER);

	//  Create a message  text area (bottom of the GUI).
	messageTextArea = new JTextArea(10,20);
	messageTextArea.setEditable(false);
	topPanel.add(new JScrollPane(messageTextArea), BorderLayout.SOUTH);

	//  JSplitPane to divide the CDBTree with the tabbedPane.
	splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, true);
	//  Set up right component (area for the tabbed pane).
	saveChanges.setEnabled(false);
	resetData.setEnabled(false);
	JTextArea outputArea = new JTextArea();
	outputArea.setBackground(Color.LIGHT_GRAY);
	outputArea.setEditable(false);
	setRightComp(outputArea, false);
	//  Set up left Component (CDB Tree)
	setLeftComp();
	//  add  splitPane to top Panel
	topPanel.add(splitPane, BorderLayout.CENTER);
	
	//  The location bar and the 'refresh button'
	JPanel north = new JPanel(new GridLayout(1,2));
	north.setBorder(BorderFactory.createEmptyBorder(5,5,5,5));
	refreshTree.addActionListener(this);
	JLabel locationLabel = new JLabel("CURRENT LOCATION:   ", JLabel.RIGHT);
	currentLocation = new JTextField(40);
	currentLocation.setBackground(Color.WHITE);
	currentLocation.setEditable(false);
	JPanel n = new JPanel(new GridLayout(1,2));
	n.add(refreshTree);
	n.add(locationLabel);
	north.add(n);
	north.add(currentLocation);

	topPanel.add(north, BorderLayout.NORTH);
	
    }

    /**
     *  Create the tabbed pane with two tabs (Table View and XML View).
     *  @param nodeHashMap data of the entry (null is possible).
     *  @param xml XML record of the node (null is possible).
     */
    public void createTabbedPane(LinkedHashMap attributes, String XML){
	JTabbedPane newTabbedPane = new JTabbedPane();

	//  create table tab
	//if(attributes != null){
	    String titles [] = {titleCol1, titleCol2};
	    Object data [][] = CDBLogic.getData(attributes);
	    if(data != null){
		JTable newTable = new CDBTable(data,titles);
		TableModel newTableModel = new CDBTableModel(data,titles);
		newTable.setModel(newTableModel);
		newTable.setBackground(Color.LIGHT_GRAY);
		newTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		CDBLogic.tableModels.put(CDBLogic.getKey(), (CDBTableModel)newTableModel);
		CDBLogic.tables.put(CDBLogic.getKey(), (CDBTable)newTable);

		newTabbedPane.addTab(" Table View ",new JScrollPane(newTable));
		CDBLogic.selectedTable = (CDBTable)newTable;
		CDBLogic.selectedTableModel = (CDBTableModel)newTableModel;
		//}
	}
	else{
	    CDBLogic.selectedTable = null;
	    CDBLogic.selectedTableModel = null;
	    CDBLogic.tableModels.put(CDBLogic.getKey(),null);
	    CDBLogic.tables.put(CDBLogic.getKey(), null);

	    newTabbedPane.addTab(" Table View ", null); 
	    newTabbedPane.setEnabledAt(CDBLogic.tableIndex,false);
	}

	//  create XML tab
	if(XML != null){
	    JTextArea XMLArea = new JTextArea(XML);
	    CDBLogic.addListener(XMLArea);
	    XMLArea.setEditable(true);
	    XMLArea.setBackground(Color.LIGHT_GRAY);
	    XMLArea.setLineWrap(true);
	    XMLArea.setWrapStyleWord(true);
	    CDBLogic.xmls.put(CDBLogic.getKey(), (JTextArea)XMLArea);

	    newTabbedPane.addTab(" XML View ", (JTextArea)XMLArea);
	    if(!newTabbedPane.isEnabledAt(CDBLogic.tableIndex)){
		newTabbedPane.setSelectedIndex(CDBLogic.xmlIndex);
	    }
	    CDBLogic.selectedXMLArea = (JTextArea)XMLArea;
	}
	else{
	    CDBLogic.xmls.put(CDBLogic.getKey(),null);
	    newTabbedPane.addTab(" XML View ", null);
	    newTabbedPane.setEnabledAt(CDBLogic.xmlIndex, false);
	    CDBLogic.selectedXMLArea = null;
	}
	
	CDBLogic.selectedTabbedPane = (JTabbedPane)newTabbedPane;
	CDBLogic.tabbedPanes.put(CDBLogic.getKey(),(JTabbedPane)newTabbedPane);

	setRightComp(newTabbedPane,true);
    }
    

    /**
     *  Sets the right component either a tabbed Pane or an empty text area.
     *  @param component the component that is added to the right side of the GUI.
     *  @param showButtons true only if component is instance of JTabbedPane.
     */
    public void setRightComp(JComponent component, boolean showButtons){
	JPanel panel = new JPanel(new BorderLayout());
	panel.setBorder(BorderFactory.createEmptyBorder(5,5,5,5));
	panel.add(component, BorderLayout.CENTER);
	
	if(showButtons){
	    JPanel buttonPanel = new JPanel(new GridLayout(1,2));
	    saveChanges.setEnabled(false);
	    resetData.setEnabled(false);
	    saveChanges.addActionListener(this);
	    resetData.addActionListener(this);
	    buttonPanel.add(saveChanges);
	    buttonPanel.add(resetData);
	    panel.add(buttonPanel, BorderLayout.NORTH);
	}

	splitPane.setRightComponent(new JScrollPane(panel));
	splitPane.setDividerLocation(dividerLocation);
    }

    /**
     *  Sets the cdb Tree in the left side of the GUI.
     */
    public void setLeftComp(){
	JTree cdbTree = CDBLogic.setUpCDBTree(strIOR);
	splitPane.setLeftComponent(new JScrollPane(cdbTree));
	splitPane.setDividerLocation(dividerLocation);
    }

    /**
     *  Adds some text to the message area at the buttom of the GUI.
     *  @param message the text to be added.
     *  @param newLine dispalay message in the next (new) line.
     */
    public void display(String message, boolean newLine){
	if(newLine){
	    messageTextArea.append("\n" + message);
	}
	else
	    messageTextArea.append(" " + message);
    }

    /**
     *  Updates the location text field.
     *  @param newLoc the new selected location.
     */
    public void setPath(String newLoc){
	currentLocation.setText(" " + newLoc);
    }

    /**
     *  Returns the path of the current selected tree node.
     *  @return String current selected tree path.
     */
    public String getPath(){
	return currentLocation.getText();
    }

    /**
     *  Enables/Disables both buttons: saveChanges & resetData
     *  @param enable enable or disable both buttons.
     */
    public void enableButtons(boolean enable){
	if(enable){
	    saveChanges.setEnabled(true);
	    resetData.setEnabled(true);
	}
	else{
	    saveChanges.setEnabled(false);
	    resetData.setEnabled(false); 
	}
    }

    /**
     *  Checks if the buttons (on top pf the tabbed pane) are enabled.
     *  @reurn true if buttons are enabled; false otherwise.
     */
    public boolean buttonsEnabled(){
	if(saveChanges.isEnabled() && resetData.isEnabled()){
	    return true;
	}else
	    return false;
    }

    /**
     * Invoked when an action occurs (User presses the button).
     * @param e A semantic event which indicates that a component-defined action occured.
     */
    public void actionPerformed(ActionEvent e) {
	if(e.getSource() == refreshTree){

	    if(buttonsEnabled()){
		String msg1 = "WARNING: Not able to refresh CDB Tree.";
		String msg2 = "Please save or reset changes made to node "+ getPath() +".";
		CDBDialog d = new CDBDialog(this,msg1,msg2);
		return;
	    }
	    messageTextArea.setText("\nRefreshing CDB Tree... ");
	    CDBLogic.clearHashMaps();
	    setPath("");
	    JTextArea empty = new JTextArea();
	    empty.setBackground(Color.LIGHT_GRAY);
	    empty.setEditable(false);
	    setRightComp(empty, false);
	    setLeftComp();
	    display("done.", false);
	}

	if(e.getSource() == resetData){
	    if(CDBLogic.isXMLTabSelected()){  //XML string has been edited
		CDBLogic.resetXMLString();
	    }
	    else{  //  Table has been edited
		CDBLogic.selectedTable.editCellAt(0,0);
		CDBLogic.resetTable();
	    }
	}

	if(e.getSource() == saveChanges){
	    if(CDBLogic.isXMLTabSelected()){
		CDBLogic.saveXMLString(false);
	    }
	    else{
		CDBLogic.selectedTable.editCellAt(0,0);
		CDBLogic.saveTable();
	    }
	}
    }
    
    /**
     *  The main method.
     *  @param args the IOR (Input Output reference) which represents the DAL access parameters.
     */
    public static void main(String []args){
	for(int i = 0; i < args.length; i ++){
	    if(args[i].equals("-d")){
		if(i < args.length -1){
		    strIOR = args[++i];
		}
	    }
	}
	instance = getInstance();
	instance.setJMenuBar(new CDBMenu());
	instance.createGUI();
	instance.setBounds(50,50,850,650);
	instance.setVisible(true);
    }
}
