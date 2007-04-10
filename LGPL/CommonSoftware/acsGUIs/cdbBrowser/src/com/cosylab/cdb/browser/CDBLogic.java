/***************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) European Southern Observatory, 2002
*    Copyright by ESO (in the framework of the ALMA collaboration)
*    and Cosylab 2002, All rights reserved
*
*    This library is free software; you can redistribute it and/or
*    modify it under the terms of the GNU Lesser General Public
*    License as published by the Free Software Foundation; either
*    version 2.1 of the License, or (at your option) any later version.
*
*    This library is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*    Lesser General Public License for more details.
*
*    You should have received a copy of the GNU Lesser General Public
*    License along with this library; if not, write to the Free Software
*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
*    MA 02111-1307  USA
*/
package com.cosylab.cdb.browser;
import com.cosylab.CDB.*;
import java.util.*;
import java.io.*;
import javax.swing.*;
import javax.swing.tree.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import javax.naming.*;
import alma.acs.util.ACSPorts;
import alma.cdbErrType.CDBRecordIsReadOnlyEx;

class CDBLogic implements  TreeSelectionListener, TreeExpansionListener, KeyListener
{
    /**
     *  The CDB tree.
     */
    static JTree CDBTree = null;

    /**
     *  Instance of the DAL server.
     */
    private static DAL dal;

    /**
     *  Reference to writable DAL interface if it is implemented by DAL
     */
    private static WDAL wdal = null;
	
    /**
     *  Reference to JDAL interface if it is implemented by DAL
     */
    private static JDAL jdal = null;

    /**
     *  The string is used as a key for the hashMaps used to store the components.
     *  String is the full tree paht of the component.
     *  (set in BrowserJNDIContext & BrowserJNDIXMLContext).
     */
    private static String hashMapKey;

    /**
     *  Stores the original XML String how it is before the user starts editing 
     */
    private static String originalXMLString = null;
    
    /**
     *  Boolean that tells if the XML string has been edited.
     */
    public static boolean XMLStringChanged = false;

    /**
     *  The name of the first table column.
     */
    static final String titleCol1 = "ATTRIBUTE NAME";

    /**
     *  The name of the second table column.
     */
    static final String titleCol2 = "ATTRIBUTE VALUE";

	/**
	 *  The prefix added to each curl
	 */
	static final String rootPrefix = "/root";

    /**
     *  The index of the tab containing the Table.
     */
    static final int tableIndex = 0;

    /**
     *  The index of the tab containing the XML String.
     */
    static final int xmlIndex = 1;

    //private static boolean valueChangedEvent = true;
    /**
     *  Boolean that tells id the table has been edited.
     */
    static boolean tableChanged = false;

    /**
     *  Boolean that tells if a tabbed pane has a table tab.
     *  If false -> no table view available.
     */
    private static boolean tableIndexEnabled = false;
    
    /**
     *  Boolean that tells if a tabbed pane has a XML tab.
     *  If false -> no XML View.
     */
    static boolean XMLIndexEnabled = false;

    /**
     *  HashMap that stored the tabbed panes (JTebbedPane).
     */
    static HashMap tabbedPanes = new HashMap();
    
    /**
     *  HashMap that stores the tables (CDBTable).
     */
    static HashMap tables = new HashMap();

    /**
     *  HashMap that stores the table models (CDBTableModel)
     */
    static HashMap tableModels = new HashMap();

    /**
     *  HashMap that stores the XML components (JTextArea).
     */
    static HashMap xmls = new HashMap();

    /**
     *  The currently selected tabbed pane (can be null).
     */
    static JTabbedPane selectedTabbedPane;
    
    /**
     *  The currently selected XML component (can be null).
     */
    static JTextArea selectedXMLArea;

    /**
     *  The currently selected table component (can be null).
     */
    static CDBTable selectedTable;

    /**
     *  The currently selected table model (can be null).
     */
    static CDBTableModel selectedTableModel;

    /**
     *  Create the CDB Tree.
     */
    public static JTree setUpCDBTree(String strIOR){
	Hashtable env = new Hashtable();
	env.put(Context.INITIAL_CONTEXT_FACTORY, "com.cosylab.cdb.browser.BrowserJNDIContextFactory");
	
	if(strIOR == null)
	    {
		env.put(Context.PROVIDER_URL, "corbaloc::" + ACSPorts.getIP() + ":" + ACSPorts.getCDBPort() + "/CDB");
	    }
	else
	    env.put(Context.PROVIDER_URL, strIOR);
	
	Context context = null;
	try{
	    context = new InitialContext(env);
	    
	}catch(NamingException e)
	    {
		e.printStackTrace();
	    }
	if(context == null)
	    {
		return null;
	    }
	
	// create Tree with the given context
	CDBTree = new JTree(new com.cosylab.cdb.CDBTreeNode("root",null,context));
	CDBTree.addTreeSelectionListener(new CDBLogic());
	CDBTree.addTreeExpansionListener(new CDBLogic());
	CDBTree.setToggleClickCount(1);
	return CDBTree;
    }

    /**
     *  Set the DAL server.
     */
    public static void setDAL(DAL dal){
	CDBLogic.dal = dal;
	// check if it is writable DAL 
	try {
		wdal = WDALHelper.narrow(dal);
	}
	catch(Exception e) {
		// no op
	}
		
	// check if it is writable DAL 
	try {
		jdal = JDALHelper.narrow(dal);
	}
	catch(Exception e) {
		// no op
	}
    
     }

    /**
     *  Sets the key of all hash maps used to store the data.
     *  @param key the unique key of the next entry to be stored. 
     */
    public static void setKey(String key){
	hashMapKey = "/root" + key;
    }

    /**
     *  Get the current key.
     *  @return the current key.
     */
    public static String getKey(){
	return hashMapKey;
    }

    /**
     *  Creates a two dimensional array used to create the tables data.
     *  @param hashM the data of the table.
     */
    public static Object[][]getData(LinkedHashMap attributes){
	if(!attributes.isEmpty()){// && attributes != null){
	    int len = attributes.size();
	    //System.out.println(attributes.toString());
	    //  Get ATTRIBUTE NAMES (keys of the LinkedHashMap) and store them into an array.
	    Set names = attributes.keySet();
	    Object [] attributeNames = names.toArray(new String[len]);
	    
	 
	    //  Get ATTRIBUTE VALUES ()
	    Collection values = attributes.values();
	    Object [] attributeValues = values.toArray();

	    //  put names & values into 2 dimensional array
	    String att [] = new String[len*2];
	    int j = 0;
 	    for(int i = 0; i < len*2; i = i+2){
 		att [i] = (String) attributeNames [j];
		j ++;
       	    }
	    j = 0;
 	    for(int i = 1; i <= len*2 - 1; i = i+2){
		att [i] = (String)attributeValues [j];
		j++;
	    }
	    Object data [] [] = new Object [len][2];

	    int index = 0;
	    for(int i = 0; i < len; i ++){
		for(int col = 0; col < 2; col ++){
		    data [i][col] = att[index];
		    //System.out.println("data " + i + " " + col + " = " + att[index]);
		    index ++;
		}
	    }
	    return data;   
	}

	return null;
    }

    /**
     *  Called whenever an item in the tree has been expanded.
     *  @param event the event that characterizes the change.
     */
    public void treeExpanded(TreeExpansionEvent event){
	TreePath tp = event.getPath();
	String path = stringPath(tp);
	CDBTree.setSelectionPath(tp);
    }

    /**
     *  Called whenever an item in the tree has been collapsed.
     *  @param event the event that characterizes the change.
     */
    public void treeCollapsed(TreeExpansionEvent event){
	TreePath tp = event.getPath();
	String path = stringPath(tp);
	CDBTree.setSelectionPath(tp);
    }

    /**
     *  Called whenever the value of the selection changes.
     *  @param event the event that characterizes the change.
     */
    public void valueChanged(TreeSelectionEvent event){
	
	TreePath tp = event.getPath();
	String path = stringPath(tp);
	Browser.getInstance().setPath(path);
	//Browser.getInstance().display("MESSAGE: Selected node: " + tp.toString() + ".",true);
	
	//  check that selected node has a corresponding tabbed pane
	if(tabbedPanes.containsKey(path)){
	    //  update selected components
	    selectedTable = (CDBTable)tables.get(path);
	    selectedTableModel = (CDBTableModel)tableModels.get(path);
	    selectedXMLArea = (JTextArea)xmls.get(path);
	    selectedTabbedPane = (JTabbedPane)tabbedPanes.get(path);
	    //  set selected tabbedPane
	    Browser.getInstance().setRightComp(selectedTabbedPane,true);
	}
	else{	 
	    //Browser.getInstance().display(" No tabbed pane available for this node", false);
	    //  set selected components
	    selectedTabbedPane = null;
	    selectedTable = null;
	    selectedTableModel = null;
	    selectedXMLArea = null;
	    
	    //  set an emty text area
	    JTextArea empty = new JTextArea();
	    empty.setBackground(Color.LIGHT_GRAY);
	    empty.setEditable(false);
	    Browser.getInstance().setRightComp(empty,false);
	}
    }
           
    /**
     *  Send the XML String for validation.
     */
    public static void saveXMLString(boolean ok){
	if(XMLStringChanged){
	    //  Get XML String that has to be validated
	    String newXMLString = selectedXMLArea.getText();
	    
	    //  Validation was suesfully.
	    if(ok){
			// -> DV  
			 if(wdal != null) {
				 String curl = CDBLogic.getKey().substring(rootPrefix.length());
				 try {
					Browser.getInstance().display("Invoking set_DAO for " + curl, true);
					 wdal.set_DAO(curl, newXMLString);
					ok = true;
				 }catch (CDBRecordIsReadOnlyEx e){ 
                    e.printStackTrace();
                    Browser.getInstance().display("==> ERROR MESSAGE: The file or record is read only: " + e, true);

                 }catch (Exception e) {
					 e.printStackTrace();
					 Browser.getInstance().display("==> ERROR MESSAGE: Exception while saving changes " + e, true);				
				 } 
			 }
			 // <-


		//  update components??

		Browser.getInstance().enableButtons(false);
		CDBTree.setEnabled(true);
		if(tableIndexEnabled){
		    selectedTabbedPane.setEnabledAt(tableIndex,true);
		    tableIndexEnabled = false;
		}
		originalXMLString = null;	
	    }

	    // Validation failed.
	    if(!ok){
		Browser.getInstance().display("==> ERROR MESSAGE: Validation function under development. ",true);
		//  Reset XML String to its original value.
		selectedXMLArea.setText(originalXMLString);
		originalXMLString = null;
		Browser.getInstance().enableButtons(false);
		CDBTree.setEnabled(true);
		if(tableIndexEnabled){
		    selectedTabbedPane.setEnabledAt(tableIndex,true);
		    tableIndexEnabled = false;
		}
		Browser.getInstance().display("XML string is reset to its original value.",false);
	    }
	    
	    XMLStringChanged = false; 
	}
    }

    /**
     *  Reset the XML String of the selected tabbed pane to its original value
     *  No interaction with jDAL needed.
     */
    public static void resetXMLString(){
	if(XMLStringChanged){
	    Browser.getInstance().display("MESSAGE: Resetting XML string for node " + Browser.getInstance().getPath() + ". ",true);
	    selectedXMLArea.setText(originalXMLString);
	    originalXMLString = null;
	    Browser.getInstance().enableButtons(false);
	    CDBTree.setEnabled(true);
	    if(tableIndexEnabled){
		selectedTabbedPane.setEnabledAt(tableIndex,true);
		tableIndexEnabled = false;
	    }
	    //Browser.getInstance().display("Enable CDBTree & disable buttons.",false);
	    XMLStringChanged = false;
	}
    }
    

    public static void resetTable(){
 	if(tableChanged){
	   
 	    Browser.getInstance().display("MESSAGE: Resetting values for table " + Browser.getInstance().getPath() + ". ",true);
 	    selectedTableModel.resetValues();
 	    Browser.getInstance().enableButtons(false);
 	    CDBTree.setEnabled(true);
 	    if(XMLIndexEnabled){
 		selectedTabbedPane.setEnabledAt(xmlIndex,true);
 		XMLIndexEnabled = false;
 	    }
 	    tableChanged = false;
	    selectedTable.emptyArray();
 	}
    }

    public static void saveTable(){
	if(tableChanged){
	    Browser.getInstance().display("MESSAGE: Saving values for table " + Browser.getInstance().getPath() + ". ",true);
	    
	    // -> DV added 
		try {
			// compose short xml of changed fields and send it to the WDAL
			boolean isNode = XMLIndexEnabled; // we must find node for DAL since we can be inside inner element i.e. MOUNT/current
			String currentPath = CDBLogic.getKey();

			String curl = CDBLogic.getKey();
			while(CDBLogic.xmls.get(curl) == null) {
				curl = curl.substring(0, curl.lastIndexOf('/'));
			}
			
			// TODO find better way for curl finding
			// TODO BUG CDBLogic.getKey() must be used because maps are filled with it but it is not synchronized
			// with Browser.getInstance().getPath() 
			String elementName;
			if(!currentPath.equals(curl)) // in cases when we have element which is not a node i.e. 'current' inside 'TEST_PS_1' then we strip elementName 
				elementName = currentPath.substring(curl.length()+1);
			else
				elementName = "";
			curl = curl.substring("/root/".length());
			
			System.out.println("curl=" + curl + " elem=" + elementName);
			//
			// send changed values to the WDAL
			StringWriter sw = new StringWriter();
			sw.write("<?xml version='1.0' encoding='ISO-8859-1'?>\n");
			sw.write("<curl ");
			if(elementName.length() >0)
				sw.write("><" + elementName +" ");
			for (int i = 0; i < selectedTableModel.getRowCount(); i++) {
				if (selectedTableModel.resetValue.get(new Integer(i)) != null) {
					System.out.println(XMLIndexEnabled + " " + Browser.getInstance().getPath() + " changed is " + selectedTableModel.getValueAt(i,0) + "=" + selectedTableModel.getValueAt(i,1));
					sw.write(selectedTableModel.getValueAt(i,0) + "=\"" + selectedTableModel.getValueAt(i,1) + "\" ");
				}
			}
			if(elementName.length() >0)
				sw.write("/></curl>");
			else
				sw.write("/>");
			
			if(wdal != null) {
				Browser.getInstance().display("Invoking set_DAO for " + curl, true);
				wdal.set_DAO(curl, sw.toString());
			}
		}catch (CDBRecordIsReadOnlyEx e){
            e.printStackTrace();
            Browser.getInstance().display("==> ERROR MESSAGE: The file or record is read only: " + e, true);

        } catch (Exception e) {
			e.printStackTrace();
			Browser.getInstance().display("==> ERROR MESSAGE: Exception while saving changes " + e, true);				
		}
	    // <-
	    
	    Browser.getInstance().enableButtons(false);
 	    CDBTree.setEnabled(true);
	    if(XMLIndexEnabled){
 		selectedTabbedPane.setEnabledAt(xmlIndex,true);
 		XMLIndexEnabled = false;
 	    }
	    tableChanged = false;
	    selectedTable.emptyArray();
	}
    }

    /**
     *  Add a key listener to the XML text Area.
     */
    public static void addListener(JTextArea XMLArea){
	XMLArea.addKeyListener(new CDBLogic());
    }

    /**
     *  Invoked when a key has been pressed (inside a XML text area).
     */
    public void keyPressed(KeyEvent e){
	if(e.getSource() == selectedXMLArea){
	    if(XMLStringChanged == false){
		originalXMLString = selectedXMLArea.getText();
		Browser.getInstance().display("MESSAGE: Editing XML string for node:  " + Browser.getInstance().getPath() ,true);
		CDBTree.setEnabled(false);
		if(selectedTabbedPane.isEnabledAt(tableIndex)){
		    tableIndexEnabled = true;
		    //  disable the table index
		    selectedTabbedPane.setEnabledAt(tableIndex,false);
		}
		Browser.getInstance().enableButtons(true);
		XMLStringChanged = true;
		Browser.getInstance().display(". To enable CDBTree save or reset changes made.",false);
	    }
	}
    }
    
    /**
     *  Invoked when a key has been released.
     */
    public void keyReleased(KeyEvent e){}

    /**
     *  Invoked when a key has been typed.
     */
    public void keyTyped(KeyEvent e){}

    /**
     *  Checks if any XML tab is selected.
     *  @return true if XML tab is selected, false otherwise
     */
    public static boolean isXMLTabSelected(){
	if(selectedTabbedPane == null){
	    return false;
	}
	if(selectedTabbedPane.getSelectedIndex() == xmlIndex){
	    return true;
	}
	else
	    return false;
    }

    /**
     *
     */
    public static void clearCache(){
	if(jdal != null){
	    jdal.clear_cache_all();
	    Browser.getInstance().display("MESSAGE: Clear Cache executed sucessfully", true);
	}else{
	    
	    Browser.getInstance().display("==> ERROR MESSAGE: Clear Cache was not executed.", true);
	}
    }

    /**
     *  Return a String representation of the selected path.
     *  @param tp the selected Tree Path
     */
    public static String stringPath(TreePath tp){
	Object [] p = tp.getPath();
	String path = "";
	for(int i = 0; i < tp.getPathCount(); i ++){
	    path = path + "/" + (String)p[i].toString();
	}
	return path;
    }

    /**
     *  remove all elements inside HashMaps.
     */
    public static void clearHashMaps(){
	tabbedPanes.clear();
	tables.clear();
	tableModels.clear();
	xmls.clear();
	
	selectedTabbedPane = null;
	selectedTable = null;
	selectedTableModel = null;
	selectedXMLArea = null;
    }
    
    
    public static String getCurl() {
    	if(CDBLogic.getKey() == null)
    		return "";
		return CDBLogic.getKey().substring(rootPrefix.length());
    }

	/**
	 * 
	 */
	public static void addNode() {
		// if we have writable interface
		if(wdal == null) {
			Browser.getInstance().display("==> ERROR MESSAGE: Unable to narrow writable DAL server ", true);
			return;
		}
		String curl = getCurl();
		CDBAddNodeDlg dlg = new CDBAddNodeDlg(Browser.getInstance());
		dlg.setCurl(curl+"/");
		dlg.setXML("<?xml version='1.0' encoding='ISO-8859-1'?>\n<Node>\n</Node>");

		if(dlg.showModal() != JOptionPane.OK_OPTION)
			return;
		
		System.out.println("Dialog showed " + dlg.getCurl() + " -> " + dlg.getXML());
		// try to add node
		try {
			wdal.add_node(dlg.getCurl(), dlg.getXML());
		}
		catch(Exception e ) {
			Browser.getInstance().display("==> ERROR MESSAGE: Error while trying to add node " + curl + " : " + e, true);
		}
	}

	/**
	 * 
	 */
	public static void deleteNode() {
		// if we have writable interface
		if(wdal == null) {
			Browser.getInstance().display("==> ERROR MESSAGE: Unable to narrow writable DAL server ", true);
			return;
		}
		// ask to be sure
		String curl = getCurl();
		int retVal = JOptionPane.showConfirmDialog(Browser.getInstance(),
			"Are you sure you want to delete node " + curl + "?", "Delete node", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
		if(retVal != JOptionPane.YES_OPTION )
			return;

		// try to delete it
		try {
			wdal.remove_node(curl);
		}
		catch(Exception e ) {
			Browser.getInstance().display("==> ERROR MESSAGE: Error while trying to delete node " + curl + " : " + e, true);
		}
	}
}
