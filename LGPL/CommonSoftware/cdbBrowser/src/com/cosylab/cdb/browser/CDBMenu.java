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

import javax.swing.*;
import java.awt.event.*;
import java.io.*;

class CDBMenu extends JMenuBar implements ActionListener
{
    private JMenuItem exit, clearCache, saveXML;
	private JMenuItem addNode, removeNode;

    /**
     *  Constracts a new CDBMenu.
     */
    CDBMenu(){
	JMenu file = new JMenu("File ");
	file.setMnemonic(KeyEvent.VK_F);
	
	saveXML = new JMenuItem("Save XML file");
	saveXML.addActionListener(this);
	file.add(saveXML);

	exit = new JMenuItem("Exit");
	exit.addActionListener(this);
	file.add(exit);

	JMenu cdbAdmin = new JMenu("Administration");
	cdbAdmin.setMnemonic(KeyEvent.VK_A);

	clearCache = new JMenuItem("Clear Cache");
	clearCache.addActionListener(this);
	cdbAdmin.add(clearCache);

	JMenu cdbEdit = new JMenu("Edit");
	cdbEdit.setMnemonic(KeyEvent.VK_E);

	addNode = new JMenuItem("Add node");
	addNode.addActionListener(this);
	cdbEdit.add(addNode);

	removeNode = new JMenuItem("Remove node");
	removeNode.addActionListener(this);
	cdbEdit.add(removeNode);


	this.add(file);
	this.add(cdbEdit);
	this.add(cdbAdmin);
    }

    /**
     *  Invoked when an action occurs.
     */
    public void actionPerformed(ActionEvent event){
		if(event.getSource() == addNode){
			CDBLogic.addNode();
		}
		if(event.getSource() == removeNode){
			CDBLogic.deleteNode();
		}

	if(event.getSource() == exit){
	    if(Browser.getInstance().buttonsEnabled()){
		String msg1 = "Warning: Node has been modified.";
		String msg2 = "Please save or reset changes before closing the Browser.";
		CDBDialog dialog1 = new CDBDialog(Browser.getInstance(), msg1,msg2);
		return;
	    }
	    System.exit(0);
	}
	if(event.getSource() == saveXML){

	    if(Browser.getInstance().buttonsEnabled() && CDBLogic.isXMLTabSelected()){
		String msg1 = "Warning: XML string has been modified.";
		String msg2 = "It is raccomanded to validate (save) or reset it before saving it.";
		CDBDialog dialog2 = new CDBDialog(Browser.getInstance(),msg1,msg2);
		//return;
	    }
	    
	    if(CDBLogic.isXMLTabSelected()){
		Browser.getInstance().display("MESSAGE: Trying to save XML String... ", true);
		JFileChooser fc = new JFileChooser();
		int returnVal = fc.showSaveDialog(fc);

		switch(returnVal){
		    case JFileChooser.CANCEL_OPTION:
			Browser.getInstance().display("saving interrupted.\t(Location " + Browser.getInstance().getPath() + ")." , false);
			break;
		case JFileChooser.APPROVE_OPTION:
		    File xmlFile = fc.getSelectedFile();
		    //JTextArea xmlArea = (JTextArea)currentTabbedPane.getComponentAt(xmlIndex);
		    try{
			CDBLogic.selectedXMLArea.write(new FileWriter(xmlFile));
		    }
		    catch(IOException e){};
		    Browser.getInstance().display("XML String saved successfully to file: " + xmlFile.getAbsolutePath()
						  + ".\t(Location: " + Browser.getInstance().getPath() + ").", false);
		    break;
		case JFileChooser.ERROR_OPTION:   
		    Browser.getInstance().display("ERROR occured while trying to save XML string. \t(Location: "
						  + Browser.getInstance().getPath() + ").", false);
		    break;
		}
	    }
	    else{
		String msg1 = "Warning: Trying to save XML String.";
		String msg2 = "To save XML String you have to select a XML Tab.";	
		CDBDialog dialog3 = new CDBDialog(Browser.getInstance(),msg1,msg2);
	    }
	}

	if(event.getSource() == clearCache){
	    CDBLogic.clearCache();
	}
    }
}
