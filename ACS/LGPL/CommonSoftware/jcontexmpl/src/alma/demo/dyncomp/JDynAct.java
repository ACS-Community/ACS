/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
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
package alma.demo.dyncomp;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

import si.ijs.maci.ComponentSpec;


/**
 * @author acaproni Nov 3, 2003
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class JDynAct extends JDialog implements ActionListener,  WindowListener 
{
	// GUI components
	JComboBox nameCB, idlCB, implCB, containerCB;
	JPanel variableAreaP;
	JTable activatedT;
	JPopupMenu popMenu;

	// The mangerLoc and the logger
	private String m_managerLoc;

	// The clients to acivate dynamic components
	Client theClient;
	
	public JDynAct(String managerLoc) {
		super();
		setTitle("Dynamic component activator");
		buildWindow();
		setBounds(50,50,10,10);
		pack();
		addWindowListener(this);
		
		m_managerLoc=managerLoc;
		try {
			theClient =  new Client(null,m_managerLoc,"DynCompClient");
		} catch (Exception e) {
			System.err.println("Error: "+e.toString());
			e.printStackTrace(System.err);
			System.exit(-1);
		}

		popMenu = new JPopupMenu();
		setVisible(true);
		setDefaultCloseOperation(DISPOSE_ON_CLOSE);
	}
	
	/** Build the GUI
	 *
	 */
	private void buildWindow() {
		Container rootCnt = getContentPane();
		rootCnt.setLayout(new BorderLayout());
		// The container with labels and combo boxes
		Container firstCnt = new Container();
		firstCnt.setLayout(new GridLayout(4,2));
		firstCnt.add(new JLabel("Name"));
		nameCB=new JComboBox();
		nameCB.setEditable(true);
		nameCB.addItem("*");
		nameCB.addItem("PIPPO");
		nameCB.addItem("PLUTO");
		firstCnt.add(nameCB);
		firstCnt.add(new JLabel("IDL interface"));
		idlCB=new JComboBox();
		idlCB.addItem("*");
		idlCB.addItem("IDL:alma/acsexmplLamp/Lamp:1.0");
		idlCB.addItem("IDL:alma/MOUNT_ACS/Mount:1.0");
		idlCB.addItem("IDL:alma/demo/HelloDemo:1.0");
		idlCB.setEditable(true);
		firstCnt.add(idlCB);
		firstCnt.add(new JLabel("Implementation"));
		implCB=new JComboBox();
		implCB.addItem("*");
		implCB.addItem("acsexmplLampImpl");
		implCB.addItem("acsexmplMountImpl");
		implCB.addItem("alma.demo.HelloDemoImpl.HelloDemoHelper");
		implCB.addItem("demoImpl.HelloDemo");
		implCB.addItem("acsexmplHelloWorldClient");
		implCB.setEditable(true);
		firstCnt.add(implCB);
		firstCnt.add(new JLabel("Container"));
		containerCB=new JComboBox();
		containerCB.addItem("*");
		containerCB.addItem("bilboContainer");
		containerCB.addItem("frodoContainer");
		containerCB.addItem("aragornContainer");
		containerCB.setEditable(true);
		firstCnt.add(containerCB);
		// The container with the activate button
		Container secondCnt = new Container();
		secondCnt.setLayout(new FlowLayout());
		JButton activateB = new JButton("Activate");
		activateB.addActionListener(this);
		secondCnt.add(activateB,"Center");
		// The container with activated container
		MyTableModel tableModel = new MyTableModel();
		activatedT = new JTable(tableModel);
		activatedT.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		ListSelectionModel rowSM = activatedT.getSelectionModel();
		JScrollPane scrollP = new JScrollPane(activatedT);
		activatedT.setPreferredScrollableViewportSize(new Dimension(400,90));
		MyCellRendererr cellR = new  MyCellRendererr();
		TableColumnModel tcm = activatedT.getColumnModel();
		TableColumn tc = tcm.getColumn(2);
		tc.setCellRenderer(cellR);
		MyCellEditor cellE = new MyCellEditor(this);
		tc.setCellEditor(cellE);
		Container thirdCnt = new Container();
		thirdCnt.setLayout(new FlowLayout());
		thirdCnt.add(scrollP,"North");

		// Add the container to the main container
		rootCnt.add(firstCnt,"North");
		rootCnt.add(secondCnt,"Center");
		rootCnt.add(thirdCnt,"South");
	}

	/** Start a dynamic component
	 * Some of the paraeters may be a '*' instead of a full specified string
	 * 
	 * @param Name The name of the dynamic component
	 * @param IDL The idl interface
	 * @param Implementation The implementation 
	 * @param Container The container
	 * 
	 */
	private void startDynamicComponent(String Name, String IDL, String Implementation, String Container) {
		boolean activated;
		// Check if a dynamic component with the same name was already activated
		MyTableModel myModel=(MyTableModel)activatedT.getModel();
		if (myModel.exist(Name)) {
			 JOptionPane.showOptionDialog(
				this,
				"The component "+Name+" already exists",
				"Error activating the component",
				JOptionPane.DEFAULT_OPTION,
				JOptionPane.ERROR_MESSAGE,
				null,null,null);
			return;
		}
		// Start the dynamic component....
		String activatedComponentUrl=null;
		if (theClient.hasFreeSlot()) {
			ComponentSpec compSpec = new ComponentSpec(Name,IDL,Implementation,Container);
			try {
				activatedComponentUrl=theClient.getDynamicComponent(compSpec,false);
			} catch (Exception ce) {
				System.err.println("Error activating the component: "+ce.toString());
				ce.printStackTrace(System.err);
				activated=false;
			}
			// The component is activated => add a new entry in the GUI
			if (activatedComponentUrl!=null) {
				myModel.append(Name,activatedComponentUrl);
			} else {
			 JOptionPane.showOptionDialog(
				this,
				"Dynamic activation error",
				"Error activating the component",
				JOptionPane.DEFAULT_OPTION,
				JOptionPane.ERROR_MESSAGE,
				null,null,null);
			}
		} else JOptionPane.showOptionDialog(
				this,
				"No free slot available",
				"Error activating the component",
				JOptionPane.DEFAULT_OPTION,
				JOptionPane.ERROR_MESSAGE,
				null,null,null);
	}
	
	/** Method execution when an ActionEvent arrived
	*/
	public void actionPerformed(ActionEvent ae) {
		popMenu.setVisible(false);
		if (ae.getActionCommand().compareTo("Activate")==0) {
			startDynamicComponent(
				(String)nameCB.getSelectedItem(),
				(String)idlCB.getSelectedItem(),
				(String)implCB.getSelectedItem(),
				(String)containerCB.getSelectedItem());
		} else if (ae.getActionCommand().split(" ")[0].compareTo("Release")==0) {
			String splitted[] = ae.getActionCommand().split(" ");
			// Get the cURL of the component
			MyTableModel myModel=(MyTableModel)activatedT.getModel();
			String cURL=myModel.getURL(splitted[1]);
                	if (releaseComponent(cURL)) {
	                        ((JButton)ae.getSource()).removeActionListener(this);
				((MyCellEditor)activatedT.getCellEditor()).stopEditing();
				((MyTableModel)activatedT.getModel()).sort();
			}
		} 
	}

	/** The starting point of the program
	*/
	public static void main(String[] args)
	{

		String managerLoc= System.getProperty("ACS.manager");
		if (managerLoc==null) {
			System.err.println("Error getting ACS.manager property");
			System.exit(-1);
		}
		new JDynAct(managerLoc);
	}

	/** Release a componet
	* @param url The url of the component
	*/
	public boolean releaseComponent(String url) {
		if (theClient.releaseComponent(url)) {
			MyTableModel tm=(MyTableModel)activatedT.getModel();
			tm.deleteEntry(url);
			return true;
		} else {
			 JOptionPane.showOptionDialog(
				this,
				"Error releasing the component",
				"Dynamic activation error",
				JOptionPane.DEFAULT_OPTION,
				JOptionPane.ERROR_MESSAGE,
				null,null,null);
			return false;
		}
	}

	/** Release all the resources and exit the program
	*/
	private void cleanExit() {
		// Realease all the dynamic components
		theClient.cleanExit();
		System.exit(0);
	}

	public void windowActivated(WindowEvent e) {}
	public void windowClosing(WindowEvent e) {}
	public void windowDeactivated(WindowEvent e) {}
	public void windowDeiconified(WindowEvent e) {}
	public void windowIconified(WindowEvent e) {}
	public void windowOpened(WindowEvent e) {}
	public void windowClosed(WindowEvent e) { cleanExit(); }
}





