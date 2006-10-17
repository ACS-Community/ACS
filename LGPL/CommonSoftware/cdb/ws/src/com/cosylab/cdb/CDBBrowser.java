package com.cosylab.cdb;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Frame;
import java.awt.HeadlessException;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.Hashtable;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.swing.JApplet;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import javax.swing.JTree;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;

import alma.acs.util.ACSPorts;

/*******************************************************************************
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
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *
 * @author dragan
 *
 * To change this generated comment edit the template variable "typecomment":
 * Window>Preferences>Java>Templates.
 * To enable and disable the creation of type comments go to
 * Window>Preferences>Java>Code Generation.
 */
public class CDBBrowser extends JApplet implements TreeSelectionListener {

	// members
	private JSplitPane jSplitPane;
	private JTree cdbTree;
	private JTextArea jTextArea;
	private String strIOR = null;

	/**
	 * Constructor for CDBBrowser.
	 * @throws HeadlessException
	 */
	public CDBBrowser() throws HeadlessException {
		super();
	}

	/**
	 * Sets the strIOR.
	 * @param strIOR The strIOR to set
	 */
	public void setIOR(String strIOR) {
		this.strIOR = strIOR;
	}

	/**
	 * @see java.applet.Applet#init()
	 */
	public void init() {
		// try to get context of the CDB server
		Hashtable env = new Hashtable();
		env.put(Context.INITIAL_CONTEXT_FACTORY, "com.cosylab.cdb.jdal.JNDIContextFactory");
		if (strIOR == null)
			env.put(Context.PROVIDER_URL, "corbaloc::" + ACSPorts.getIP() + ":" + ACSPorts.getCDBPort() + "/CDB");
		else
			env.put(Context.PROVIDER_URL, strIOR);
		Context context = null;
		try {
			context = new InitialContext(env);
		} catch (NamingException e) {
			e.printStackTrace();
		}
		if (context == null) {
			return;
		}

		cdbTree = new JTree(new CDBTreeNode("root", null, context));
		cdbTree.addTreeSelectionListener(this);

		jTextArea = new JTextArea();

		jSplitPane = new JSplitPane();
		jSplitPane.setOrientation(JSplitPane.HORIZONTAL_SPLIT);
		jSplitPane.setLeftComponent(new JScrollPane(cdbTree));
		jSplitPane.setRightComponent(new JScrollPane(jTextArea));

		// add it to container
		Container cp = getContentPane();
		cp.setLayout(new BorderLayout());
		cp.add(jSplitPane, java.awt.BorderLayout.CENTER);

		validate();

	}

	public void valueChanged(TreeSelectionEvent event) {
		Object last = cdbTree.getLastSelectedPathComponent();
		if (last != null) {
			if (last instanceof CDBTreeNode) {
				CDBTreeNode node = (CDBTreeNode) last;
				String value = node.getValue();
				if (value != null)
					jTextArea.setText(value);
			}
			//System.out.println( "Last path " + last.getClass().getName() + " is " + last.toString());
		}
	}

	public static void main(String[] args) {
		CDBBrowser app = new CDBBrowser();
		// test for IOR in cmd line
		for (int i = 0; i < args.length; i++) {
			if (args[i].equals("-d")) {
				if (i < args.length - 1) {
					app.setIOR(args[++i]);
				}
			}
			if(args[i].equals("-h") || args[i].equals("-help")){
				System.out.println("Usage: cmd [-d ior -h|-help]");
				return;
			}
		}

		Frame frame = new Frame("CDB Browser");
		frame.add(app);
		frame.setSize(800, 600);
		frame.setLocationRelativeTo(null);
		frame.addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent e) {
				System.exit(0);
			}
		});
		app.init();
		app.start();
		frame.setVisible(true);
	}

}
