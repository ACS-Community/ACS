/*
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
package alma.acs.gui.loglevel;

import java.awt.Dimension;

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.SwingUtilities;

import alma.acs.container.ContainerServices;
import alma.acs.gui.loglevel.leveldlg.ButtonTabComponent;
import alma.acs.gui.loglevel.leveldlg.LogLevelSelectorPanel;
import alma.acs.gui.loglevel.tree.LogLvlTree;
import alma.acs.gui.util.panel.IPanel;

/**
 * The panel of the application containing the tabs:
 * - one tab with the ACS tree showing clients, containers and components
 * - one tab per each container whose logs levels the user wants to change
 * 
 * @author acaproni
 *
 */
public class LogLevelPanel extends JTabbedPane implements IPanel {

	private static final long serialVersionUID = 4841746787234184889L;

	// The container services
    private ContainerServices contSvc=null;
    
    // The tree
    private LogLvlTree tree=null;
    private JScrollPane treeSP;
    
    // The window that shows this panel
    private JFrame frame=null;
    
	/**
	 * Constructor 
	 *
	 */
	public LogLevelPanel() {
		super();
		initialize();
	}
	
	/**
	 * Constructor 
	 * 
	 * @param frame The window that owns this panel
	 */
	public LogLevelPanel(JFrame frame) {
		if (frame==null) {
			throw new IllegalArgumentException("Invalid null frame in constructor");
		}
		this.frame=frame;
		initialize();
	}
	
	/**
	 * Init the component
	 *
	 */
	private void initialize() {
		setPreferredSize(new Dimension(150,100));
	}
	
	/**
	 * Method used by the plugin interface in EXEC:
	 * it connects the application to the NC
	 * @see alma.exec.extension.subsystemplugin.SubsystemPlugin
	 * 
	 * @throws Exception
	 */
	public void start() throws Exception {
		if (contSvc==null) {
			throw new IllegalStateException("Trying to start the panel with null ContainerServices");
		}
		
		String managerLoc = System.getProperty("ACS.manager");
	    if (managerLoc!=null) {
	    	managerLoc=managerLoc.trim();
	    } else {
	    	throw new IllegalStateException("ACS.magager property not set!");
	    }
	   
		tree = new LogLvlTree(this,contSvc.getAdvancedContainerServices().getORB(),contSvc.getLogger());
		treeSP= new JScrollPane(tree);
		treeSP.setName("ACS tree");
		tree.start();
		add(treeSP);
	}
	
	/**
	 * Method used by the plugin interface in EXEC.
	 * Stop the application disconnecting from the NC
	 * @see alma.exec.extension.subsystemplugin.SubsystemPlugin
	 * 
	 * @throws Exception
	 */
	public void stop() throws Exception {
		tree.stop();
		tree.setVisible(false);
		tree=null;
		contSvc=null;
	}
	
	/**
	 * Method used by the plugin interface in EXEC.
	 * Pause the application (scroll lock enabled)
	 * @see alma.exec.extension.subsystemplugin.IPauseResume
	 * 
	 * @throws Exception
	 */
	public void pause() throws Exception {
	}
	
	/**
	 * Method used by the plugin interface in EXEC.
	 * Unpause the application (scroll lock disabled)
	 * @see alma.exec.extension.subsystemplugin.IPauseResume
	 * 
	 * @throws Exception
	 */
	public void resume() throws Exception {
	}
	
	 /* Run in restricted mode
	 * 
	 * @see alma.exec.extension.subsystemplugin.SubsystemPlugin
	 */
	public boolean runRestricted (boolean restricted) throws Exception {
		return restricted;
	}
	
	/**
	 * Set the ACS Container Services
	 * 
	 * @param cs The container services
	 */
	public void setACSContainerServices(ContainerServices cs) {
		if (cs==null) {
			throw new IllegalArgumentException("Invalid null ContainerServices");
		}
		contSvc=cs;
	}
	
	public boolean isOMCPlugin() {
		return frame==null;
	}
	
	/**
	 * Add a new selector tab.
	 * The name of the new tab is set to be the name of the tab
	 * 
	 *@param tab The panel to show in the new tab
	 *@throws InvalidLogPaneException If the name of the panel is empty or null
	 *@throws LogPaneAlreadyExistException If a tab with the given name already exist
	 */
	public void addLogSelectorTab(LogLevelSelectorPanel logTab) throws  InvalidLogPaneException, LogPaneAlreadyExistException {
		if (logTab==null) {
			throw new IllegalArgumentException("Invalid null component");
		}
		if (logTab.getName()==null || logTab.getName().isEmpty()) {
			throw new InvalidLogPaneException("Trying to add a panel with no name");
		}
		// Check if a tab with this name already exist
		String name=logTab.getName();
		for (int t=0; t<getTabCount(); t++) {
			if (getComponentAt(t).getName().equals(name)) {
				throw new LogPaneAlreadyExistException("A log with the name "+name+" is already present");
			}
		}
		// Add the tab
		class TabInserter extends Thread {
			LogLevelSelectorPanel tabContent;
			LogLevelPanel thePane;
			public TabInserter(LogLevelSelectorPanel t, LogLevelPanel pane) {
				tabContent=t;
				thePane=pane;
			}
			public void run() {
				add(tabContent, new JLabel(tabContent.getName()));
				setTabComponentAt(indexOfComponent(tabContent), new ButtonTabComponent(thePane,tabContent));
			}
		}
		SwingUtilities.invokeLater(new TabInserter(logTab,this));
	}
	
	/**
	 * Remove the tab with the given name
	 * 
	 * @param name The name of the panel to remove
	 * @throw LogPaneNotFoundException If the pane with the given name does not exist
	 */
	public void removeLogSelectorTab(String name) throws LogPaneNotFoundException {
		if (name==null || name.isEmpty()) {
			throw new IllegalArgumentException("Invalid name of the tab to remove "+name);
		}
		// Look for the tab with the given name
		for (int t=0; t<getTabCount(); t++) {
			if (getComponentAt(t).getName().equals(name)) {
				class TabRemover extends Thread {
					int index;
					public TabRemover(int tabIndex) {
						index=tabIndex;
					}
					public void run() {
						remove(index);
					}
				}
				SwingUtilities.invokeLater(new TabRemover(t));
				return;
			}
		}
		// No tab with the given name has been found
		throw new LogPaneNotFoundException("The tab with name "+name+" does not exist");
	}
	
	/**
	 * Select the tab with the given name 
	 * 
	 * @param name The name of the tab to select
	 * @thorws LogPaneNotFoundException If the tab with the given name does not exist
	 */
	public void showTab(String name) throws LogPaneNotFoundException{
		if (name==null || name.isEmpty()) {
			throw new IllegalArgumentException("Invalid name of the tab to remove "+name);
		}
		// Look for the tab with the given name
		for (int t=0; t<getTabCount(); t++) {
			if (getComponentAt(t).getName().equals(name)) {
				setSelectedIndex(t);
				return;
			}
		}
		// No tab with the given name has been found
		throw new LogPaneNotFoundException("The tab with name "+name+" does not exist");
	}
}