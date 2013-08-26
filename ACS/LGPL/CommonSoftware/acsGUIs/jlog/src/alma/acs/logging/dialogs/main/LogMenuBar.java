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
package alma.acs.logging.dialogs.main;

import java.awt.Toolkit;
import java.awt.event.ActionListener;

import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.JSeparator;
import javax.swing.KeyStroke;
import javax.swing.event.MenuListener;

import com.cosylab.logging.engine.audience.Audience.AudienceInfo;
import com.cosylab.logging.engine.log.LogTypeHelper;

/**
 * The menu bar of the main GUI
 * 
 * @author acaproni
 *
 */
public class LogMenuBar extends JMenuBar {
	
	// Connect or disconnect text depending on the status of the connection
	// The text is changed by checking the connection before displaying the menu
	private JMenuItem connectMenuItem = null; 
	
	private JMenuItem loadMenuItem = null; // Load File
	private JMenuItem loadURLMenuItem = null; // Load from URL
	private JMenuItem loadDBMenuItem = null; // Load from database
	private JMenuItem saveFileMenuItem = null; // Save File As
	private JMenuItem clearLogsMI = null; // Clear All
	private JMenuItem exitMenuItem = null; // Exit Not visible per default
	private JSeparator exitSeparator = new JSeparator(); //Not visible per default
	private boolean exitHided; // true if the exit menu is not visible
	
	/**
	 * The menu item to show the statistics dialog
	 */
	private JMenuItem statisticsMenuItem = null;
	
	/**
     * The menu item to show the error log window
     */
    private JMenuItem viewErrorLogMI;
    
    /**
     * The menu item to show the error browser
     */
    private JMenuItem viewErrorBrowserMI;
    
	/**
	 * The menu item to show the filters control panel
	 */
	private JMenuItem filtersMenuItem;
	
	private JMenuItem fieldsMenuItem = null; // Fields...

	private JMenuItem searchMenuItem; // Search...
    private JMenuItem searchNextMenuItem; // Search Next

	private JMenu fileMenu; // File
	private JMenu viewMenu; // View
    private JMenu searchMenu; // Search
    private JMenu zoomMenu; // Zoom
    private JMenu expertMenu; // Expert
    
    private JMenuItem suspendMI;
    private JMenuItem prefsMI;
    
    /**
     * The menu item to show/hide the toolbar
     */
    private JCheckBoxMenuItem viewToolbarMI;
    
    /**
     * The menu item to show/hide the toolbar
     */
    private JCheckBoxMenuItem autoReconnectMI;
    
    /**
     * The menu item to show/hide the Detailed log info panel
     */
    private JCheckBoxMenuItem viewDetailedInfoMI;
    
    /**
     * The menu item to show/hide the Detailed log info panel
     */
    private JCheckBoxMenuItem viewStatusAreaMI;
    
    /**
     * The menu to select the operator mode
     */
    private JRadioButtonMenuItem operatorMode;
    
    /**
     * The menu to select the engineering mode
     */
    private JRadioButtonMenuItem engineeringMode;
    
    /**
     * The menu to select the SciLog mode
     */
    private JRadioButtonMenuItem sciLogMode;
    
    /**
     * The menu to add filters to the engine
     */
    private JMenuItem engineFiltersMI;
    
    /**
     * The menu item to select the format of the date column in the table of logs
     * If it is true, the date appear as hh:mm:ss otherwise it's shown with a complete
     * longest format
     * shortDateViewMI defaults to true
     */
    private JCheckBoxMenuItem shortDateViewMI;
    
    /**
     * The menu item to select to show only the icon in the renderer of the type
     * of log i.e. the description is hidden
     */
    private JCheckBoxMenuItem logTypeDescritptionViewMI;
    
    /**
     * The preferences for zooming
     */
    private JMenuItem zoomPrefsMI;
    
    /**
     * The menu for performing the zoom of an interval
     * selected by the user
     */
    private JMenuItem manualZoomMI;
	
	/**
	 * Constructor
	 */
	public LogMenuBar() {
		super();
		setName("LoggingClientMenuBar");
		setupMenuBar();
		hideExitMenu(true); // Hide the exit menu
	}
	
	/**
     * Builds the menu bar
     */
    private void setupMenuBar() {
        // Add the File menu
        fileMenu = new javax.swing.JMenu();
        fileMenu.setName("FileMenu");
        fileMenu.setText("File");
        fileMenu.add(getConnectMenuItem());
        fileMenu.add(getAutoReconnectMenuItem());
        fileMenu.addSeparator();
        fileMenu.add(getLoadMenuItem());
        fileMenu.add(getLoadURLMenuItem());
        fileMenu.add(getLoadDBMenuItem());
        fileMenu.add(getSaveFileMenuItem());
        fileMenu.add(getClearLogsMenuItem());
        //fileMenu.addSeparator();
        fileMenu.add(exitSeparator);
        fileMenu.add(getExitMenuItem());
        add(fileMenu);
        
        // Add the View Menu
        viewMenu = new javax.swing.JMenu();
        viewMenu.setName("ViewMenu");
        viewMenu.setText("View");
        
        
        viewMenu.add(getViewToolbarMenuItem());
        viewMenu.add(getViewDetailedInfoMenuItem());
        viewMenu.add(getViewStatusAreaMenuItem());
        viewMenu.addSeparator();
        viewMenu.add(getFieldsMenuItem());
        viewMenu.add(getShortDateViewMenuItem());
        viewMenu.add(getLogTypeDescriptionViewMenuItem());
        viewMenu.addSeparator();
        viewMenu.add(getFiltersMenuItem());
        viewMenu.add(getViewErrorBrowserMenuItem());
        viewMenu.addSeparator();
        viewMenu.add(getStatisticsMenuItem());
        viewMenu.add(getViewErrorLogMenuItem());
        
        add(viewMenu);
        
        // Add the Search Menu
        add(getSearchMenu());
        
        // Add the zoom menu
        add(getZoomMenu());
        
        // Add the expert menu
        expertMenu = new JMenu();
        expertMenu.setName("ExpertMenu");
        expertMenu.setText("Expert");
        JMenu modeMenu =new JMenu("Mode");
        expertMenu.add(modeMenu);
        modeMenu.add(getOperatorMode());
        modeMenu.add(getEngineeringMode());
        modeMenu.add(getSciLogMode());
        ButtonGroup group = new ButtonGroup();
        group.add(getOperatorMode());
        group.add(getEngineeringMode());
        group.add(getSciLogMode());
        expertMenu.addSeparator();
        expertMenu.add(getSuspendMenuItem());
        expertMenu.add(getPrefsMenuItem());
        expertMenu.add(getEngineFiltersMenuItem());
        add(expertMenu);
    }
    
    /**
     * Build the search menu
     * @return
     */
    public JMenu getSearchMenu() {
    	if (searchMenu==null) {
	    	searchMenu = new JMenu();
	        searchMenu.setName("SearchMenu");
	        searchMenu.setText("Search");
	        searchMenu.add(getSearchMenuItem());
	        searchMenu.add(getSearchNextMenuItem());
    	}
    	return searchMenu;
    }
    
    /**
     * Build the zoom menu.
     * @return
     */
    public JMenu getZoomMenu() {
    	if (zoomMenu==null) {
    		zoomMenu = new JMenu("Drill down");
    		zoomPrefsMI = new JMenuItem("Preferences...");
    		zoomMenu.add(zoomPrefsMI);
    		manualZoomMI= new JMenuItem("Drill down...");
    		zoomMenu.add(manualZoomMI);
    	}
    	return zoomMenu;
    }
    
    /**
	 * Returns the NewMenuItem property value.
	 * @return javax.swing.JMenuItem
	 */
	public JMenuItem getConnectMenuItem()
	{
		if (connectMenuItem == null) {
			ImageIcon icon =new ImageIcon(LogTypeHelper.class.getResource("/link.png"));
			connectMenuItem = new JMenuItem("Connect",icon);
			connectMenuItem.setName("connectMenuItem");
			connectMenuItem.setText("Connect");
		}
		return connectMenuItem;
	}
	
	/**
	 * Returns the LoadDB menu item
	 * 
	 * @return The LoadDB menu item
	 */
	public JMenuItem getLoadDBMenuItem() {
		if (loadDBMenuItem == null) {
			ImageIcon icon =new ImageIcon(LogTypeHelper.class.getResource("/database.png"));
			loadDBMenuItem = new JMenuItem("Load from database",icon);
			loadDBMenuItem.setName("LoadDBMenuItem");
		}
		return loadDBMenuItem;
	}

	/**
	 * Return the LoadURL menu item
	 * 
	 * @return The LoadURL menu item
	 */
	public JMenuItem getLoadURLMenuItem() {
		if (loadURLMenuItem == null) {
			ImageIcon icon =new ImageIcon(LogTypeHelper.class.getResource("/loadURL.png"));
			loadURLMenuItem = new JMenuItem("Load from URL",icon);
			loadURLMenuItem.setName("LoadURLMenuItem");

		}
		return loadURLMenuItem;
	}
	
	/**
	 * Returns the Fields menu item.
	 * @return javax.swing.JMenuItem
	 */

	public JMenuItem getFieldsMenuItem() {
		if (fieldsMenuItem == null) {
			fieldsMenuItem = new JMenuItem();
			fieldsMenuItem.setName("FieldsMenuItem");
			fieldsMenuItem.setText("Fields...");
		}
		return fieldsMenuItem;
	}
	
	/**
	 * Return the editFiltersMenuItem
	 * @return
	 */
	public JMenuItem getFiltersMenuItem() {
		if (filtersMenuItem==null) {
			ImageIcon filterIcon = new ImageIcon(LogTypeHelper.class.getResource("/filters.png"));
			filtersMenuItem=new JMenuItem("Filters...",filterIcon);
			filtersMenuItem.setName("EditFiltersMenuItem");
		}
		return filtersMenuItem;
	}
	
	/**
	 * Returns the LoadMenuItem property value.
	 * @return javax.swing.JMenuItem
	 */
	public JMenuItem getLoadMenuItem() {
		if (loadMenuItem == null) {
			ImageIcon icon =new ImageIcon(LogTypeHelper.class.getResource("/load.png"));
			loadMenuItem = new JMenuItem("Load from File",icon);
			loadMenuItem.setName("LoadMenuItem");
		}
		return loadMenuItem;
	}
	
	/**
	 * Returns the ExitMenuItem property value.
	 * @return javax.swing.JMenuItem
	 */
	public JMenuItem getExitMenuItem() {
		if (exitMenuItem == null) {
			ImageIcon icon =new ImageIcon(LogTypeHelper.class.getResource("/exit.png"));
			exitMenuItem = new JMenuItem("Exit",icon);
			exitMenuItem.setName("ExitMenuItem");
		}
		return exitMenuItem;
	}
	
	/**
	 * Returns the SaveFileMenuItem property value.
	 * @return javax.swing.JMenuItem
	 */

	public JMenuItem getSaveFileMenuItem() {
		if (saveFileMenuItem == null) {
			ImageIcon icon =new ImageIcon(LogTypeHelper.class.getResource("/disk.png"));
			saveFileMenuItem = new JMenuItem("Save File As",icon);
			saveFileMenuItem.setName("SaveFileMenuItem");
		}
		return saveFileMenuItem;
	}
	
	/**
	 * Returns the ClearAllMenuItem property value.
	 * @return javax.swing.JMenuItem
	 */
	public JMenuItem getClearLogsMenuItem() {
		if (clearLogsMI == null) {
			ImageIcon icon =new ImageIcon(LogTypeHelper.class.getResource("/delete.png"));
			clearLogsMI = new JMenuItem("Clear logs",icon);
			clearLogsMI.setName("ClearAllMenuItem");
		}
		return clearLogsMI;
	}
	
	public JMenuItem getSearchMenuItem() {
		if (searchMenuItem==null) {
			ImageIcon searchIcon =new ImageIcon(LogTypeHelper.class.getResource("/search.png"));
	        searchMenuItem = new JMenuItem("Search...",searchIcon);
	        searchMenuItem.setAccelerator(KeyStroke.getKeyStroke('S',Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
	        searchMenu.add(searchMenuItem);
	        searchMenu.setName("SearchMenuItem");
		}
		return searchMenuItem;
	}
	
	public JMenuItem getSearchNextMenuItem() {
		if (searchNextMenuItem==null) {
			searchNextMenuItem = new JMenuItem("Search Next");
			searchNextMenuItem.setAccelerator(KeyStroke.getKeyStroke('N',Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
			searchNextMenuItem.setEnabled(false);
			searchNextMenuItem.setName("SearchNextMenuItem");
		}
		return searchNextMenuItem;
	}
	
	/**
	 * Set the event handler for the menus
	 * 
	 * @param listener The MenuListener
	 */
	public void setEventHandler(ActionListener actionListener, MenuListener menuListener) {
		fileMenu.addMenuListener(menuListener);
		viewMenu.addMenuListener(menuListener);
		searchMenu.addMenuListener(menuListener);
		
		connectMenuItem.addActionListener(actionListener); // Connect/Disconnect
		autoReconnectMI.addActionListener(actionListener);
		viewToolbarMI.addActionListener(actionListener);
		viewDetailedInfoMI.addActionListener(actionListener);
		shortDateViewMI.addActionListener(actionListener);
		logTypeDescritptionViewMI.addActionListener(actionListener);
		viewStatusAreaMI.addActionListener(actionListener);
		statisticsMenuItem.addActionListener(actionListener);
		viewErrorLogMI.addActionListener(actionListener);
		viewErrorBrowserMI.addActionListener(actionListener);
		searchMenuItem.addActionListener(actionListener);
		searchNextMenuItem.addActionListener(actionListener);
		suspendMI.addActionListener(actionListener);
		prefsMI.addActionListener(actionListener);
		loadMenuItem.addActionListener(actionListener); // Load File
		loadURLMenuItem.addActionListener(actionListener); // Load URL
		loadDBMenuItem.addActionListener(actionListener); // Load from database
		saveFileMenuItem.addActionListener(actionListener); // Save File As
		clearLogsMI.addActionListener(actionListener); // ClearAll
		exitMenuItem.addActionListener(actionListener); // Exit
		fieldsMenuItem.addActionListener(actionListener); // Fields
		filtersMenuItem.addActionListener(actionListener); // Filters
		
		operatorMode.addActionListener(actionListener); // Operator mode
		engineeringMode.addActionListener(actionListener); // Engineering mode
		sciLogMode.addActionListener(actionListener); // SciLog mode
		engineFiltersMI.addActionListener(actionListener); // Engine filters
		
		zoomPrefsMI.addActionListener(actionListener); // zoom preferences
		manualZoomMI.addActionListener(actionListener); // manual zoom
		
	}
	
	/**
	 * Getter 
	 * 
	 * @return The menu item
	 */
	public JCheckBoxMenuItem getViewToolbarMenuItem() {
		if (viewToolbarMI==null) {
			viewToolbarMI = new JCheckBoxMenuItem("Toolbar",true);
		}
		return viewToolbarMI;
	}
	
	/**
	 * Getter 
	 * 
	 * @return The menu item
	 */
	public JCheckBoxMenuItem getViewDetailedInfoMenuItem() {
		if (viewDetailedInfoMI==null) {
			viewDetailedInfoMI = new JCheckBoxMenuItem("Detailed log info",true);
		}
		return viewDetailedInfoMI;
	}
	
	/**
	 * Getter 
	 * 
	 * @return The menu item
	 */
	public JCheckBoxMenuItem getViewStatusAreaMenuItem() {
		if (viewStatusAreaMI==null) {
			viewStatusAreaMI = new JCheckBoxMenuItem("Status area",false);
		}
		return viewStatusAreaMI;
	}
	
	/**
	 * Getter 
	 * 
	 * @return The menu item
	 */
	public JCheckBoxMenuItem getShortDateViewMenuItem() {
		if (shortDateViewMI==null) {
			shortDateViewMI = new JCheckBoxMenuItem("Short date format",true);
		}
		return shortDateViewMI;
	}
	
	/**
	 * Getter 
	 * 
	 * @return The menu item
	 */
	public JCheckBoxMenuItem getLogTypeDescriptionViewMenuItem() {
		if (logTypeDescritptionViewMI==null) {
			logTypeDescritptionViewMI = new JCheckBoxMenuItem("Show log type description",true);
		}
		return logTypeDescritptionViewMI;
	}
	
	/**
	 * Getter 
	 * 
	 * @return The menu item
	 */
	public JCheckBoxMenuItem getAutoReconnectMenuItem() {
		if (autoReconnectMI==null) {
			ImageIcon icon =new ImageIcon(LogTypeHelper.class.getResource("/autoreconnect.png"));
	        autoReconnectMI = new JCheckBoxMenuItem("Auto reconnect",icon,false);
		}
		return autoReconnectMI;
	}
	
	/**
	 * Getter 
	 * 
	 * @return The menu item
	 */
	public JMenuItem getSuspendMenuItem() {
		if (suspendMI==null) {
			suspendMI = new JCheckBoxMenuItem("Suspend",false);
		}
		return suspendMI;
	}
	
	/**
	 * Getter 
	 * 
	 * @return The menu item
	 */
	public JMenuItem getPrefsMenuItem() {
		if (prefsMI==null) {
			prefsMI = new JMenuItem("Preferences");
		}
		return prefsMI;
	}
	
	/**
	 * Getter 
	 * 
	 * @return The menu item
	 */
	public JMenuItem getStatisticsMenuItem() {
		if (statisticsMenuItem==null) {
			ImageIcon statIcon =new ImageIcon(LogTypeHelper.class.getResource("/statistics.png"));
			statisticsMenuItem = new JMenuItem("Statistics",statIcon);
		}
		return statisticsMenuItem;
	}
	
	/**
	 * Getter 
	 * 
	 * @return The menu item
	 */
	public JMenuItem getViewErrorLogMenuItem() {
		if (viewErrorLogMI==null) {
			ImageIcon errorLogIcon =new ImageIcon(LogTypeHelper.class.getResource("/errorLogIcon.png"));
			viewErrorLogMI = new JMenuItem("jlog error log",errorLogIcon);
		}
		return viewErrorLogMI;
	}
	
	/**
	 * Getter 
	 * 
	 * @return The menu item
	 */
	public JMenuItem getViewErrorBrowserMenuItem() {
		if (viewErrorBrowserMI==null) {
			ImageIcon browserIcon =new ImageIcon(LogTypeHelper.class.getResource("/errorBrowserIcon.png"));
			viewErrorBrowserMI = new JMenuItem("Error browser",browserIcon);
		}
		return viewErrorBrowserMI;
	}
	
	/**
	 * Enable/Disable all the controls in the GUI that could trigger
	 * the invalidation of the logs
	 * 
	 * @param enabled If true the controls are enabled
	 */
	@Override
	public void setEnabled(boolean enabled) {
		viewMenu.setEnabled(enabled);
		searchMenu.setEnabled(enabled);
		expertMenu.setEnabled(enabled);
		fileMenu.setEnabled(enabled);
		zoomMenu.setEnabled(enabled);
		super.setEnabled(enabled);
	}
	
	/**
	 * Hide the Exit menu item
	 * 
	 * @param hide If true the menu is set to invisible
	 */
	public void hideExitMenu(boolean hide) {
		if (hide && !exitHided) {
			exitHided=true;
			fileMenu.remove(exitMenuItem);
			fileMenu.remove(exitSeparator);
		} else if (!hide && exitHided){
			exitHided=false;
			fileMenu.add(exitSeparator);
			fileMenu.add(exitMenuItem);
		}
	}

	/**
	 * @return the operatorMode
	 */
	public JRadioButtonMenuItem getOperatorMode() {
		if (operatorMode==null) {
			operatorMode= new JRadioButtonMenuItem(AudienceInfo.OPERATOR.name);
		}
		return operatorMode;
	}

	/**
	 * @return the standardMode
	 */
	public JRadioButtonMenuItem getEngineeringMode() {
		if (engineeringMode==null) {
			engineeringMode=new JRadioButtonMenuItem(AudienceInfo.ENGINEER.name);
		}
		return engineeringMode;
	}
	
	/**
	 * @return the SciLog mode
	 */
	public JRadioButtonMenuItem getSciLogMode() {
		if (sciLogMode==null) {
			sciLogMode=new JRadioButtonMenuItem(AudienceInfo.SCILOG.name);
		}
		return sciLogMode;
	}

	/**
	 * @return the engineFiltersMI
	 */
	public JMenuItem getEngineFiltersMenuItem() {
		if (engineFiltersMI==null) {
			engineFiltersMI = new JMenuItem("Engine filters...");
		}
		return engineFiltersMI;
	}

	/**
	 * 
	 * @return The manual zoom menu item
	 */
	public JMenuItem getManualZoomMI() {
		return manualZoomMI;
	}

	/**
	 * @return The zoom preferences menu item
	 */
	public JMenuItem getZoomPrefsMI() {
		return zoomPrefsMI;
	}
	
}
