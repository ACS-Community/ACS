package alma.acs.logging.dialogs.main;

import java.awt.Toolkit;
import java.awt.event.ActionListener;

import javax.swing.ImageIcon;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;
import javax.swing.event.MenuListener;

import com.cosylab.logging.engine.log.LogTypeHelper;

public class LogMenuBar extends JMenuBar {
	
	// Connect or disconnect text depending on the status of the connection
	// The text is changed by checking the connection before displaying the menu
	private JMenuItem connectMenuItem = null; 
	
	private JMenuItem loadMenuItem = null; // Load File
	private JMenuItem loadURLMenuItem = null; // Load from URL
	private JMenuItem loadDBMenuItem = null; // Load from database
	private JMenuItem saveFileMenuItem = null; // Save File As
	private JMenuItem clearLogsMI = null; // Clear All
	private JMenuItem exitMenuItem = null; // Exit
	
	/**
	 * The menu item to load filters
	 */
	private JMenuItem loadFiltersMenuItem = null;
	
	/**
	 * The menu item to save the filters
	 */
	private JMenuItem saveFiltersMenuItem = null;
	
	/**
	 * The menu item to show the statistics dialog
	 */
	private JMenuItem statisticsMenuItem = null;
	
	/**
     * The menu item to show the error log window
     */
    private JMenuItem viewErrorLogMI;
    
	/** 
	 * The filters menu
	 */
	private JMenu filtersMenu;
	
	/**
	 * The menu item to save the filters with a new name
	 */
	private JMenuItem saveAsFiltersMenuItem = null;
	
	/**
	 * The menu item to edit the filters
	 */
	private JMenuItem editFiltersMenuItem;
	
	private JMenuItem fieldsMenuItem = null; // Fields...

	private JMenuItem searchMenuItem; // Search...
    private JMenuItem searchNextMenuItem; // Search Next

	private JMenu fileMenu; // File
	private JMenu viewMenu; // View
    private JMenu searchMenu; // Search
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
     * The menu item to select the format of the date column in the table of logs
     * If it is true, the date appear as hh:mm:ss otherwise it's shown with a complet
     * longest format
     * shortDateViewMI defaults to true
     */
    private JCheckBoxMenuItem shortDateViewMI;
	
	/**
	 * Constructor
	 */
	public LogMenuBar() {
		super();
		setName("LoggingClientMenuBar");
		setupMenuBar();
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
        fileMenu.addSeparator();
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
        viewMenu.addSeparator();
        viewMenu.add(getFiltersMenu());
        viewMenu.addSeparator();
        viewMenu.add(getStatisticsMenuItem());
        viewMenu.add(getViewErrorLogMenuItem());
        
        add(viewMenu);
        
        // Add the Search Menu
        getSearchMenu();
        add(searchMenu);
        
        // Add the expert menu
        expertMenu = new JMenu();
        expertMenu.setName("ExpertMenu");
        expertMenu.setText("Expert");
        expertMenu.add(getSuspendMenuItem());
        expertMenu.add(getPrefsMenuItem());
        add(expertMenu);
    }
    
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
	 * Returns the FiltersMenuItem property value.
	 * @return javax.swing.JMenuItem
	 */
	public JMenu getFiltersMenu() {
		filtersMenu = new JMenu("Filters");
	
		filtersMenu.add( getLoadFiltersMenuItem());
		filtersMenu.add(getSaveFiltersMenuItem());
		filtersMenu.add(getSaveAsFiltersMenuItem());
		filtersMenu.add(getEditFiltersMenuItem());
		
		return filtersMenu; 
	}
	
	/**
	 * Return the loadFiltersMenuItem
	 * @return
	 */
	public JMenuItem getSaveAsFiltersMenuItem() {
		if (saveAsFiltersMenuItem==null) {
			saveAsFiltersMenuItem=new JMenuItem("Save As,,,");
			saveAsFiltersMenuItem.setName("LoadFiltersMenuItem");
		}
		return saveAsFiltersMenuItem;
	}
	
	
	/**
	 * Return the loadFiltersMenuItem
	 * @return
	 */
	public JMenuItem getSaveFiltersMenuItem() {
		if (saveFiltersMenuItem==null) {
			saveFiltersMenuItem=new JMenuItem("Save");
			saveFiltersMenuItem.setName("LoadFiltersMenuItem");
		}
		return saveFiltersMenuItem;
	}
	
	/**
	 * Return the loadFiltersMenuItem
	 * @return
	 */
	public JMenuItem getLoadFiltersMenuItem() {
		if (loadFiltersMenuItem==null) {
			loadFiltersMenuItem=new JMenuItem("Load");
			loadFiltersMenuItem.setName("LoadFiltersMenuItem");
		}
		return loadFiltersMenuItem;
	}
	
	/**
	 * Return the editFiltersMenuItem
	 * @return
	 */
	public JMenuItem getEditFiltersMenuItem() {
		if (editFiltersMenuItem==null) {
			editFiltersMenuItem=new JMenuItem("Edit...");
			editFiltersMenuItem.setName("EditFiltersMenuItem");
		}
		return editFiltersMenuItem;
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
		filtersMenu.addMenuListener(menuListener);
		
		connectMenuItem.addActionListener(actionListener); // Connect/Disconnect
		autoReconnectMI.addActionListener(actionListener);
		viewToolbarMI.addActionListener(actionListener);
		viewDetailedInfoMI.addActionListener(actionListener);
		shortDateViewMI.addActionListener(actionListener);
		viewStatusAreaMI.addActionListener(actionListener);
		statisticsMenuItem.addActionListener(actionListener);
		viewErrorLogMI.addActionListener(actionListener);
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
		editFiltersMenuItem.addActionListener(actionListener); // Edit Filters
		loadFiltersMenuItem.addActionListener(actionListener); // Load Filters
		saveFiltersMenuItem.addActionListener(actionListener); // Save Filters
		saveAsFiltersMenuItem.addActionListener(actionListener); // Save Filters
	}
	
	public JCheckBoxMenuItem getViewToolbarMenuItem() {
		if (viewToolbarMI==null) {
			viewToolbarMI = new JCheckBoxMenuItem("Toolbar",true);
		}
		return viewToolbarMI;
	}
	
	public JCheckBoxMenuItem getViewDetailedInfoMenuItem() {
		if (viewDetailedInfoMI==null) {
			viewDetailedInfoMI = new JCheckBoxMenuItem("Detailed log info",true);
		}
		return viewDetailedInfoMI;
	}
	
	public JCheckBoxMenuItem getViewStatusAreaMenuItem() {
		if (viewStatusAreaMI==null) {
			viewStatusAreaMI = new JCheckBoxMenuItem("Status area",true);
		}
		return viewStatusAreaMI;
	}
	
	public JCheckBoxMenuItem getShortDateViewMenuItem() {
		if (shortDateViewMI==null) {
			shortDateViewMI = new JCheckBoxMenuItem("Short date format",true);
		}
		return shortDateViewMI;
	}
	
	public JCheckBoxMenuItem getAutoReconnectMenuItem() {
		if (autoReconnectMI==null) {
			ImageIcon icon =new ImageIcon(LogTypeHelper.class.getResource("/autoreconnect.png"));
	        autoReconnectMI = new JCheckBoxMenuItem("Auto reconnect",icon,false);
		}
		return autoReconnectMI;
	}
	
	public JMenuItem getSuspendMenuItem() {
		if (suspendMI==null) {
			suspendMI = new JCheckBoxMenuItem("Suspend",false);
		}
		return suspendMI;
	}
	
	public JMenuItem getPrefsMenuItem() {
		if (prefsMI==null) {
			prefsMI = new JMenuItem("Preferences");
		}
		return prefsMI;
	}
	
	public JMenuItem getStatisticsMenuItem() {
		if (statisticsMenuItem==null) {
			ImageIcon statIcon =new ImageIcon(LogTypeHelper.class.getResource("/statistics.png"));
			statisticsMenuItem = new JMenuItem("Statistics",statIcon);
		}
		return statisticsMenuItem;
	}
	
	public JMenuItem getViewErrorLogMenuItem() {
		if (viewErrorLogMI==null) {
			viewErrorLogMI = new JMenuItem("Error log");
		}
		return viewErrorLogMI;
	}
}
