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
package com.cosylab.logging.settings;

import java.awt.BorderLayout;
import java.awt.Dialog;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JToolBar;
import javax.swing.ListSelectionModel;
import javax.swing.WindowConstants;
import javax.swing.filechooser.FileFilter;

import com.cosylab.gui.components.r2.CheckListModel;
import com.cosylab.gui.components.r2.JCheckList;
import com.cosylab.logging.LoggingClient;
import com.cosylab.logging.engine.Filter;
import com.cosylab.logging.engine.Filterable;
import com.cosylab.logging.engine.FiltersVector;

/**
 * Serves the purpose of selecting the right filters.
 * Filters are used to display logs in the table according to the user's preferences.
 * They are used by the engine too.
 * 
 * <p>
 * Creation date: (1/2/2002 22:53:33)
 * 
 * @author: Ales Pucelj (ales.pucelj@kgb.ijs.si)
 */
public class FilterChooserDialog extends JDialog {

	private JCheckList filterList = null;

	private JLabel description = null;

	private JButton buttonClose = null;

	private JButton buttonApply = null;
	
	private JButton buttonRestore = null;
	
	// The toolbar with its buttons
	private JToolBar toolBar = new JToolBar();
	private JButton buttonAdd = null;
	private JButton buttonRemove = null;
	private JButton buttonModify = null;
	
	// The menu bar with its items
	private JMenuBar menuBar = new JMenuBar();
	private JMenuItem loadMI=new JMenuItem("Load");
	private JMenuItem saveMI=new JMenuItem("Save");
	private JMenuItem saveAsMI=new JMenuItem("Save as");
	private JMenuItem closeMI=new JMenuItem("Close");
	private JMenuItem activateAllMI=new JMenuItem("Activate all");
	private JMenuItem deactivateAllMI=new JMenuItem("Deactivate all");
	private JMenuItem clearAllMI=new JMenuItem("Clear all");
	
	// The listener
	private ButtonListener bl = new ButtonListener();
	
	// The logging client
	private LoggingClient loggingClient;
	
	// The Filterable object to apply filters to
	private Filterable filterable=null;
	
	// The filters whose description appears in the
	// filter list
	// We instantiate a FiltersVector to use its load/save capabilities
	private FiltersVector filters=new FiltersVector();
	
	// The filters defined at startup (used to implement
	// the restore)
	private FiltersVector initialFilters = new FiltersVector();
	
	// true if the vector of filters has been modified
	private boolean modified=false;
	
	/**
	 * The name of the last save/load filter file
	 * (to implement the save as option)
	 */
	private String filterFileName = null;

	/**
	 * The cobject that receives the events generated
	 * by the button 
	 * 
	 * @author acaproni
	 *
	 */
	private class ButtonListener implements ActionListener {
		private FilterChooserDialog fcd = FilterChooserDialog.this;

		private Filter editFilter(Filter f) {
			FilterParameterDialog fpd = new FilterParameterDialog(FilterChooserDialog.this);
			fpd.setFilter(f);
			fpd.setVisible(true);
			if (fpd.okPressed()) {
				return fpd.getFilter();
			}
			return null;
		}

		/**
		 * @see <code>ActionListener</code>
		 * @see <code>ActionEvent</code>
		 */
		public void actionPerformed(ActionEvent e) {
			if (e.getSource() == buttonClose || e.getSource() == closeMI) { // Close
				if (modified) {
					int ret = JOptionPane.showConfirmDialog(null,
							"<HTML>You have modified the filters withouth saving/applying.<BR>Do you really want to close?</HTML>",
							"Close confimation", JOptionPane.YES_NO_OPTION);
					if (ret != JOptionPane.YES_OPTION) {
						return;
					}
				}
				setVisible(false);
				loggingClient.enableFiltersWidgets(true);
				return;
			} else if (e.getSource() == buttonModify) { // Modify/Edit
				CheckListModel clm = (CheckListModel) fcd.filterList.getModel();
				int i = fcd.filterList.getSelectedIndex();
				if (i >= 0) {
					boolean isItemChecked = clm.isChecked(i);

					if (i > -1) {
						Filter f = (Filter) clm.get(i);
						f = editFilter(f);
						if (f != null) {
							clm.set(i, f);
							filters.set(i, f);
						}
					}
					clm.setChecked(i, isItemChecked);
					modified=true;
				} else {
					JOptionPane.showMessageDialog(FilterChooserDialog.this,
							"Invalid item to modify", "Error",
							JOptionPane.ERROR_MESSAGE);
				}
			} else if (e.getSource() == buttonAdd) { // Add
				Filter f = editFilter(null);
				if (f != null) {
					CheckListModel clm = (CheckListModel) fcd.filterList.getModel();
					clm.addElement(f);
					clm.setChecked(clm.getSize() - 1, true);
					filters.add(f);
					modified=true;
				}
			} else if (e.getSource() == buttonRemove) { // Remove
				int i = fcd.filterList.getSelectedIndex();
				if (i > -1)
					((CheckListModel) (fcd.filterList.getModel())).remove(i);
					filters.remove(i);
					modified=true;
			} else if (e.getSource() == activateAllMI) {
				CheckListModel clm = (CheckListModel) filterList.getModel();
				for (int i = 0; i < clm.getSize(); i++) {
					clm.setChecked(i, true); // Deactivate all the filters
					modified=true;
				}
			} else if (e.getSource() == deactivateAllMI) { // Deactivate All
				CheckListModel clm = (CheckListModel) filterList.getModel();
				for (int i = 0; i < clm.getSize(); i++) {
					clm.setChecked(i, false); // Deactivate all the filters
					modified=true;
				}
			} else if (e.getSource() == clearAllMI) { // Delete all
				CheckListModel clm = (CheckListModel) filterList.getModel();
				if (clm.getSize() > 0) {
					// Ask the user for a confirmation
					int ret = JOptionPane.showConfirmDialog(null,
							"Do you really want to delete all the filters?",
							"Delete all filters", JOptionPane.YES_NO_OPTION);
					if (ret == JOptionPane.YES_OPTION) {
						clm.clear();
						filters.clear();
						modified=true;
					}
				}
			}  else if (e.getSource() == buttonApply) { // Apply
				applyFilters();
				modified=false;
			} else if (e.getSource() == loadMI) { // Load
				loadFilters();
			} else if (e.getSource() == saveMI) { // Save
				saveFilters(filterFileName);
			} else if (e.getSource() == saveAsMI) { // Save as
				saveAsFilters();
			} else if (e.getSource() == buttonRestore) { // Restore
				restoreFilters();
			} else {
				System.out.println("Unhandled event: " + e);
			}
			FilterChooserDialog.this.updateButtons();
		}
	}
	
	/**
	 * The filter to load save filters as xml files
	 * The filter checks for the extension .xml in the name
	 * 
	 * @author acaproni
	 */
	private class XmlFileFilter extends FileFilter {
		public boolean accept(File f) {
			// Check if the name has the extension .xml at the end and is readable
			boolean pass = f.isFile() && f.getName().toUpperCase().endsWith(".XML") && f.canRead();
			// Check if the file is a directory
			pass = pass || f.isDirectory();
			// Check if f is a hidden file
			pass = pass && !f.isHidden();
			return pass; 
		}
		
		public String getDescription() {
			return "xml file";
		}
	}
	
	/**
	 * FilterChooserDialog constructor.
	 * 
	 * @param title The tile of the window
	 * @param logLci The loggingClient showing this dialog
	 * @param model The table model
	 */
	public FilterChooserDialog(String title, LoggingClient logCli, Filterable filterable) {
		super((Dialog)null,title);
		if (filterable == null) {
			throw new IllegalArgumentException(
					"Invalid null LogTableDataModel in constructor");
		}
		if (logCli == null) {
			throw new IllegalArgumentException(
					"Invalid null LoggingClient in constructor");
		}
		loggingClient = logCli;
		this.filterable=filterable;
		setModal(false);
		
		ImageIcon filterIcon = new ImageIcon(FilterChooserDialog.class.getResource("/filters.png"));
		setIconImage(filterIcon.getImage());
		initialize();
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
		addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent e) {
				loggingClient.enableFiltersWidgets(true);
			}
		});
	}
	
	/**
	 * Set the filters
	 * 
	 * @param flts The filters to set in the table
	 */
	public void setFilters(FiltersVector flts) {
		if (flts==null) {
			throw new IllegalArgumentException("Invalid null filter vector");
		}
		initialFilters.setFilters(flts);
		filters.setFilters(flts);
		setupFields(filters);
	}

	/**
	 * Init the GUI
	 */
	private void initialize() {
		setLocation(50, 50);
		
		// Add the toolbars
		initMenubar();
		initToolbar();
		
		JPanel panel = new JPanel(new GridBagLayout());

		GridBagConstraints c = new GridBagConstraints();

		description = new JLabel("Select filter to apply");
		c = newConstraints(0, 0, 0, 4);
		c.gridwidth = 3;
		panel.add(description, c);

		c = newConstraints(0, 1, 0, 4);
		c.gridwidth = 3;
		c.weighty = 1.0;
		c.fill = GridBagConstraints.BOTH;
		filterList = new JCheckList();
		panel.add(new JScrollPane(filterList), c);
		filterList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

		JSeparator sep = new JSeparator();
		GridBagConstraints constr = newConstraints(0, 2, 4, 4);
		constr.gridwidth = 3;
		panel.add(sep, constr);

		buttonApply = new JButton("Apply");
		buttonApply.addActionListener(bl);
		panel.add(buttonApply, newConstraints(0, 3, 4, 4));
		
		buttonRestore= new JButton("Restore");
		buttonRestore.addActionListener(bl);
		panel.add(buttonRestore, newConstraints(1, 3, 4, 4));

		buttonClose = new JButton("Close");
		buttonClose.addActionListener(bl);
		panel.add(buttonClose, newConstraints(2, 3, 4, 4));
		
		getContentPane().add(panel,BorderLayout.CENTER);
		pack();
	}

	/**
	 * Insert the method's description here.
	 * <p>
	 * Creation date: (1/2/2002 23:35:37)
	 * 
	 * @return boolean[]
	 */
	public boolean[] getChecked() {
		return filterList.getChecked();
	}

	/**
	 * Insert the method's description here. Creation date: (2/6/02 3:34:17 PM)
	 * 
	 * @return com.cosylab.logging.engine.Filter[]
	 */
	public Filter[] getFilters() {
		CheckListModel clm = (CheckListModel) (filterList.getModel());

		int l = clm.size();
		Filter[] returnValue = new Filter[l];

		for (int i = 0; i < l; i++) {
			returnValue[i] = (Filter) clm.get(i);
		}
		return returnValue;
	}

	/**
	 * Insert the method's description here. Creation date: (2/7/02 4:30:09 PM)
	 * 
	 * @return java.awt.GridBagConstraints
	 * @param x
	 *            int
	 * @param y
	 *            int
	 * @param top
	 *            int
	 * @param bottom
	 *            int
	 */
	protected GridBagConstraints newConstraints(int x, int y, int top,
			int bottom) {
		GridBagConstraints constraints = new GridBagConstraints();
		constraints.gridx = x;
		constraints.gridy = y;
		constraints.insets = new Insets(top, 4, bottom, 4);
		constraints.weightx = 1.0;
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.gridheight = 1;
		constraints.gridwidth = 1;

		return constraints;
	}

	/**
	 * Set the entries in the list of filters
	 * (one row per each filter) 
	 * 
	 * @param filters The list of filters
	 */
	private void setupFields(FiltersVector filters) {
		if (filters==null) {
			throw new IllegalArgumentException("The FiltersVector can't be null");
		}
		CheckListModel clm = (CheckListModel) filterList.getModel();
		clm.clear();

		for (int i = 0; i < filters.size(); i++) {
			clm.addElement(filters.get(i));
			clm.setChecked(i, false); // Deactivate all the filters
		}
		// Activate the selected filters
		if (filters.hasActiveFilters()) {
			int active[] = filters.getAppliedFiltersIndexes();
			if (active != null) {
				for (int i = 0; i < active.length; i++) {
					clm.setChecked(active[i], true);
				}
			}
		}

		updateButtons();
	}

	/**
	 * Insert the method's description here.
	 * <p>
	 * Creation date: (2/13/2002 18:35:12)
	 */
	protected void updateButtons() {
		boolean hasEntries = (filterList.getModel().getSize() > 0);
		buttonRemove.setEnabled(hasEntries);
		buttonModify.setEnabled(hasEntries);
		activateAllMI.setEnabled(hasEntries);
		deactivateAllMI.setEnabled(hasEntries);
		clearAllMI.setEnabled(hasEntries);
		saveMI.setEnabled(filterFileName!=null && hasEntries);
		saveAsMI.setEnabled(hasEntries);
	}

	/**
	 * Load filters from a XML file The user chooses if the loaded filters
	 * substitutes the existing ones or merges with them
	 */
	private void loadFilters() {
		boolean eraseOldFilters;
		// Check if already exists filters
		if (filters.size() > 0) {
			int ret = JOptionPane.showConfirmDialog(this,
					"Do you want do discard existing filters?",
					"Merge filters?", 
					JOptionPane.YES_NO_CANCEL_OPTION);
			if (ret == JOptionPane.CANCEL_OPTION) {
				return;
			}
			eraseOldFilters = (ret == JOptionPane.YES_OPTION);
		} else {
			eraseOldFilters = false; // We have no filters so.. nothing to
			  						// delete ;-)
		}

		// Show the dialog to choose the file to load
		JFileChooser fileChooserDlg = new JFileChooser();
		fileChooserDlg.setMultiSelectionEnabled(false);
		fileChooserDlg.setDialogTitle("Load filters");
		XmlFileFilter fileFilter = new XmlFileFilter();
		fileChooserDlg.setFileFilter(fileFilter);
		if (fileChooserDlg.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
			// Load filters from file
			File fileToLoad = fileChooserDlg.getSelectedFile();
			if (fileToLoad != null) {
				try {
					filters.loadFilters(fileToLoad,	eraseOldFilters, null);
				} catch (Throwable t) {
					JOptionPane.showMessageDialog(null, "Error: "+t.getMessage(), "Error loading filters", JOptionPane.ERROR_MESSAGE);
				}
				setupFields(filters);
				filterFileName = fileToLoad.getAbsolutePath();
			}
		}
	}

	/**
	 * Save the filters in a new XML file
	 * 
	 */
	private void saveAsFilters() {
		// Check if there are filters in use
		if (filters.size() == 0) {
			JOptionPane.showMessageDialog(this, "No filters to save",
					"Warning", JOptionPane.INFORMATION_MESSAGE);
			return;
		}
		// Show the dialog to choose the file to save
		JFileChooser fileChooserDlg = new JFileChooser();
		fileChooserDlg.setMultiSelectionEnabled(false);
		fileChooserDlg.setDialogTitle("Save filters");
		XmlFileFilter fileFilter = new XmlFileFilter();
		fileChooserDlg.setFileFilter(fileFilter);
		if (fileChooserDlg.showSaveDialog(this) == JFileChooser.APPROVE_OPTION) {
			// Get the selected file
			File fileToSave = fileChooserDlg.getSelectedFile();
			if (!fileToSave.getAbsolutePath().toUpperCase().endsWith(".XML")) {
				fileToSave = new File(fileToSave.getAbsolutePath() + ".xml");
			}
			if (fileToSave != null) {
				try {
					filters.saveFilters(fileToSave);
					filterFileName = fileToSave.getAbsolutePath();
				} catch (IOException e) {
					System.err.println("Exception: " + e.toString());
				}
			}
		}
	}

	/**
	 * Save the filters to a XML files with a given name It create the File
	 * object then call overloaded method
	 * 
	 * @param fileName The name of the xml file
	 */
	private void saveFilters(String fileName) {
		if (fileName==null || fileName.length()==0) {
			throw new IllegalArgumentException("Inavlid file name: "+fileName);
		}
		// Check if the name terminate with xml
		if (!fileName.toUpperCase().endsWith(".XML")) {
			fileName = fileName + ".xml";
		}
		CheckListModel clm = (CheckListModel) filterList.getModel();
		for (int t=0; t<clm.getSize(); t++) {
			Filter f = (Filter)clm.get(t);
			filters.activateFilter(f,clm.isChecked(t));
		}
		File f = new File(fileName);
		try {
			filters.saveFilters(f);
			filterFileName = f.getAbsolutePath();
			System.out.println("Saved " + filterFileName);
		} catch (IOException e) {
			System.err.println("Error opening " + fileName);
		}
	}
	
	/**
	 * Apply the filters in the table of logs
	 *
	 */
	private void applyFilters() {
		if (filterList.getModel().getSize()!=filters.size()) {
			throw new IllegalArgumentException("The filterList and the FiltersVector differ");
		}
		Filter[] theFilters = new Filter[filters.size()];
		filters.toArray(theFilters);
		boolean actives[] = new boolean[filters.size()];
		CheckListModel clm = (CheckListModel)filterList.getModel();
		for (int t=0; t<clm.getSize(); t++) {
			actives[t]=clm.isChecked(t);
		}
		FiltersVector newFilters = new FiltersVector();
		for (int t=0; t<theFilters.length; t++) {
			newFilters.addFilter(theFilters[t], actives[t]);
		}
		filterable.setFilters(newFilters,false);
	}
	
	/**
	 * Restore the initial filters in the list
	 * 
	 *
	 */
	private void restoreFilters() {
		if (modified) {
			int ret = JOptionPane.showConfirmDialog(null,
				"Do you really want to restore the original filters?",
				"Restore filters", JOptionPane.YES_NO_OPTION);
			if (ret != JOptionPane.YES_OPTION) {
				return;
			}
		}
		filters.setFilters(initialFilters);
		setupFields(filters);
	}
	
	/**
	 * Initialize the toolbar
	 *
	 */
	private void initToolbar() {
		ImageIcon addIcon=new ImageIcon(FilterChooserDialog.class.getResource("/page_add.png"));
		buttonAdd = new JButton("Add",addIcon);
		buttonAdd.addActionListener(bl);
		toolBar.add(buttonAdd);

		ImageIcon removeIcon=new ImageIcon(FilterChooserDialog.class.getResource("/page_delete.png"));
		buttonRemove = new JButton("Remove",removeIcon);
		buttonRemove.addActionListener(bl);
		toolBar.add(buttonRemove);

		ImageIcon editIcon=new ImageIcon(FilterChooserDialog.class.getResource("/page_edit.png"));
		buttonModify = new JButton("Modify",editIcon);
		buttonModify.addActionListener(bl);
		toolBar.add(buttonModify);
		
		getContentPane().add(toolBar,BorderLayout.NORTH);
	}
	
	/**
	 * Initialize the menubar
	 *
	 */
	private void initMenubar() {
		JMenu fileMenu = new JMenu("File");
		fileMenu.add(loadMI);
		fileMenu.add(saveMI);
		fileMenu.add(saveAsMI);
		fileMenu.add(new JSeparator());
		fileMenu.add(closeMI);
		
		JMenu editMenu = new JMenu("Edit");
		editMenu.add(activateAllMI);
		editMenu.add(deactivateAllMI);
		editMenu.add(new JSeparator());
		editMenu.add(clearAllMI);
		
		menuBar.add(fileMenu);
		menuBar.add(editMenu);
		
		setJMenuBar(menuBar);
		
		// Add the event listeners
		loadMI.addActionListener(bl);
		saveMI.addActionListener(bl);
		saveAsMI.addActionListener(bl);
		closeMI.addActionListener(bl);
		activateAllMI.addActionListener(bl);
		deactivateAllMI.addActionListener(bl);
		clearAllMI.addActionListener(bl);
	}
	
	/**
	 * Override <code>JDialog.setVisible</code> to show this dialog over
	 * the <code>LogsingClient</code> component.
	 */
	@Override
	public void setVisible(boolean visible) {
		setLocationRelativeTo(loggingClient);
		pack();
		super.setVisible(visible);
		toFront();
	}

}
