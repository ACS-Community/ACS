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

import java.awt.Component;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.ListSelectionModel;
import javax.swing.WindowConstants;
import javax.swing.filechooser.FileFilter;

import com.cosylab.gui.components.r2.CheckListModel;
import com.cosylab.gui.components.r2.JCheckList;
import com.cosylab.logging.LogTableDataModel;
import com.cosylab.logging.LoggingClient;
import com.cosylab.logging.engine.Filter;
import com.cosylab.logging.engine.FiltersVector;

/**
 * Serves the purpose of selecting the right filters (Timestamp, Entry Type,
 * File, etc.) to be displayed in the table according to the user's preferences.
 * Used by LogEntryTable.
 * <p>
 * Creation date: (1/2/2002 22:53:33)
 * 
 * @author: Ales Pucelj (ales.pucelj@kgb.ijs.si)
 */
public class FilterChooserDialog extends JDialog {

	private JCheckList filterList = null;

	private JLabel description = null;

	private JButton buttonClose = null;

	private JButton buttonModify = null;

	private JButton buttonAdd = null;

	private JButton buttonRemove = null;

	private JButton buttonActivateAll = null;

	private JButton buttonDeactivateAll = null;

	private JButton buttonDeleteAll = null;

	private JButton buttonSave = null;

	private JButton buttonSaveAs = null;

	private JButton buttonLoad = null;

	private JButton buttonApply = null;
	
	private JButton buttonRestore = null;

	// The logging client
	private LoggingClient loggingClient;
	
	// The table model
	private LogTableDataModel tableModel=null;
	
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
	private class ButtonListener implements java.awt.event.ActionListener {
		private FilterChooserDialog fcd = FilterChooserDialog.this;

		private Filter editFilter(Filter f) {
			FilterParameterDialog fpd = new FilterParameterDialog();
			fpd.setFilter(f);
			if (fpd.showModal() == FilterParameterDialog.MODAL_OK) {
				fpd.dispose();
				return fpd.getFilter();
			}
			return null;
		}

		public void actionPerformed(java.awt.event.ActionEvent e) {
			if (e.getSource() == buttonClose) { // Close
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
			} else if (e.getSource() == buttonActivateAll) {
				CheckListModel clm = (CheckListModel) filterList.getModel();
				for (int i = 0; i < clm.getSize(); i++) {
					clm.setChecked(i, true); // Deactivate all the filters
					modified=true;
				}
			} else if (e.getSource() == buttonDeactivateAll) { // Deactivate All
				CheckListModel clm = (CheckListModel) filterList.getModel();
				for (int i = 0; i < clm.getSize(); i++) {
					clm.setChecked(i, false); // Deactivate all the filters
					modified=true;
				}
			} else if (e.getSource() == buttonDeleteAll) { // Delete all
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
			} else if (e.getSource() == buttonLoad) { // Load
				loadFilters();
			} else if (e.getSource() == buttonSave) { // Save
				saveFilters(filterFileName);
			} else if (e.getSource() == buttonSaveAs) { // Save as
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
	 * Edit a filter
	 *
	 */
	private void editFilter() {
		
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
	 */
	public FilterChooserDialog(LoggingClient logCli, LogTableDataModel model) {
		super();
		if (model == null) {
			throw new IllegalArgumentException(
					"Invalid null LogTableDataModel in constructor");
		}
		if (logCli == null) {
			throw new IllegalArgumentException(
					"Invalid null LoggingClient in constructor");
		}
		loggingClient = logCli;
		tableModel=model;
		System.out.println("FilterChooserDialog::FilterChooserDialog(...)");
		setTitle("Filter chooser");
		setModal(false);

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
		getContentPane().setLayout(new GridBagLayout());

		ButtonListener bl = new ButtonListener();

		GridBagConstraints c = new GridBagConstraints();

		description = new JLabel("Select filter to apply");
		c = newConstraints(0, 0, 0, 4);
		c.gridwidth = 3;
		getContentPane().add(description, c);

		c = newConstraints(0, 1, 0, 4);
		c.gridwidth = 3;
		c.weighty = 1.0;
		c.fill = GridBagConstraints.BOTH;
		filterList = new JCheckList();
		getContentPane().add(new JScrollPane(filterList), c);
		filterList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

		buttonAdd = new JButton("Add");
		buttonAdd.addActionListener(bl);
		getContentPane().add(buttonAdd, newConstraints(0, 2, 4, 4));

		buttonRemove = new JButton("Remove");
		buttonRemove.addActionListener(bl);
		getContentPane().add(buttonRemove, newConstraints(1, 2, 4, 4));

		buttonModify = new JButton("Modify");
		buttonModify.addActionListener(bl);
		getContentPane().add(buttonModify, newConstraints(2, 2, 4, 4));

		buttonActivateAll = new JButton("Activate all");
		buttonActivateAll.addActionListener(bl);
		getContentPane().add(buttonActivateAll, newConstraints(0, 3, 4, 4));

		buttonDeactivateAll = new JButton("Deactivate all");
		buttonDeactivateAll.addActionListener(bl);
		getContentPane().add(buttonDeactivateAll, newConstraints(1, 3, 4, 4));

		buttonDeleteAll = new JButton("Delete all");
		buttonDeleteAll.addActionListener(bl);
		getContentPane().add(buttonDeleteAll, newConstraints(2, 3, 4, 4));

		buttonSave = new JButton("Save");
		buttonSave.addActionListener(bl);
		getContentPane().add(buttonSave, newConstraints(0, 4, 4, 4));

		buttonSaveAs = new JButton("Save as");
		buttonSaveAs.addActionListener(bl);
		getContentPane().add(buttonSaveAs, newConstraints(1, 4, 4, 4));

		buttonLoad = new JButton("Load");
		buttonLoad.addActionListener(bl);
		getContentPane().add(buttonLoad, newConstraints(2, 4, 4, 4));

		JSeparator sep = new JSeparator();
		GridBagConstraints constr = newConstraints(0, 5, 4, 4);
		constr.gridwidth = 3;
		getContentPane().add(sep, constr);

		buttonApply = new JButton("Apply");
		buttonApply.addActionListener(bl);
		getContentPane().add(buttonApply, newConstraints(0, 6, 4, 4));
		
		buttonRestore= new JButton("Restore");
		buttonRestore.addActionListener(bl);
		getContentPane().add(buttonRestore, newConstraints(1, 6, 4, 4));

		buttonClose = new JButton("Close");
		buttonClose.addActionListener(bl);
		getContentPane().add(buttonClose, newConstraints(2, 6, 4, 4));

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
	 * Set the enties in the list of filters
	 * (one row per each filter) 
	 * 
	 * @param filters The list of filters
	 */
	public void setupFields(FiltersVector filters) {
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
		buttonActivateAll.setEnabled(hasEntries);
		buttonDeactivateAll.setEnabled(hasEntries);
		buttonDeleteAll.setEnabled(hasEntries);
		buttonSave.setEnabled(filterFileName!=null && hasEntries);
		buttonSaveAs.setEnabled(hasEntries);
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
				filters.loadFilters(fileToLoad,
						eraseOldFilters, null);
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
		tableModel.setFilters(theFilters,actives);
		loggingClient.getLogEntryTable().updateFilteredString();
		tableModel.invalidateVisibleLogs();
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

}
