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

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.ListSelectionModel;

import com.cosylab.gui.components.r2.CheckListModel;
import com.cosylab.gui.components.r2.JCheckList;
import com.cosylab.logging.engine.Filter;
import com.cosylab.logging.engine.FiltersVector;

/**
 * Serves the purpose of selecting the right filters (Timestamp, Entry Type, File, etc.) 
 * to be displayed in the table according to the user's preferences. Used by LogEntryTable. 
 * <p>
 * Creation date: (1/2/2002 22:53:33)
 * @author: Ales Pucelj (ales.pucelj@kgb.ijs.si)
 */
public class FilterChooserDialog extends JModalDialog {
	private JCheckList filterList = null;
	private JLabel description = null;
	private JButton buttonOK = null;
	private JButton buttonCancel = null;
	private JButton buttonModify = null;
	private JButton buttonAdd = null;
	private JButton buttonRemove = null;
	private JButton buttonActivateAll = null;
	private JButton buttonDeactivateAll = null;
	private JButton buttonDeleteAll = null;
	

	private JLabel historyLabel = null;

	private ButtonListener buttonListener = new ButtonListener();
	
	private Insets defaultInsets = new Insets(4, 4, 4, 4);
	private int modalResult = 0;

	private JScrollPane scrollPane = null;

	private class ButtonListener implements java.awt.event.ActionListener {
		FilterChooserDialog fcd = FilterChooserDialog.this;

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
			if (e.getSource() == buttonOK) {
				FilterChooserDialog.this.returnModalOK();
			}
			if (e.getSource() == buttonCancel) {
				FilterChooserDialog.this.returnModalCancel();
			}
			if (e.getSource() == buttonModify) {
				CheckListModel clm = (CheckListModel)fcd.filterList.getModel();
				int i = fcd.filterList.getSelectedIndex();
				if (i>=0) {
					boolean isItemChecked = clm.isChecked(i);
					
					if (i > -1) {
						Filter f = (Filter)clm.get(i);
						f = editFilter(f);
						if (f != null)
							clm.set(i, f);
					}
					clm.setChecked(i,isItemChecked);
				} else {
					JOptionPane.showMessageDialog(FilterChooserDialog.this,"Invalid item to modify","Error",JOptionPane.ERROR_MESSAGE);
				}
			} 
			if (e.getSource() == buttonAdd) {
				Filter f=editFilter(null);
				if (f != null) {
					CheckListModel clm = (CheckListModel)fcd.filterList.getModel();
					clm.addElement(f);
					clm.setChecked(clm.getSize()-1, true);
				}
			}
			if (e.getSource() == buttonRemove) {
				int i = fcd.filterList.getSelectedIndex();
				if (i > -1)
					((CheckListModel)(fcd.filterList.getModel())).remove(i);
			}
			if (e.getSource()==buttonActivateAll) {
				CheckListModel clm = (CheckListModel)filterList.getModel();
				for (int i = 0; i < clm.getSize(); i++) {
					clm.setChecked(i, true); // Deactivate all the filters
				}
			}
			if (e.getSource()==buttonDeactivateAll) {
				CheckListModel clm = (CheckListModel)filterList.getModel();
				for (int i = 0; i < clm.getSize(); i++) {
					clm.setChecked(i, false); // Deactivate all the filters
				}
			}
			if (e.getSource()==buttonDeleteAll) {
				CheckListModel clm = (CheckListModel)filterList.getModel();
				if (clm.getSize()>0) {
					// Ask the user for a confirmation
					int ret=JOptionPane.showConfirmDialog(null,"Do you really want to delete all the filters?","Delete all filters",JOptionPane.YES_NO_OPTION);
					if (ret==JOptionPane.YES_OPTION) {
						clm.clear();
					}
				}
			}
			FilterChooserDialog.this.updateButtons();
		}
	}

/**
 * FilterChooserDialog constructor.
 */
public FilterChooserDialog() {
	super();

	setTitle("Filter chooser");
	setModal(true);

	setBounds(50,50,300,300);
	
	//getContentPane().setLayout(new GridBagLayout());
	getContentPane().setLayout(new GridBagLayout());

	createComponents();
}

/**
 * Insert the method's description here.
 * Creation date: (2/7/02 4:29:36 PM)
 */
protected void createComponents() {

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

	buttonOK = new JButton("OK");
	buttonOK.addActionListener(bl);
	getContentPane().add(buttonOK, newConstraints(0, 6, 4, 4));
	
	buttonCancel = new JButton("Cancel");
	buttonCancel.addActionListener(bl);
	getContentPane().add(buttonCancel, newConstraints(2, 6, 4, 4));
	
	pack();
}

/**
 * Insert the method's description here.
 * <p>
 * Creation date: (1/2/2002 23:35:37)
 * @return boolean[]
 */
public boolean[] getChecked() {
	return filterList.getChecked();
}

/**
 * Insert the method's description here.
 * Creation date: (2/6/02 3:34:17 PM)
 * @return com.cosylab.logging.engine.Filter[]
 */
public Filter[] getFilters() {
	CheckListModel clm = (CheckListModel)(filterList.getModel());

	int l = clm.size();
	Filter[] returnValue = new Filter[l];

	for (int i = 0; i < l; i++) {
		returnValue[i] = (Filter)clm.get(i);
	}
	return returnValue;
}

/**
 * Insert the method's description here.
 * <p>
 * Creation date: (1/2/2002 23:26:28)
 * @return int
 */
public int getModalResult() {
	return modalResult;
}

/**
 * Insert the method's description here.
 * <p>
 * Creation date: (1/2/2002 22:59:20)
 * @param args java.lang.String[]
 */
public static void main(String[] args) {
	FilterChooserDialog fcd = new FilterChooserDialog();
	
	fcd.setVisible(true);	
}

/**
 * Insert the method's description here.
 * Creation date: (2/7/02 4:30:09 PM)
 * @return java.awt.GridBagConstraints
 * @param x int
 * @param y int
 * @param top int
 * @param bottom int
 */
protected GridBagConstraints newConstraints(int x, int y, int top, int bottom) {
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
 * Insert the method's description here.
 * <p>
 * Creation date: (1/2/2002 23:34:08)
 * @param fieldNames java.lang.String[]
 * @param checked boolean[]
 */
public void setupFields(FiltersVector filters) {
	CheckListModel clm = (CheckListModel)filterList.getModel();
	clm.clear();

	Filter f = null;
	
	for (int i = 0; i < filters.size(); i++) {
		clm.addElement(filters.get(i));
		clm.setChecked(i, false); // Deactivate all the filters
	}
	// Activate the selected filters
	if (filters.hasActiveFilters()) {
		int active[] = filters.getAppliedFiltersIndexes();
		if (active!=null) {
			for (int i=0; i<active.length; i++) {
				clm.setChecked(active[i],true);
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
}

}
