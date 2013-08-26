package com.cosylab.gui.components.r2;

import java.awt.event.*;
import java.awt.Component;
import javax.swing.*;
import javax.swing.border.EmptyBorder;
/**
 * An extension of javax.swing.JList that displays a list with checkboxes
 * before each item, allowing the user to select only specific items.
 * The list is represented by specialized data model, that wraps the
 * objects put into it by an object that provides boolean property. This
 * indicates the checked state of the items. This is provided transparently.
 * Any object can be put into the list, text displayed in the list is
 * obtained by calling that objects toString() method.
 * Creation date: (24-Oct-01 1:13:20 PM)
 * @author: 
 */

public class JCheckList extends javax.swing.JList {



	public class CLKeyAdapter extends java.awt.event.KeyAdapter {
		public void keyPressed(KeyEvent e) {
			if (e.getKeyChar() == ' ')
				JCheckList.this.checkboxAction();
		}
	}

	public class CLMouseAdapter extends java.awt.event.MouseAdapter {
		public void mouseClicked(MouseEvent e) {
			if (e.getX() < 20)
				JCheckList.this.checkboxAction();
		}
	}
	public class CheckListRenderer extends JCheckBox implements ListCellRenderer {
		protected final EmptyBorder defaultBorder = new EmptyBorder(1, 1, 1, 1);

		public CheckListRenderer() {
			super();
			setOpaque(true);
			setBorder(defaultBorder);

		}
		public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
			setText(value.toString());

			setBackground(isSelected ? list.getSelectionBackground() : list.getBackground());
			setForeground(isSelected ? list.getSelectionForeground() : list.getForeground());

			CheckListModel lm = (CheckListModel)list.getModel();
			if (lm == null)
				return this;
				
			setSelected(lm.isChecked(index));

			setFont(list.getFont());
			setBorder((cellHasFocus) ? UIManager.getBorder("List.focusCellHighlightBorder") : defaultBorder);

			return this;
		}
	}

/**
 * Constructs new JCheckedList.
 */
public JCheckList() {
	super();
	
	setModel(new CheckListModel());
	
	addMouseListener(new CLMouseAdapter());
	addKeyListener(new CLKeyAdapter());
	addKeyListener(new ListSearchListener(this));

	setCellRenderer(new CheckListRenderer());
	
}
/**
 * Constructs new JCheckedList.
 * @param listData java.lang.Object[]
 */
public JCheckList(java.lang.Object[] listData) {
	super(listData);
}
/**
 * Constructs new JCheckedList.
 * @param listData java.util.Vector
 */
public JCheckList(java.util.Vector listData) {
	super(listData);
}
/**
 * Internal helper method toggles the checked state of the item.
 * <p>
 * Creation date: (12/25/2001 18:34:55)
 */
protected void checkboxAction() {
	int index = getSelectedIndex();

	if (index < 0)
		return;

	((CheckListModel) getModel()).toggleChecked(index);

}
/**
 * Returns array containing boolean values of the checked states of the items
 * in this list.
 * <p>
 * Creation date: (1/2/2002 22:50:19)
 * @return boolean[]
 */
public boolean[] getChecked() {
	CheckListModel clm = (CheckListModel) getModel();
	int n = clm.size();

	boolean[] result = new boolean[n];

	for (int i = 0; i < n; i++) {
		result[i] = clm.isChecked(i);
	}
	return result;
}
/**
 * Returns an array of all the items that have the same state as the
 * isChecked parameter. This is a utility method that allows selective
 * retrieval of data in this list.
 * @param isChecked boolean
 * @return Object[]
 */

public Object[] getCheckedItems(boolean isChecked) {
		CheckListModel clm = (CheckListModel)getModel();
		java.util.ArrayList validItems = new java.util.ArrayList();

		int n = clm.size();
				
		for (int i = 0; i<n; i++) {
			if (clm.isChecked(i) == isChecked) {
				validItems.add(clm.get(i));
			}
		}	
		return validItems.toArray();
	}
/**
 * Sets the checked state of the items in this list. The length of array
 * must match the number of elements in this list.
 * <p>
 * Creation date: (1/2/2002 22:51:36)
 * @param value boolean[]
 */
public void setChecked(boolean[] value) {
	CheckListModel clm = (CheckListModel)getModel();
	int n = clm.size();
	if (value.length != n)
		return;

	for (int i = 0; i < n; i++) 
		clm.setChecked(i, value[i]);
}
}
