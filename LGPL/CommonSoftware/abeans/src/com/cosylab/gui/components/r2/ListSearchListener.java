package com.cosylab.gui.components.r2;

import java.awt.event.*;
import javax.swing.*;

/**
 * Insert the type's description here.
 * Creation date: (24-Oct-01 12:50:38 PM)
 * @author: 
 */
public class ListSearchListener extends KeyAdapter {
	protected JList ownerList;
	private long timeDelta = 0;
	private CheckListModel clm = null;
	private String searchString = "";
/**
 * CheckListListener constructor comment.
 */
public ListSearchListener() {
	super();
}
	public ListSearchListener(JList parent)
	{
		ownerList = parent;
		clm = (CheckListModel)ownerList.getModel();

	}
public void keyTyped(KeyEvent e) {

	char ch = e.getKeyChar();
	if (!Character.isLetterOrDigit(ch))
		return;

	if (timeDelta + 1000 < System.currentTimeMillis())
		searchString = "";
	timeDelta = System.currentTimeMillis();

	searchString += Character.toLowerCase(ch);

	int listSize = clm.getSize();
	
	for (int k = 0; k < listSize; k++) {
		String str = ((String) clm.get(k)).toLowerCase();
		if (str.startsWith(searchString)) {
			ownerList.setSelectedIndex(k);
			ownerList.ensureIndexIsVisible(k);
			break;
		}
	}
}

}
