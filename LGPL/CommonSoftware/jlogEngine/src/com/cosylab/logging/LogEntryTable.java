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
package com.cosylab.logging;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Date;
import java.text.SimpleDateFormat;

import javax.swing.ButtonGroup;
import javax.swing.JComponent;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JTable;
import javax.swing.JViewport;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;
import javax.swing.ListSelectionModel;
import javax.swing. DefaultListSelectionModel;

import com.cosylab.logging.client.EntryTypeRenderer;
import com.cosylab.logging.client.ExpandButtonRenderer;
import com.cosylab.logging.client.InfoRenderer;
import com.cosylab.logging.engine.LogEntry;
import com.cosylab.logging.settings.FieldChooserDialog;
import com.cosylab.logging.settings.FilterChooserDialog;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.ClipboardOwner;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.Font;
import java.awt.FontMetrics;
import java.io.IOException;

import com.cosylab.logging.LogTypeHelper;
import com.cosylab.logging.settings.UserInfoDlg;

/**
 * Subclasses JTable allowing grouping and sorting depending on user's input. 
 * Creation date: (11/11/2001 13:45:22)
 * @author: 
 */

public class LogEntryTable extends javax.swing.JTable
{
	private TableColumn[] columnsList;
	private boolean[] visibleColumns;
	private FieldChooserDialog fieldChooser = new FieldChooserDialog();
	private ColumnMenu popupMenu = new ColumnMenu();

	private SortMenu sortMenu;
	private GroupMenu groupMenu;
	
	private LoggingClient loggingClient;
	
	public TextTransfer textTransfer;
	
	private DefaultListSelectionModel selectionModel;

	protected class JMyMenuItem extends JRadioButtonMenuItem
	{
		public int columnIndex = 0;
		public JMyMenuItem()
		{
			super();
		}
	}
	
	/**
	 * The class to set the clipboard content 
	 *  
	 * @author acaproni
	 *
	 */
	public final class TextTransfer implements ClipboardOwner {
		/**
	   * Empty implementation of the ClipboardOwner interface.
	   */
	   public void lostOwnership( Clipboard aClipboard, Transferable aContents) {
	     //do nothing
	   }
	   
	   /**
	    * Place a String on the clipboard, and make this class the
	    * owner of the Clipboard's contents.
	    */
	    public void setClipboardContents( String str){
	      StringSelection stringSelection = new StringSelection(str);
	      Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
	      try {
	      	clipboard.setContents( stringSelection, stringSelection );
	      } catch (IllegalStateException e) {
	      	// This exception may be returned in some cases
	      	// It is a temporary situation: we do nothing here
	      	// and the user will retry again or most likely 
	      	// submit an SPR ;-)
	      	e.printStackTrace();
	      }
	      /* Check if the clipboards contains the right string
	      String readStr=null;
	      try {
	      	readStr = (String)(clipboard.getContents(this).getTransferData(DataFlavor.stringFlavor));
	      } catch (Exception e) {
	      	e.printStackTrace();
	      }
	      System.out.println("Write ["+str+"]");
	      System.out.println("Write ["+readStr+"]");
	      System.out.println("Comparison="+str.compareTo(readStr));*/
	    }
	    
	    /**
	     * Get the String residing on the clipboard.
	     *
	     * @return any text found on the Clipboard; if none found, return an
	     * empty String.
	     */
	     private String getClipboardContents() {
	       String result = "";
	       Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
	       //odd: the Object param of getContents is not currently used
	       Transferable contents = clipboard.getContents(null);
	       boolean hasTransferableText = (contents != null) &&
		   		contents.isDataFlavorSupported(DataFlavor.stringFlavor);
	       if (hasTransferableText) {
	         try {
	           result = (String)contents.getTransferData(DataFlavor.stringFlavor);
	         }
	         catch (UnsupportedFlavorException ex){
	           //highly unlikely since we are using a standard DataFlavor
	           System.out.println(ex);
	         }
	         catch (IOException ex) {
	           System.out.println(ex);
	         }
	       }
	       return result;
	     }
	}
	
	/**
	 * The JPopupMenu displayed when the user presses the right mouse button
	 * over a row of the table
	 * 
	 * @author acaproni
	 *
	 */
	private class ColumnPopupMenu extends javax.swing.JPopupMenu implements ActionListener
	{
		// The menu item to copy the text to the clipboard
		private JMenuItem copyClipboard = new JMenuItem("to clipboard");
		// The menu item to copy the text to the additional info field
		private JMenuItem copyAddInfo = new JMenuItem("to additionalInfo");
		// The menu item to allow the user to add his information
		private JMenuItem addUserInfo = new JMenuItem("add Info");
		// The text to copy
		private String textToCopy;
		// The row and column under the mouse pointer
		private int row;
		private int col;
		
		/**
		 * The constructor builds the menu
		 *
		 */
		public ColumnPopupMenu(int row, int col, String text)
		{
			// Copy the parameter to local variables
			this.row=row;
			this.col=col;
			// A make a copy of the original string 
			this.textToCopy=new String(text);
			// Add the Label
			setLabel("Paste");
			
			// Add the menu items
			add(copyClipboard);
			add(copyAddInfo);
			addSeparator();
			add(addUserInfo);
			// Hide the uneeded buttons
			boolean singleLineSelected =
				selectionModel.getMinSelectionIndex() == selectionModel.getMaxSelectionIndex(); 
			copyAddInfo.setEnabled(singleLineSelected);
			addUserInfo.setEnabled(singleLineSelected);
			copyClipboard.setEnabled(true);
			// Add the listeners
			copyClipboard.addActionListener(this);
			copyAddInfo.addActionListener(this);
			addUserInfo.addActionListener(this);
			// Disable the menu items if the test is null or empty
			if (textToCopy==null || textToCopy.length()==0) {
				copyClipboard.setEnabled(false);
				copyAddInfo.setEnabled(false);
				return;
			}
			// Check if the system clipboard is available
			// and eventually disable the menu item
			SecurityManager sm = System.getSecurityManager();
		    if (sm != null) {
		    	try {
		    		sm.checkSystemClipboardAccess();
		    	} catch (Exception e) {
		    		copyClipboard.setEnabled(false);
		    	}
		    }
		    // Check if the selected log entry has already a data defined 
		    // In this case we cannot copy our data there overriding the original one
		    // TODO: check if it is possible to define multilevel data
		    //       because we're using a tree in the aditional info pane
			if (row >= 0) {
				//copyAddInfo.setEnabled(!getLCModel().getVisibleLogEntry(row).hasDatas());
			} else {
				// Strange error because the popup menu activates only if the user
				// pressed the mouse button over an item
				copyAddInfo.setEnabled(false);
			}
		}
		
		/**
		 * Handle the events from the menu
		 * 
		 * @param e The event
		 */
		public void actionPerformed(ActionEvent e) {
			if (textToCopy==null) {
				return;
			}
			if (e.getSource()==copyClipboard) {
				// Build the text to copy in the clipboard
				if (!selectionModel.isSelectionEmpty()) {
					StringBuffer strBuffer = new StringBuffer();
					for (int i=selectionModel.getMinSelectionIndex(); 
						i<=selectionModel.getMaxSelectionIndex(); i++) {
						if (!selectionModel.isSelectedIndex(i)) {
							continue;
						} else {
							LogEntry log = getLCModel().getVisibleLogEntry(row);
							strBuffer.append(log.toXMLString());
							strBuffer.append("\n");
						}
					}
					// Copy the text to the clipboard
					textTransfer.setClipboardContents(strBuffer.toString());
				} 
			} else if (e.getSource()==copyAddInfo) {
				// Copy the text to the additionalInfo field
				LogEntry logEntry = getLCModel().getVisibleLogEntry(row);
				logEntry.addData("Generated by jlog",textToCopy);
				
				LogTableDataModel tableModel = (LogTableDataModel) getModel();
				tableModel.fireTableRowsUpdated(row,row);
				setRowSelectionInterval(row,row);
				
				getLCModel().replaceLog(row,logEntry);
				
				loggingClient.getJScrollPane2().setViewportView(loggingClient.getDataTable(logEntry));
			} else if (e.getSource()==addUserInfo) {
				// Show the dialog
				UserInfoDlg dlg = new UserInfoDlg();
				if (dlg.okPressed()) {
					String name =  dlg.getInfoName();
					String value = dlg.getInfo();
					// Replace some chars potentially dangerous for xml
					value = value.replaceAll("&","&amp;");
					value = value.replaceAll("'","&apos;");
					value = value.replaceAll("`","&apos;");
					value = value.replaceAll("\"","&quot;");
					value = value.replaceAll("<","&lt;");
					value = value.replaceAll(">","&gt;");
					name = name.replaceAll("&","&amp;");
					name = name.replaceAll("'","&apos;");
					name = name.replaceAll("`","&apos;");
					name = name.replaceAll("\"","&quot;");
					name = name.replaceAll("<","&lt;");
					name = name.replaceAll(">","&gt;");
					
					LogEntry logEntry = getLCModel().getVisibleLogEntry(row);
					logEntry.addData(name,value);
					
					LogTableDataModel tableModel = (LogTableDataModel) getModel();
					tableModel.fireTableRowsUpdated(row,row);
					setRowSelectionInterval(row,row);
					
					getLCModel().replaceLog(row,logEntry);
					
					loggingClient.getJScrollPane2().setViewportView(loggingClient.getDataTable(logEntry));
				}
			} else {
				// Unknown source ==> does nothing
			}
		}
	}
	
	

	private class GroupMenu extends javax.swing.JMenu implements ActionListener
	{
		private ButtonGroup groupGroup = new ButtonGroup();
		private int columnCount = 0;
		private JMyMenuItem newItem(String s, ButtonGroup bg, int index)
		{
			JMyMenuItem rb = new JMyMenuItem();
			rb.setText(s);
			rb.columnIndex = index;
			bg.add(rb);
			add(rb);
			rb.addActionListener(this);
			return rb;
		}
		public void rebuild()
		{
			setText("Group By");
			removeAll();
			columnCount = 0;
			LogEntryTable let = LogEntryTable.this;
			JMyMenuItem rb = null;
			int n = let.columnsList.length;
			int s = let.getGroupIndex() + 1;
			for (int i = 1; i < n; i++)
			{
				if (let.visibleColumns[i])
				{
					rb = newItem(LogEntry.getFieldDescription(i - 1), groupGroup, i - 1);
					columnCount++;
					if (i == s)
						rb.setSelected(true);
				}
			}
			add(new JSeparator());
			rb = newItem("Ungroup", groupGroup, -2);
			if (!let.isGrouped())
				rb.setSelected(true);
		}
		
		public void actionPerformed(ActionEvent e)
		{
			if (e.getSource() instanceof JMyMenuItem)
			{
				LogEntryTable let = LogEntryTable.this;
				JMyMenuItem mmi = (JMyMenuItem) (e.getSource());
				int columnIndex = mmi.columnIndex;

				if (columnIndex > -1)
					let.setGroupIndex(columnIndex);

				if (columnIndex == -2)
					let.setGroupIndex(-1);
			}
		}
	}

	private class SortMenu extends javax.swing.JMenu implements ActionListener
	{
		private ButtonGroup sortGroup = new ButtonGroup();
		private ButtonGroup orderGroup = new ButtonGroup();
		private int columnCount = 0;
		private JMyMenuItem newItem(String s, ButtonGroup bg, int index)
		{
			JMyMenuItem rb = new JMyMenuItem();
			rb.setText(s);
			rb.columnIndex = index;
			bg.add(rb);
			add(rb);
			rb.addActionListener(this);
			return rb;
		}
		public void rebuild()
		{
			setText("Sort By");
			removeAll();
			columnCount = 0;
			LogEntryTable let = LogEntryTable.this;
			JMyMenuItem rb = null;
			int n = let.columnsList.length;
			int s = let.getSortIndex() + 1;
			for (int i = 1; i < n; i++)
			{
				if (let.visibleColumns[i])
				{
					rb = newItem(LogEntry.getFieldDescription(i - 1), sortGroup, i - 1);
					columnCount++;
					if (i == s)
						rb.setSelected(true);
				}
			}
			add(new JSeparator());
			rb = newItem("Ascending", orderGroup, -2);
			if (let.isSortAscending())
				rb.setSelected(true);
			rb = newItem("Descending", orderGroup, -3);
			if (!let.isSortAscending())
				rb.setSelected(true);
		}
		public void actionPerformed(ActionEvent e)
		{
			if (e.getSource() instanceof JMyMenuItem)
			{
				LogEntryTable let = LogEntryTable.this;
				JMyMenuItem mmi = (JMyMenuItem) (e.getSource());
				int columnIndex = mmi.columnIndex;

				if (columnIndex > 0)
					let.setSortIndex(columnIndex);

				if (columnIndex == -2)
					let.setSortOrder(true);

				if (columnIndex == -3)
					let.setSortOrder(true);
			}
		}
	}

	/**
	 * Popup menu used to display the column options for the table.
	 * It is displayed when the user presses the right mouse button over the
	 * header of the table
	 * 
	 * Creation date: (1/25/02 11:16:28 AM)
	 * @author: acaproni
	 */
	public class ColumnMenu extends javax.swing.JPopupMenu
	{

		private String[] menuNames =
			{
				"Sort ascending",
				"Sort descending",
				"-",
				"Remove column",
				"Column chooser",
				"-",
				"Group by column",
				"Ungroup" };

		private final int itemCount = menuNames.length;

		private int columnIndex = 0;

		private JMenuItem[] items = new JMenuItem[itemCount];

		private class MenuClicked implements java.awt.event.ActionListener
		{

			public void actionPerformed(java.awt.event.ActionEvent e)
			{

				LogEntryTable let = LogEntryTable.this;
				ColumnMenu menu = ColumnMenu.this;

				switch (ColumnMenu.this.findItem(e.getSource()))
				{
					case -1 :
						break;
					case 0 :
						let.setSortIndex(menu.getColumnIndex());
						let.setSortOrder(true);
						break;
					case 1 :
						let.setSortIndex(menu.getColumnIndex());
						let.setSortOrder(false);
						break;
					case 3 :
						let.hideColumn(menu.getColumnIndex() + 1);
						break;
					case 4 :
						let.showFieldChooser();
						break;
					case 6 :
						let.setGroupIndex(menu.getColumnIndex());
						break;
					case 7 :
						let.setGroupIndex(-1);
				}
			}
		}
		/**
		 * Constructs new column popup menu.
		 */
		public ColumnMenu()
		{
			super();

			JMenuItem menuItem = null;

			for (int i = 0; i < itemCount; i++)
			{
				if (menuNames[i].equals("-"))
				{
					addSeparator();
				}
				else
				{
					menuItem = new JMenuItem();
					menuItem.setText(menuNames[i]);
					menuItem.addActionListener(new MenuClicked());
					items[i] = menuItem;
					add(menuItem);
				}
			}
		}

		public void setColumnIndex(int index)
		{
			columnIndex = index;
		}
		public int getColumnIndex()
		{
			return columnIndex;
		}
		/**
		 * Finds the index of the menu item that was clicked. This value
		 * is used internally to perform appropriate actions.
		 * Creation date: (1/25/02 11:35:04 AM)
		 * @return int
		 * @param source java.lang.Object
		 */
		protected int findItem(Object source)
		{
			int i = 0;
			while (i < itemCount)
			{
				if (items[i] == source)
					return i;
				i++;
			}
			return -1;
		}

	}

	private class headerMouseAdapter extends MouseAdapter
	{
		private boolean performPopup(MouseEvent e)
		{
			LogEntryTable owner = LogEntryTable.this;

			int col = getColumnModel().getColumnIndexAtX(e.getX());

			if (e.isPopupTrigger())
			{
				popupMenu.setColumnIndex(owner.columnToModel(col));
				popupMenu.show(e.getComponent(), e.getX(), e.getY());
				e.consume();
				return true;
			}
			return false;
		}

		public void mouseClicked(MouseEvent e)
		{
			performPopup(e);

			if ((e.getModifiers() & MouseEvent.BUTTON1_MASK) == 0)
				return;

			LogEntryTable owner = LogEntryTable.this;

			int col = getColumnModel().getColumnIndexAtX(e.getX());

			col = owner.convertColumnIndexToModel(col) - 2;
			if (col == 0)
			{
				e.consume();
			}

			if (col == owner.getSortIndex()) {
				setSortOrder(!isSortAscending());
			} else {
				setSortIndex(col);
			}
		}

		public void mouseReleased(MouseEvent e)
		{
			performPopup(e);
		}
		public void mousePressed(MouseEvent e)
		{
			performPopup(e);
		}
	}

	private class ColumnMouseAdapter implements MouseListener
	{
		public void mouseReleased(MouseEvent e) {}
		public void mouseEntered(MouseEvent e) {}
		public void mouseExited(MouseEvent e) {}
		
		public void mouseClicked(MouseEvent e) { }
		
		public void mousePressed(MouseEvent e)
		{
			// Get the index of the row and the column below
			// the mouse pointer
			LogEntryTable owner = LogEntryTable.this;
			int col = getColumnModel().getColumnIndexAtX(e.getX());
			int row = owner.rowAtPoint(e.getPoint());
			
			// Handle the specific event for each mouse button
			if (e.getButton()==MouseEvent.BUTTON1)
			{
				// Left mouse button
				if ((owner.isGrouped()) && (owner.convertColumnIndexToModel(col)== 0))
				{
					owner.clearSelection();
					owner.getLCModel().toggleExpand(row);
					e.consume();
					return;
				}
				owner.updateExtraInfo();
			} else if (e.isPopupTrigger()) {
				// The mouse button is the pop up trigger so we show the menu
				// but only if the user selectd at least one row
				if (selectionModel.getMaxSelectionIndex()>=0) {
					String textUnderMouse = getCellStringContent(row, col);
					ColumnPopupMenu popMenu = new ColumnPopupMenu(row, col, textUnderMouse);
					popMenu.show(LogEntryTable.this,e.getX(),e.getY());
				}
			}
		}

	}
	
	/**
	 * LCLogTable constructor comment.
	 */
	public LogEntryTable(LoggingClient logClient)
	{
		super();
		initialize();
		// Create the object for the clipboard
		textTransfer = new TextTransfer();
		loggingClient=logClient;
	}
	/**
	 * Utility method to provide mapping of column index to data fields.
	 * Creation date: (1/25/02 11:11:43 AM)
	 * @return int
	 * @param index int
	 */
	protected int columnToModel(int index)
	{
		return convertColumnIndexToModel(index) - 1;
	}
	protected TableModel createDefaultDataModel()
	{
		return new LogTableDataModel();
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (2/7/02 3:52:36 PM)
	 * @return java.lang.String
	 */
	public org.w3c.dom.Node getExtraInfo()
	{
		//	String noInfoString = ;

		//	org.w3c.dom.Node nn = new org.w3c.dom.Node.;
		//	DataNode dn = new DataNode(null);
		//	dn.setNodeValue("No additional information");

		int index = getSelectedRow();
		if (index < 0)
			return null;

		LogEntry additionalInfo = getLCModel().getVisibleLogEntry(index);

		if ((additionalInfo == null) || (additionalInfo.getDatas() == null))
			return null;

		return additionalInfo.getDatas().item(0);
	}
	/**
	 * Insert the method's description here.
	 * <p>
	 * Creation date: (2/13/2002 21:30:23)
	 * @return java.lang.String
	 */
	public String getFilterString()
	{
		return getLCModel().getFilters().getFilterString();
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (2/6/02 10:34:56 AM)
	 * @return int
	 */
	public int getGroupIndex()
	{
		if (isGrouped())
		{
			return getLCModel().getGroupComparator().getFieldIndex();
		}
		return -1;
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (2/6/02 10:36:43 AM)
	 * @return javax.swing.JMenu
	 */
	public JMenu getGroupMenu()
	{
		if (groupMenu == null)
		{
			groupMenu = new GroupMenu();
			groupMenu.rebuild();
		}
		return groupMenu;
	}
	/**
	 * Returns the LogTableDataModel. This is a convinience method that returns propertly
	 * case data model.
	 * Creation date: (11/24/2001 18:44:41)
	 * @return com.cosylab.logging.client.LCLogTableDataModel
	 */
	public LogTableDataModel getLCModel()
	{
		return (LogTableDataModel) getModel();
	}
	/**
	 * Sets the index of the column the table should be sorted by. If the table is
	 * currently unsorted, the result is -1.
	 * Creation date: (1/24/02 10:43:26 AM)
	 * @return int
	 */
	public int getSortIndex()
	{
		LogEntryComparator comparator = getLCModel().getSortComparator();

		int result = -1;
		if (comparator != null)
			result = comparator.getFieldIndex();

		repaint();

		return result;
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (2/6/02 9:29:27 AM)
	 * @return javax.swing.JMenu
	 */
	public javax.swing.JMenu getSortMenu()
	{
		if (sortMenu == null)
			sortMenu = new SortMenu();
		return sortMenu;
	}
	/**
	 * Hides a table column specified by index.
	 * Creation date: (12/4/2001 22:57:58)
	 * @param columnIndex int
	 */
	public void hideColumn(int columnIndex)
	{
		if ((columnIndex > 0) && (columnIndex < columnsList.length))
		{
			getColumnModel().removeColumn(columnsList[columnIndex]);
			visibleColumns[columnIndex] = false;
		}
		if (sortMenu != null)
			sortMenu.rebuild();
	}

	/**
	 * Sets a tool tip on all the cells. It pops up when the value is not fully displayed while the mose 
	 * scrolls over it. 
	 * @see javax.swing.JTable#prepareRenderer(TableCellRenderer, int, int)
	 */
	public Component prepareRenderer(TableCellRenderer renderer, int rowIndex, int vColIndex)
	{
		String tooltipTxt = getCellStringContent(rowIndex, vColIndex);
		Component c = super.prepareRenderer(renderer, rowIndex, vColIndex);
		int columnWidth = getColumnModel().getColumn(vColIndex).getWidth();
		setToolTip((JComponent)c,tooltipTxt,columnWidth);
		return c;
	}
	
	/**
	 * Get a string representing the content of a cell
	 * 
	 * @param row The table row of the cell
	 * @param col The table column of the cell
	 * 
	 * @return A string representing the content of the cell
	 */
	private String getCellStringContent(int row, int col) {
		Object value = getValueAt(row, col);
		if (value == null) {
			return "";
		}
		
		String tempStr = "";
		if (value instanceof Date) {
			SimpleDateFormat sdf = new SimpleDateFormat("yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'SSS");
			tempStr = sdf.format(value);
		} else if (value instanceof Integer) {
			if (getColumnName(col).compareTo(LogEntry.getFieldDescription(LogEntry.FIELD_ENTRYTYPE))==0) {
				tempStr=LogTypeHelper.getLogTypeDescription(((Integer)value).intValue());
			} else {
				tempStr = value.toString();
			}
		} else {
			tempStr = value.toString();
		}

		return tempStr;
	}
	
	/**
	 * Format the string before setting the tooltip for the given component
	 * The tooltip is shown only if the text is not visible (i.e. the num.
	 * of displayed chars for the column containing the text is less then
	 * the given text).
	 * 
	 * To show the string as multine it is transformed in HTML so
	 * \n are replaced by <BR>/ To show strings containing HTML and/or
	 * XML the <PRE> tag is used (for this reason existing < and > in
	 * the original string are replaced by &lt; and &lgt;)
	 * 
	 * @param c The component to set the tooltip 
	 * @param text The string to display in the tooltip
	 * @param colWidth The width of the column
	 * 
	 * @return
	 */
	private void setToolTip(JComponent  c, String text, int colWidth) {
		if (text==null || text.length()==0)	{
			((JComponent) c).setToolTipText(null);
			return;
		}
		Font font = this.getFont();
		FontMetrics fm = getFontMetrics(font);
		if (fm.stringWidth(text)<colWidth) {
			((JComponent) c).setToolTipText(null);
		} else {
			// I am going to print the string in HTML format: new lines become <BR>
			text=text.replaceAll("<","&lt;");
			text=text.replaceAll(">","&gt;");
			// Eventually, set the tooltip
			((JComponent) c).setToolTipText("<HTML><FONT size=\"-2\">"+text+"</FONT></HTML>");
		}
	}

	/**
	 * Computes the total width of the table taking into consideration the visible columns' width.
	 * @return number of columns
	 */
	public int getColumnWidth(int n)
	{
		int width = 0;
		for (int i = 0; i < n; i++)
		{
			width = width + getColumnModel().getColumn(i).getWidth();
		}
		return width;
	}

	/**
	 * Assigns the additional width that is left to the right to Log Message column.
	 * @param width
	 * @param number of columns
	 * @return TableColumn
	 */
	public void setAdditionalWidth(int n, int width)
	{
		for (int i = 0; i < n; i++)
		{
			TableColumn tc = getColumnModel().getColumn(i);

			if ((tc.getHeaderValue()).equals("Log Message"))
			{
				tc.setPreferredWidth(tc.getWidth() + width);
			}
		}
	}

	/**
	 * Setup the table
	 */
	private void initialize()
	{
		createDefaultColumnsFromModel();
		setShowHorizontalLines(false);
		TableColumnModel tcm = getColumnModel();
		
		// Setup the first col 
		TableColumn tc = tcm.getColumn(0);
		String[] columnIcons = { "", "/add.gif", "/delete.gif" };
		tc.setCellRenderer(new ExpandButtonRenderer(columnIcons));
		tc.setWidth(18);
		tc.setMaxWidth(18);
		
		// Setup the first col 
		tc = tcm.getColumn(1);
		String[] infoColIcons = { "", "/info.gif"};
		tc.setCellRenderer(new InfoRenderer(infoColIcons));
		tc.setWidth(18);
		tc.setMaxWidth(18);
		
		tc = tcm.getColumn(LogEntry.FIELD_ENTRYTYPE + 2);
		tc.setCellRenderer(new EntryTypeRenderer(LogTypeHelper.getAllIcons()));

		tc = tcm.getColumn(LogEntry.FIELD_TIMESTAMP + 2);
		tc.setCellRenderer(new com.cosylab.logging.client.DateRenderer());

		SortableHeaderRenderer shr = new SortableHeaderRenderer();

		int n = tcm.getColumnCount();

		columnsList = new TableColumn[n];
		visibleColumns = new boolean[n];
		for (int i = 0; i < n; i++)
		{
			columnsList[i] = tcm.getColumn(i);
			visibleColumns[i] = true;
			if (i == LogEntry.FIELD_LOGMESSAGE+2)
			{
				columnsList[i].setPreferredWidth(250);
			}
			columnsList[i].setHeaderRenderer(shr);
		}

		columnsList[0].setResizable(false);
		columnsList[1].setResizable(false);
		addMouseListener(new ColumnMouseAdapter());
		getTableHeader().addMouseListener(new headerMouseAdapter());

		setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
		sizeColumnsToFit(JTable.AUTO_RESIZE_OFF);

        // Hide some columns (default at startuup)
		hideColumn(LogEntry.FIELD_LINE+2);
		hideColumn(LogEntry.FIELD_ROUTINE+2);
		hideColumn(LogEntry.FIELD_HOST+2);
		hideColumn(LogEntry.FIELD_PROCESS+2);
		hideColumn(LogEntry.FIELD_CONTEXT+2);
		hideColumn(LogEntry.FIELD_THREAD+2);
		hideColumn(LogEntry.FIELD_LOGID+2);
		hideColumn(LogEntry.FIELD_PRIORITY+2);
		hideColumn(LogEntry.FIELD_URI+2);
        hideColumn(LogEntry.FIELD_STACKID+2);
        hideColumn(LogEntry.FIELD_FILE+2);
        hideColumn(LogEntry.FIELD_STACKLEVEL+2);

		sortMenu = new SortMenu();
		sortMenu.rebuild();
		
		// Build and set the slection model
		selectionModel = new DefaultListSelectionModel();
		selectionModel.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		setSelectionModel(selectionModel);

		//	firePropertyChange("filterString", "", getFilterString());

	}
	/**
	 * Returns true if the table entries are currently grouped.
	 * Creation date: (1/24/02 11:02:49 AM)
	 * @return boolean
	 */
	public boolean isGrouped()
	{
		return (getLCModel().getGroupComparator() != null);
	}
	/**
	 * Returns true if the elements are sorted in ascending order and false if in descending.
	 * If the table is not sorted, this parameter has no meaning.
	 * Creation date: (1/24/02 10:49:21 AM)
	 * @return boolean
	 */
	public boolean isSortAscending()
	{
		LogEntryComparator comparator = getLCModel().getSortComparator();
		if (comparator == null)
			return false;
		else
			return comparator.isAscending();

	}
	/**
	 * Sets the index of the column the elements of this table will be grouped by. This
	 * index can be same as sort index. Setting to a negative value will ungroup the table.
	 * Creation date: (1/22/02 6:25:06 PM)
	 * @param index int
	 */
	public void setGroupIndex(int index)
	{
		if (index > -1)
		{
			// showColumn(0);
			getLCModel().setGroupComparator(new LogEntryComparator((short) index, true));
			int i = convertColumnIndexToView(index + 1);
			if (i > -1)
				getColumnModel().moveColumn(i, 1);

			if (groupMenu != null)
				groupMenu.rebuild();
		}
		else
		{
			//	    hideColumn(0);
			getLCModel().setGroupComparator(null);
			if (groupMenu != null)
				groupMenu.rebuild();
		}
	}
	/**
	 * Sets the column the elements in this table should be sorted by. This value can be the
	 * same as the group index. Setting it to negative value means the table is not sorted.
	 * Creation date: (1/22/02 6:24:52 PM)
	 * @param index int
	 */
	public void setSortIndex(int index)
	{
		LogTableDataModel ltdm = getLCModel();

		if (index < 0)
		{
			ltdm.setSortComparator(null);
		}
		else
		{
			ltdm.setSortComparator(new LogEntryComparator((short) index, isSortAscending()));
		}

	}
	/**
	 * Changes the order in which the elements are sorted. Set true for ascending and false
	 * for descending. If the table is currently not sorted, this method does nothing.
	 * Creation date: (1/22/02 6:29:52 PM)
	 * @param ascending boolean
	 */
	public void setSortOrder(boolean ascending)
	{
		getLCModel().setSortComparator(new LogEntryComparator((short) getSortIndex(), ascending));
	}

	/**
	 * Displays the column specified by index.
	 * Creation date: (12/4/2001 22:56:11)
	 * @param columnIndex int
	 */
	public void showColumn(int columnIndex)
	{
		if ((columnIndex > 0) && (columnIndex < columnsList.length))
		{
			TableColumnModel tcm = getColumnModel();
			if (!visibleColumns[columnIndex])
			{
				tcm.addColumn(columnsList[columnIndex]);
				visibleColumns[columnIndex] = true;
			}
			int w = columnsList[0].getWidth();
			(columnsList[0]).setWidth(w + 1);
			(columnsList[0]).setWidth(w);
		}
		if (sortMenu != null)
			sortMenu.rebuild();
	}
	/**
	 * Displays the field chooser dialog.
	 * <p>
	 * Creation date: (1/2/2002 23:20:27)
	 */
	public void showFieldChooser()
	{
		int nFields = LogEntry.NUMBER_OF_FIELDS;

		String[] fieldNames = new String[nFields];
		boolean[] fieldVisible = getVisibleColumns(true);

		for (int i = 0; i < nFields; i++)
		{
			fieldNames[i] = LogEntry.getFieldDescription(i);
		}

		fieldChooser.setupFields(fieldNames, fieldVisible);

		fieldChooser.setVisible(true);

		boolean[] newFields = fieldChooser.getChecked();

		for (int i = 0; i < nFields; i++)
		{
			if (newFields[i] != fieldVisible[i])
			{
				if (newFields[i])
					showColumn(i + 2);
				else
					hideColumn(i + 2);
			}
		}

	}
    
    /**
     * Returns an array of boolean with true for each visible column
     * The visibleComun array of this class starts counting the column 
     * from 1 instead of 0 (i.e. 0 is not used)
     * 
     * @param zeroBased If true the number 0 correspond to the first column 
     *                  (the array of visible comuns uses 2 for the first column)
     * @return An array of boolean describing which columns are displayed
     */
    public boolean[] getVisibleColumns(boolean zeroBased) {
        int nFields = LogEntry.NUMBER_OF_FIELDS;
        // The array of visible columns to return
        boolean[] visibleCols = new boolean[nFields];
        
        int pad =(!zeroBased)?0:2;
        
        for (int i = 0; i < nFields; i++)
        {
            visibleCols[i] = visibleColumns[i + pad];
        }
        
        return visibleCols;
    }
    
	/**
	 * Displays the filter and history configuration dialog.
	 * Creation date: (2/4/02 3:57:44 PM)
	 */
	public void showFilterChooser()
	{
		FilterChooserDialog fcd = new FilterChooserDialog();
		LogTableDataModel ltdm = getLCModel();

		fcd.setModal(true);

		fcd.setupFields(ltdm.getFilters());

		if (fcd.showModal() == FilterChooserDialog.MODAL_OK)
		{
			ltdm.getFilters().setFilters(fcd.getFilters(), fcd.getChecked());
			firePropertyChange("filterString", "", getFilterString());
			ltdm.invalidateVisibleLogs();
		}
	}
	/**
	 * Insert the method's description here.
	 * <p>
	 * Creation date: (2/13/2002 20:45:11)
	 */
	protected void updateExtraInfo()
	{
		firePropertyChange("extraInfo", null, null);
	}
	
	/**
	 * Scroll the table in such a way the given row is visible
	 * 
	 * @param row The row to make visible
	 */
	public synchronized void showRow (int row) {
		JScrollPane scrollPane = loggingClient.getLogTable();
		//loggingClient.getScrollPaneTable().changeSelection(row,1,false,false);
		//scrollRectToVisible(getCellRect(row,0,true));
		/*JViewport vPort = scrollPane.getViewport();
		
		Rectangle visible = getVisibleRect();
	    
		Rectangle cellRect = getCellRect(row,0,true);
		if (visible.contains(cellRect)) {
			return;
		}
		
		Rectangle dest = new Rectangle(visible);
		
		if (cellRect.y>visible.y+visible.height) {
			dest.y = cellRect.y-visible.height-cellRect.height;
			if (dest.y<0) {
				dest.y=0;
			}
		}
		
		System.out.println("visible="+visible.toString()+
				",  cell="+cellRect.toString());
		System.out.println("row = "+row+", rowCount = "+getRowCount());
		System.out.println("VP Viewport = "+vPort.getViewRect().toString());
		System.out.println("VP ViewSize = "+vPort.getViewSize().toString()+", size="+vPort.getExtentSize().toString());
		System.out.println("VP VisibleRect = "+vPort.getVisibleRect().toString());
	    
		if (!visible.contains(dest)) {
			System.out.println("Scrolling from "+visible.toString()+" to "+dest.toString());
			scrollPane.scrollRectToVisible(dest);
		}*/
	}
	
}
