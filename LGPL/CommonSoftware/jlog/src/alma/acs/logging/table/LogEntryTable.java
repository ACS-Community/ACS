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
package alma.acs.logging.table;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseListener;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.JTable;
import javax.swing.RowSorter;
import javax.swing.SortOrder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableRowSorter;
import javax.swing.ListSelectionModel;
import javax.swing. DefaultListSelectionModel;

import com.cosylab.logging.LoggingClient;
import com.cosylab.logging.SortableHeaderRenderer;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.engine.log.ILogEntry.Field;
import com.cosylab.logging.settings.FieldChooserDialog;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.ClipboardOwner;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.Toolkit;
import java.awt.Font;
import java.awt.FontMetrics;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;

import com.cosylab.logging.settings.UserInfoDlg;


import alma.acs.logging.table.renderer.DateRenderer;
import alma.acs.logging.table.renderer.EntryTypeRenderer;
import alma.acs.logging.table.renderer.InfoRenderer;
import alma.acs.util.IsoDateFormat;

/**
 * Subclasses JTable allowing grouping and sorting depending on user's input. 
 * Creation date: (11/11/2001 13:45:22)
 * @author: 
 */

public class LogEntryTable extends JTable {
	private TableColumn[] columnsList;
	private boolean[] visibleColumns;
	private FieldChooserDialog fieldChooser = null;
	private ColumnMenu popupMenu = new ColumnMenu();
	
	private LoggingClient loggingClient;
	
	public TextTransfer textTransfer;
	
	private DefaultListSelectionModel selectionModel;
	
	/**
	 * The row selected in the previous iteration
	 * -1 means no row selected. 
	 */
	private int oldSelectedRow =- 1;
	
	/**
	 * The renderer to show the date (short or complete format)
	 */
	private DateRenderer dateRenderer;

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
		// The menu item to save selected logs
		private ImageIcon icon =new ImageIcon(LogTypeHelper.class.getResource("/disk.png"));
		private JMenuItem saveSelected = new JMenuItem("Save selected logs...",icon);
		// The text to copy
		private String textToCopy;
		// The row and column under the mouse pointer
		private int row;
		
		/**
		 * The constructor builds the menu
		 *
		 */
		public ColumnPopupMenu(int row, int col, String text)
		{
			// Copy the parameter to local variables
			this.row=row;
			// A make a copy of the original string 
			this.textToCopy=new String(text);
			// Add the Label
			setLabel("Paste");
			
			// Add the menu items
			add(saveSelected);
			addSeparator();
			add(copyClipboard);
			add(copyAddInfo);
			addSeparator();
			add(addUserInfo);
			
			// Hide the unneeded buttons
			boolean singleLineSelected =
				selectionModel.getMinSelectionIndex() == selectionModel.getMaxSelectionIndex(); 
			copyAddInfo.setEnabled(singleLineSelected);
			addUserInfo.setEnabled(singleLineSelected);
			copyClipboard.setEnabled(true);
			saveSelected.setEnabled(true);
			
			// Add the listeners
			copyClipboard.addActionListener(this);
			copyAddInfo.addActionListener(this);
			addUserInfo.addActionListener(this);
			saveSelected.addActionListener(this);
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
		    //       because we're using a tree in the additional info pane
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
							ILogEntry log = getLCModel().getVisibleLogEntry(convertRowIndexToModel(i));
							strBuffer.append(log.toXMLString());
							strBuffer.append("\n");
						}
					}
					// Copy the text to the clipboard
					textTransfer.setClipboardContents(strBuffer.toString());
				} 
			} else if (e.getSource()==copyAddInfo) {
				// Copy the text to the additionalInfo field
				ILogEntry logEntry = getLCModel().getVisibleLogEntry(convertRowIndexToModel(row));
				logEntry.addData("Generated by jlog",textToCopy);
				getLCModel().replaceLog(row,logEntry);
				
				LogTableDataModel tableModel = (LogTableDataModel) getModel();
				tableModel.fireTableRowsUpdated(row,row);
				setRowSelectionInterval(row,row);
				
				loggingClient.setLogDetailContent(logEntry);
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
					
					ILogEntry logEntry = getLCModel().getVisibleLogEntry(convertRowIndexToModel(row));
					logEntry.addData(name,value);
					getLCModel().replaceLog(row,logEntry);
					
					LogTableDataModel tableModel = (LogTableDataModel) getModel();
					tableModel.fireTableRowsUpdated(row,row);
					setRowSelectionInterval(row,row);
					
					loggingClient.setLogDetailContent(logEntry);
				}
			} else if (e.getSource()==saveSelected) {
				saveSelectedLogs();
			} else {
				System.err.println("Unhandled event "+e);
			}
		}
		
		/**
		 * Save the selected logs into a file
		 *
		 */
		private void saveSelectedLogs() {
			// Build the text to save in the file
			StringBuilder strBuffer = new StringBuilder();
			if (!selectionModel.isSelectionEmpty()) {
				for (int i=selectionModel.getMinSelectionIndex(); 
					i<=selectionModel.getMaxSelectionIndex(); i++) {
					if (!selectionModel.isSelectedIndex(i)) {
						continue;
					} else {
						ILogEntry log = getLCModel().getVisibleLogEntry(i);
						strBuffer.append(log.toXMLString());
						strBuffer.append("\n");
					}
				}
				if (strBuffer.length()==0) {
					// Nothing to save
					return;
				}
					
			}
			JFileChooser fc = new JFileChooser();
			if (fc.showSaveDialog(null) == JFileChooser.APPROVE_OPTION) {
				File file = fc.getSelectedFile();
				FileOutputStream outFStream=null;
				try {
					outFStream = new FileOutputStream(file);
				} catch (FileNotFoundException fnfe) {
					JOptionPane.showMessageDialog(null,"Error creating "+file.getAbsolutePath()+":\n"+fnfe.getMessage(),"Error saving logs",JOptionPane.ERROR_MESSAGE);
					return;
				}
				try {
					outFStream.write(strBuffer.toString().getBytes());
					outFStream.flush();
					outFStream.close();
				} catch (IOException ioe) {
					JOptionPane.showMessageDialog(null,"Error saving "+file.getAbsolutePath()+":\n"+ioe.getMessage(),"Error saving logs",JOptionPane.ERROR_MESSAGE);
				}
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
						let.setOrdering(menu.getColumnIndex(),true);
						break;
					case 1 :
						let.setOrdering(menu.getColumnIndex(),false);
						break;
					case 3 :
						let.hideColumn(menu.getColumnIndex() + 1);
						break;
					case 4 :
						let.showFieldChooser();
						break;
					case 6 :
						//let.setGroupIndex(menu.getColumnIndex());
						break;
					case 7 :
						//let.setGroupIndex(-1);
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
			int col = getColumnModel().getColumnIndexAtX(e.getX());
			int row = LogEntryTable.this.rowAtPoint(e.getPoint());
			
			// Handle the specific event for each mouse button
			if (e.isPopupTrigger()) {
				// The mouse button is the pop up trigger so we show the menu
				// but only if the user selected at least one row
				if (selectionModel.getMaxSelectionIndex()>=0) {
					String textUnderMouse = getCellStringContent(row, col);
					ColumnPopupMenu popMenu = new ColumnPopupMenu(row, col, textUnderMouse);
					popMenu.show(LogEntryTable.this,e.getX(),e.getY());
				}
			}
		}

	}
	
	/**
	 * LogEntryTable constructor.
	 * 
	 * @param logClient The LoggingClient that owns this table
	 * @param initialDateFormat The format to show the date (true means short)
	 */
	public LogEntryTable(LoggingClient client,boolean initialDateFormat) throws Exception
	{
		super();
		if (client==null) {
			throw new IllegalArgumentException("Invalid null LoggingClient!");
		}
		loggingClient=client;
		// Set the table model
		LogTableDataModel model= new LogTableDataModel(client);
		setModel(model);
		
		TableRowSorter<LogTableDataModel> rowSorter = new TableRowSorter<LogTableDataModel>();
		rowSorter.setModel(model);
		
		List<RowSorter.SortKey> sortKeys = new ArrayList<RowSorter.SortKey>();
		sortKeys.add(new RowSorter.SortKey(1,SortOrder.DESCENDING));
		rowSorter.setSortKeys(sortKeys);
		rowSorter.setRowFilter(null);
		
		super.setRowSorter(rowSorter);
		
		initialize(initialDateFormat);
		
		// Create the object for the clipboard
		textTransfer = new TextTransfer();
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
	
	/**
	 * Insert the method's description here.
	 * Creation date: (2/7/02 3:52:36 PM)
	 * @return java.lang.String
	 */
	public org.w3c.dom.Node getExtraInfo()
	{
		System.out.println("getExtraInfo");
		//	String noInfoString = ;

		//	org.w3c.dom.Node nn = new org.w3c.dom.Node.;
		//	DataNode dn = new DataNode(null);
		//	dn.setNodeValue("No additional information");

		int index = getSelectedRow();
		if (index < 0)
			return null;

		ILogEntry additionalInfo = getLCModel().getVisibleLogEntry(index);
		if (!additionalInfo.hasDatas()) {
			return null;
		}

		return null; //additionalInfo.getDatas().item(0);
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
	 * Returns the LogTableDataModel. This is a convenience method that returns properly
	 * case data model.
	 * 
	 * Creation date: (11/24/2001 18:44:41)
	 * @return com.cosylab.logging.client.LCLogTableDataModel
	 */
	public LogTableDataModel getLCModel()
	{
		return (LogTableDataModel) getModel();
	}
	/**
	 * Sets the index of the column the table should be sorted by.
	 * If the table is currently unsorted, the result is -1.
	 * 
	 * @return int
	 */
	public int getSortIndex()
	{
		return getLCModel().getFieldSortNumber();
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
			IsoDateFormat sdf = new IsoDateFormat();
			tempStr = sdf.format(value);
		} else if (value instanceof Integer) {
			if (getColumnName(col).compareTo(Field.ENTRYTYPE.getName())==0) {
				tempStr=LogTypeHelper.values()[((Integer)value).intValue()].logEntryType;
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
	 * 
	 * @param The format to show the date (if true is short, otherwise complete)
	 */
	private void initialize(boolean shortDateFormat)
	{
		createDefaultColumnsFromModel();
		setShowHorizontalLines(false);
		TableColumnModel tcm = getColumnModel();
		
		// Setup the first col 
		TableColumn tc;
		
		// Setup the first col 
		tc = tcm.getColumn(0);
		tc.setCellRenderer(new InfoRenderer());
		tc.setWidth(18);
		tc.setMaxWidth(18);
		tc.setResizable(false);
		
		tc = tcm.getColumn(Field.ENTRYTYPE.ordinal() + 1);
		tc.setCellRenderer(new EntryTypeRenderer());

		tc = tcm.getColumn(Field.TIMESTAMP.ordinal() + 1);
		dateRenderer = new DateRenderer(shortDateFormat);
		tc.setCellRenderer(dateRenderer);

		SortableHeaderRenderer shr = new SortableHeaderRenderer();

		int n = tcm.getColumnCount();

		columnsList = new TableColumn[n];
		visibleColumns = new boolean[n];
		for (int i = 0; i < n; i++)
		{
			columnsList[i] = tcm.getColumn(i);
			visibleColumns[i] = true;
			if (i == Field.LOGMESSAGE.ordinal()+1)
			{
				columnsList[i].setPreferredWidth(250);
			}
			columnsList[i].setHeaderRenderer(shr);
		}

		addMouseListener(new ColumnMouseAdapter());

		setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
		sizeColumnsToFit(JTable.AUTO_RESIZE_OFF);

        // Hide some columns (default at startuup)
		hideColumn(Field.LINE.ordinal()+1);
		hideColumn(Field.ROUTINE.ordinal()+1);
		hideColumn(Field.HOST.ordinal()+1);
		hideColumn(Field.PROCESS.ordinal()+1);
		hideColumn(Field.CONTEXT.ordinal()+1);
		hideColumn(Field.THREAD.ordinal()+1);
		hideColumn(Field.LOGID.ordinal()+1);
		hideColumn(Field.PRIORITY.ordinal()+1);
		hideColumn(Field.URI.ordinal()+1);
        hideColumn(Field.STACKID.ordinal()+1);
        hideColumn(Field.FILE.ordinal()+1);
        hideColumn(Field.STACKLEVEL.ordinal()+1);
        hideColumn(Field.AUDIENCE.ordinal()+1);
        hideColumn(Field.ARRAY.ordinal()+1);
        hideColumn(Field.ANTENNA.ordinal()+1);

		// Build and set the selection model
		selectionModel = new DefaultListSelectionModel();
		selectionModel.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		setSelectionModel(selectionModel);

		//	firePropertyChange("filterString", "", getFilterString());

	}

	/**
	 * Returns true if the elements are sorted in ascending order and false if in descending.
	 * If the table is not sorted, this parameter has no meaning.
	 * Creation date: (1/24/02 10:49:21 AM)
	 * @return boolean
	 */
	public boolean isSortAscending()
	{
		return getLCModel().sortedAscending();
	}
	
	/**
	 * Sets the column the elements in this table should be sorted by. 
	 * Setting it to -1 means the table is not sorted.
	 * 
	 * To change both the index and the order, it is better
	 * to execute setOrdering
	 * 
	 * @param index int
	 */
	public void setSortIndex(int index)
	{
		LogTableDataModel ltdm = getLCModel();
		ltdm.setSortComparator(index,ltdm.sortedAscending());
	}

	/**
	 * Changes the order in which the elements are sorted. 
	 * Set true for ascending and false for descending.
	 * 
	 * To change both the index and the order, it is better
	 * to execute setOrdering
	 * 
	 * @param ascending The order for the table
	 */
	public void setSortOrder(boolean ascending)
	{
		LogTableDataModel ltdm = getLCModel();
		ltdm.setSortComparator(ltdm.getFieldSortNumber(),ascending);
	}
	
	/** 
	 * Set the order and the field for ordering at once.
	 * Calling this method is faster the calling the setSortIndex 
	 * and setSortOrder
	 * 
	 * @param field The field of the logs for ordering
	 *              -1 disable the ordering
	 * @param ascending The order ascending(true)/descending (false)
	 * 
	 */
	public void setOrdering(int field, boolean ascending) {
		getLCModel().setSortComparator(field,ascending);
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
	}
	/**
	 * Displays the field chooser dialog.
	 * <p>
	 * Creation date: (1/2/2002 23:20:27)
	 */
	public void showFieldChooser()
	{
		String[] fieldNames = new String[Field.values().length];
		int t=0;
		for (Field f: Field.values()) {
			fieldNames[t++]=f.getName();
		}
		boolean[] fieldVisible = getVisibleColumns(true);
		
		if (fieldChooser==null) {
			fieldChooser = new FieldChooserDialog(loggingClient);
		}
		fieldChooser.setupFields(fieldNames, fieldVisible);

		fieldChooser.setVisible(true);

		boolean[] newFields = fieldChooser.getChecked();

		for (int i = 0; i < Field.values().length; i++)
		{
			if (newFields[i] != fieldVisible[i])
			{
				if (newFields[i])
					showColumn(i + 1);
				else
					hideColumn(i + 1);
			}
		}

	}
    
    /**
     * Returns an array of boolean with true for each visible column
     * The visibleComun array of this class starts counting the column 
     * from 1 instead of 0 (i.e. 0 is not used)
     * 
     * @param zeroBased If true the number 0 correspond to the first column 
     *                  (the array of visible columns uses 1 for the first column)
     * @return An array of boolean describing which columns are displayed
     */
    public boolean[] getVisibleColumns(boolean zeroBased) {
        int nFields = Field.values().length;
        // The array of visible columns to return
        boolean[] visibleCols = new boolean[nFields];
        
        int pad =(!zeroBased)?0:1;
        
        for (int i = 0; i < nFields; i++)
        {
            visibleCols[i] = visibleColumns[i + pad];
        }
        
        return visibleCols;
    }
    
    /**
     * Close the filterChooser dialog releasing all the resources.
     * This is intended to be the last operatione when the application 
     * is closing
     *
     */
    public void close() {
    	if (fieldChooser!=null) {
    		fieldChooser.setVisible(false);
    		fieldChooser.dispose();
    		fieldChooser=null;
    	}
    }
    	
	/**
	 * Set the format used to show the timestamp in the date column
	 * 
	 * @param shortFormat The format of the date (true means short, false means complete)
	 */
	public void setShortDateFormat(boolean shortFormat) {
		dateRenderer.setShortDateFormat(shortFormat);
		this.repaint();
	}
	
	/**
	 * Fire a property change event that triggers a refresh
	 * in the string displayed at the bottom of the main 
	 * GUI (LoggingClient)
	 * 
	 *
	 */
	public void updateFilteredString() {
		firePropertyChange("filterString", "", getFilterString());
	}
	
	/**
	 * Override the method in <code>JTable</code> to catch the change of selection operated 
	 * by the user and update the detailed log info accordingly.
	 * 
	 * @see <code>JTable.changeSelection(int rowIndex, int columnIndex, boolean toggle,boolean extend)</code>
	 */
	@Override
	public void changeSelection(int rowIndex, int columnIndex, boolean toggle,
			boolean extend) {
		super.changeSelection(rowIndex, columnIndex, toggle, extend);
		if (rowIndex!=-1 && !toggle && !extend) {
			LogTableDataModel model =(LogTableDataModel)getModel();
			ILogEntry log = model.getVisibleLogEntry(convertRowIndexToModel(rowIndex));
			loggingClient.setLogDetailContent(log);
		}
	}
	
}
