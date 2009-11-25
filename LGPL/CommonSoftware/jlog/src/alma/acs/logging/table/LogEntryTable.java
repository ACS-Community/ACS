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
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.swing.JComponent;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.JTable;
import javax.swing.ProgressMonitor;
import javax.swing.RowSorter;
import javax.swing.SortOrder;
import javax.swing.event.RowSorterEvent;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.swing.ListSelectionModel;
import javax.swing. DefaultListSelectionModel;

import com.cosylab.logging.LoggingClient;
import com.cosylab.logging.client.EntryTypeIcon;
import com.cosylab.logging.engine.FiltersVector;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.engine.log.LogField;
import com.cosylab.logging.settings.FieldChooserDialog;

import alma.acs.logging.archive.zoom.ZoomManager;
import alma.acs.logging.archive.zoom.ZoomProgressListener;
import alma.acs.logging.table.renderer.DateRenderer;
import alma.acs.logging.table.renderer.EntryTypeRenderer;
import alma.acs.logging.table.renderer.InfoRenderer;
import alma.acs.util.IsoDateFormat;

/**
 * Subclasses JTable allowing grouping and sorting depending on user's input. 
 * Creation date: (11/11/2001 13:45:22)
 * @author: 
 */

public class LogEntryTable extends JTable implements ZoomProgressListener {
	private TableColumn[] columnsList;
	private boolean[] visibleColumns;
	private FieldChooserDialog fieldChooser = null;
	
	private LoggingClient loggingClient;
	
	private DefaultListSelectionModel selectionModel;
	
	/**
	 * The object to sort, order and filter the logs shown by the table
	 */
	private LogTableRowSorter rowSorter;	
	
	/**
	 * The index (in the view!!!) of the row selected by the user.
	 * <P>
	 * <code>-1</code> means no row selected.
	 */
	private int selecteViewdRow =- 1;
	
	/**
	 * The index (in the model!!!) of the row selected by the user.
	 * <P>
	 * <code>-1</code> means no row selected.
	 */
	private int selecteModelRow =- 1;
	
	/**
	 * The key of the last log selected by the user.
	 * <P>
	 * <code>-1</code> means no log selected.
	 */
	private int selecteLogKey =- 1;
	
	/**
	 * The renderer to show the date (short or complete format)
	 */
	private DateRenderer dateRenderer;
	
	/**
	 * The renderer to show the type of log (icon and description or icon only)
	 */
	private EntryTypeRenderer logTypeRenderer;
	
	/**
	 * The dialog to show (and stop) the progress of the zoom
	 */
	private ProgressMonitor zoomProgressMonitor=null;
	
	/**
	 * The total number of files to read while zooming
	 */
	private int zoomTotFiles;

	/**
	 * Popup menu used to display the column options for the table.
	 * It is displayed when the user presses the right mouse button over the
	 * header of the table
	 * 
	 * Creation date: (1/25/02 11:16:28 AM)
	 * @author: acaproni
	 */
	public class ColumnMenu extends JPopupMenu
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

		private class MenuClicked implements ActionListener
		{

			public void actionPerformed(ActionEvent e)
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

	/**
	 * Get events from mouse
	 * 
	 * @author acaproni
	 *
	 */
	private class TableMouseAdapter extends MouseAdapter	{
		
		private TablePopupMenu popupMenu = new TablePopupMenu(loggingClient,LogEntryTable.this);
		
		@Override
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
				if (getSelectedRowCount()>0) {
					String textUnderMouse = getCellStringContent(row, col); 
					popupMenu.show(LogEntryTable.this,e.getX(),e.getY(),row,textUnderMouse);
				}
			}
			loggingClient.getToolBar().setZoomable(getSelectedRowCount()>1 && loggingClient.getZoomManager().isAvailable());
		}

	}
	
	/**
	 * LogEntryTable constructor.
	 * 
	 * @param logClient The LoggingClient that owns this table
	 * @param initialDateFormat The format to show the date (true means short)
	 * @param initalLogTypeFormat If <code>true</code> shows the written close 
	 *                            to the icon of the log type  
	 */
	public LogEntryTable(LoggingClient client,boolean initialDateFormat, boolean initalLogTypeFormat) throws Exception
	{
		super();
		if (client==null) {
			throw new IllegalArgumentException("Invalid null LoggingClient!");
		}
		loggingClient=client;
		// Set the table model
		LogTableDataModel model= new LogTableDataModel(client);
		setModel(model);
		
		// Initialize the sorter (unsorted/unfiltered
		rowSorter = new LogTableRowSorter(model);
		setRowSorter(rowSorter);
		// Initially sort by timestamp
		List<RowSorter.SortKey> sortKeys = new ArrayList<RowSorter.SortKey>();
		sortKeys.add(new RowSorter.SortKey(LogField.TIMESTAMP.ordinal()+1, SortOrder.DESCENDING));
		rowSorter.setSortKeys(sortKeys); 
		
		initialize(initialDateFormat,initalLogTypeFormat);
		
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
		Component c; 
		try {
			c = super.prepareRenderer(renderer, rowIndex, vColIndex);
		} catch (ArrayIndexOutOfBoundsException e) {
			return null;
		}
		LogTooltipHelper.setToolTip((JComponent)c,tooltipTxt,96);
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
			if (getColumnName(col).compareTo(LogField.ENTRYTYPE.getName())==0) {
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
	 * @param shortDateFormat The format to show the date (if <code>true</code> is short, otherwise complete)
	 * @param logTypeformat The way to show the log type (if <code>true</code> the description is shown)
	 */
	private void initialize(boolean shortDateFormat, boolean logTypeformat)
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
		
		tc = tcm.getColumn(LogField.ENTRYTYPE.ordinal() + 1);
		logTypeRenderer = new EntryTypeRenderer(logTypeformat);
		tc.setCellRenderer(logTypeRenderer);

		tc = tcm.getColumn(LogField.TIMESTAMP.ordinal() + 1);
		dateRenderer = new DateRenderer(shortDateFormat);
		tc.setCellRenderer(dateRenderer);

		int n = tcm.getColumnCount();

		columnsList = new TableColumn[n];
		visibleColumns = new boolean[n];
		for (int i = 0; i < n; i++)
		{
			columnsList[i] = tcm.getColumn(i);
			visibleColumns[i] = true;
			if (i == LogField.LOGMESSAGE.ordinal()+1)
			{
				columnsList[i].setPreferredWidth(250);
			}
		}

		setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
		sizeColumnsToFit(JTable.AUTO_RESIZE_OFF);

        // Hide some columns (default at startup)
		hideColumn(LogField.LINE.ordinal()+1);
		hideColumn(LogField.ROUTINE.ordinal()+1);
		hideColumn(LogField.HOST.ordinal()+1);
		hideColumn(LogField.PROCESS.ordinal()+1);
		hideColumn(LogField.CONTEXT.ordinal()+1);
		hideColumn(LogField.THREAD.ordinal()+1);
		hideColumn(LogField.LOGID.ordinal()+1);
		hideColumn(LogField.PRIORITY.ordinal()+1);
		hideColumn(LogField.URI.ordinal()+1);
        hideColumn(LogField.STACKID.ordinal()+1);
        hideColumn(LogField.FILE.ordinal()+1);
        hideColumn(LogField.STACKLEVEL.ordinal()+1);
        hideColumn(LogField.AUDIENCE.ordinal()+1);
        hideColumn(LogField.ARRAY.ordinal()+1);
        hideColumn(LogField.ANTENNA.ordinal()+1);

		// Build and set the selection model
		selectionModel = new DefaultListSelectionModel();
		selectionModel.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		setSelectionModel(selectionModel);

		addMouseListener(new TableMouseAdapter());
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
		String[] fieldNames = new String[LogField.values().length];
		int t=0;
		for (LogField f: LogField.values()) {
			fieldNames[t++]=f.getName();
		}
		boolean[] fieldVisible = getVisibleColumns(true);
		
		if (fieldChooser==null) {
			fieldChooser = new FieldChooserDialog(loggingClient);
		}
		fieldChooser.setupFields(fieldNames, fieldVisible);

		fieldChooser.setVisible(true);

		boolean[] newFields = fieldChooser.getChecked();

		for (int i = 0; i < LogField.values().length; i++)
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
        int nFields = LogField.values().length;
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
	 * Show/Hide the description of the log type at the right side of the icon.
	 * <P>
	 * When the description is disabled, the icon appear without the written.
	 * The column is also resized to use less space as possible.
	 * 
	 * @param showDescription If <code>true</code> a string with the description 
	 * 						  of the log appear at the right side of the icon (default)
	 */
	public void setLogTypeDescriptionView(boolean showDescription) {
		logTypeRenderer.viewDescription(showDescription);
		TableColumn logTypeCol = columnsList[LogField.ENTRYTYPE.ordinal()+1];
		if (!showDescription) {
			logTypeCol.setPreferredWidth(EntryTypeIcon.INFO_ICON.icon.getIconWidth());
		} else {
			logTypeCol.setPreferredWidth(4*EntryTypeIcon.INFO_ICON.icon.getIconWidth());
		}
		this.repaint();
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
			selecteViewdRow=rowIndex;
			selecteModelRow=convertRowIndexToModel(rowIndex);
			selecteLogKey=((LogTableDataModel)getModel()).getLogKey(selecteModelRow);
		}
	}
	
	/**
	 * Scroll the table to the next selected row and select it.
	 * <P> 
	 * If there is no row selected or the selected line is the last row of the table,
	 * the method return without scrolling the table.
	 */
	public void scrollToNextSelectedRow() {
		if (selecteViewdRow==-1 || selecteViewdRow==getRowCount()-1) {
			return;
		}
		changeSelection(selecteViewdRow+1,1,false,false);
	}
	
	/**
	 * Scroll the table to the next selected row and select it.
	 * <P> 
	 * If there is no row selected or the selected row is the first row of the table,
	 * the method return without scrolling the table
	 */
	public void scrollToPrevSelectedRow() {
		if (selecteViewdRow<=0) {
			return;
		}
		changeSelection(selecteViewdRow-1,1,false,false);
	}
	
	/**
	 * Scroll the table to the first row
	 */
	public void scrollToFirstRow() {
		changeSelection(0,1,false,false);
	}
	
	/**
	 * Scroll the table to the last row
	 */
	public void scrollToLastRow() {
		changeSelection(getRowCount()-1,1,false,false);
	}
	
	/**
	 * Scroll the table to the selected row
	 * <P>
	 * If there is no selected row, the method return without scrolling.
	 * <P>
	 * The selected row could not be anymore present in the table for example because it has
	 * been deleted by the <code>LogDeleter</code> or the table has been cleared by the user.
	 */
	public void scrollToSelectedRow() {
		if (selecteViewdRow==-1) {
			return;
		}
		int modelRow=((LogTableDataModel)getModel()).findKeyPos(selecteLogKey);
		if (modelRow!=-1) {
			changeSelection(convertRowIndexToView(modelRow),1,false,false);
		}
	}
	
	/** 
	 * Return the filters defined by the user.
	 * 
	 * @return The user defined filters
	 */
	public FiltersVector getFilters() {
		return rowSorter.getFilters();
	}
	
	/**
	 * 
	 * @param newFilters
	 * @param append
	 */
	public void setFilters(FiltersVector newFilters, boolean append) {
		rowSorter.setFilters(newFilters, append);
	}
	
	/**
	 * 
	 * @return A description of the active filters
	 * @see FiltersVector.getFilterString()
	 */
	public String getFiltersString() {
		return rowSorter.getFiltersString();
	}
	
	/**
	 * Set the new log level i.e. the level of the logs shown in the table.
	 * 
	 * @param newLevel
	 */
	public void setLogLevel(LogTypeHelper newLevel) {
		rowSorter.setLogLevel(newLevel);
	}
	
	/**
	 * Shows the number of rows shown by the table.
	 * 
	 * @return the number of rows shown by the table.
	 */
	public int getViewRowCount() {
		return rowSorter.getViewRowCount();
	}

	/* (non-Javadoc)
	 * @see javax.swing.JTable#sorterChanged(javax.swing.event.RowSorterEvent)
	 */
	@Override
	public void sorterChanged(RowSorterEvent e) {
		if (e.getType()==RowSorterEvent.Type.SORT_ORDER_CHANGED) {
			setVisible(false);
			loggingClient.setEnabled(false);
			super.sorterChanged(e);
			loggingClient.animateProgressBar("Sorting");
		} else {
			if (e.getType()==RowSorterEvent.Type.SORTED) {
				loggingClient.freezeProgressBar();
				super.sorterChanged(e);
				loggingClient.setEnabled(true);
				setVisible(true);
			}
		}
	}
	
	/**
	 * Zoom over the selected logs.
	 * <P>
	 * The zoom consists of loading all the logs in the time interval
	 * defined by the selected logs.
	 * The zoom is delegated to the <code>ZoomManager</code>.
	 * 
	 *  @see {@link ZoomManager}
	 */
	public void zoom() {
		if (getSelectedRowCount()<=1) {
			return;
		}
		int[] indexes = getSelectedRows();
		long startDate=Long.MAX_VALUE;
		long endDate=0;
		zoomTotFiles=0;
		System.out.println("Keys "+indexes.length);
		for (int i: indexes) {
			ILogEntry log= getLCModel().getVisibleLogEntry(convertRowIndexToModel(i));
			long time =((Long)log.getField(LogField.TIMESTAMP)).longValue();
			if (time<startDate) {
				startDate=time;
			}
			if (time>endDate) {
				endDate=time;
			}
		}
		String startDateStr=IsoDateFormat.formatDate(new Date(startDate));
		String endDateStr=IsoDateFormat.formatDate(new Date(endDate));
		System.out.println("Zooming from "+startDateStr+" to "+endDateStr);
		try {
			loggingClient.getZoomManager().zoom(startDateStr, endDateStr, loggingClient, this, loggingClient);
		} catch (Throwable t) {
			JOptionPane.showMessageDialog(
					loggingClient, 
					"Error while loading logs.\n"+t.getMessage(), 
					"Zoom error", 
					JOptionPane.ERROR_MESSAGE);
		}
		if (zoomProgressMonitor!=null) {
			zoomProgressMonitor.close();
			zoomProgressMonitor=null;
		}
	}

	/* (non-Javadoc)
	 * @see alma.acs.logging.archive.zoom.ZoomProgressListener#zoomReadingFile(int)
	 */
	@Override
	public void zoomReadingFile(int num) {
		if (zoomProgressMonitor!=null) {
			zoomProgressMonitor.setProgress(num);
			zoomProgressMonitor.setNote("Reading "+num+"/"+zoomTotFiles);
			if (zoomProgressMonitor.isCanceled()) {
				loggingClient.getZoomManager().stopZoom();
			}
		}
	}

	/* (non-Javadoc)
	 * @see alma.acs.logging.archive.zoom.ZoomProgressListener#zoomTotalFileToRead(int)
	 */
	@Override
	public void zoomTotalFileToRead(int num) {
		zoomTotFiles=num;
		zoomProgressMonitor= new ProgressMonitor(this,"Zoom",null,0,num);
		System.out.println(""+num+" files to read while zoom");
	}
	
}
