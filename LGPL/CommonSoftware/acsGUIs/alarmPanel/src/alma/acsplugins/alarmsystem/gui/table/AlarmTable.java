/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2007
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */

/** 
 * @author  acaproni
 * @version $Id: AlarmTable.java,v 1.25 2012/10/16 09:14:19 acaproni Exp $
 * @since    
 */

package alma.acsplugins.alarmsystem.gui.table;

import java.awt.Component;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileOutputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.swing.DefaultListSelectionModel;
import javax.swing.ImageIcon;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.RowFilter;
import javax.swing.RowSorter;
import javax.swing.SortOrder;
import javax.swing.ToolTipManager;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableRowSorter;

import alma.acs.util.IsoDateFormat;
import alma.acs.gui.util.threadsupport.EDTExecutor;
import alma.acsplugins.alarmsystem.gui.CernSysPanel;
import alma.acsplugins.alarmsystem.gui.reduced.ReducedChainDlg;
import alma.acsplugins.alarmsystem.gui.table.AlarmTableModel.AlarmTableColumn;
import alma.acsplugins.alarmsystem.gui.table.AlarmTableModel.PriorityLabel;
import alma.acsplugins.alarmsystem.gui.undocumented.table.UndocAlarmTableModel;
import alma.acsplugins.alarmsystem.gui.viewcoordination.ViewCoordinator;
import alma.acsplugins.alarmsystem.gui.viewcoordination.ViewCoordinator.AlarmSelectionListener;
import alma.alarmsystem.clients.CategoryClient;
import cern.laser.client.data.Alarm;

/**
 * 
 * The table of alarms
 *
 */
public class AlarmTable extends JTable implements ActionListener {
	
	/**
	 * The mouse adapter receiving mouse events generated
	 * over the table of the alarms
	 * 
	 * @author acaproni
	 *
	 */
	private class AlarmTableMouseAdapter extends MouseAdapter {
		
		/**
		 * The last selected alarm
		 * 
		 * It is set when the user presses over a row (i,e. selects an alarm)
		 */
		public AlarmTableEntry selectedAlarm;
		/**
		 * @see MouseListener
		 */
		public void mouseClicked(MouseEvent e) {
			alarmSelected(e);
			showPopup(e);
		}

		/**
		 * @see MouseListener
		 */
		public void mousePressed(MouseEvent e) {
			alarmSelected(e);
			showPopup(e);
		}

		/**
		 * @see MouseListener
		 */
		public void mouseReleased(MouseEvent e) {
			alarmSelected(e);
			showPopup(e);
		}
		
		/**
		 * The user selected a row, i.e. an alarm.
		 * alarmSected notifies the model that the icon must be removed
		 * 
		 * @param e The event to get the selected row from
		 */
		private void alarmSelected(MouseEvent e) {
			int row=rowAtPoint(new Point(e.getX(),+e.getY()));
			AlarmTable.this.model.alarmSelected(getRowSorter().convertRowIndexToModel(row));
		}
		
		/**
		 * Show the popup menu
		 * 
		 * @param e The mouse event that triggered the pop
		 */
		private void showPopup(final MouseEvent e) {
			if (!e.isPopupTrigger()) {
				return;
			}
			int row=rowAtPoint(new Point(e.getX(),+e.getY()));
			selectedAlarm = AlarmTable.this.model.getRowAlarm(getRowSorter().convertRowIndexToModel(row));
			EDTExecutor.instance().execute(new Runnable() {
				@Override
				public void run() {
					ackMI.setEnabled(!selectedAlarm.getStatus().isActive());
					showReducedMI.setEnabled(selectedAlarm!=null && (selectedAlarm.isParent()));
					popupM.show(e.getComponent(),e.getX(),e.getY());
					popupM.setVisible(true);
				}
			});	
		}
	}
	
	/**
	 * The mouse adapter receiving mouse events generated
	 * over the header of the table of the alarms
	 * 
	 * @author acaproni
	 *
	 */
	private class AlarmHeaderMouseAdapter extends MouseAdapter implements ActionListener {
		
		// The popup
		private JPopupMenu headerPopup = new JPopupMenu("Header");
		private JCheckBoxMenuItem[] menuItems;
		
		/**
		 * Constructor
		 */
		public AlarmHeaderMouseAdapter() {
			// Build the popup menu
			//
			// Each item has the same order of the AlarmTableColumn
			menuItems = new JCheckBoxMenuItem[AlarmTableColumn.values().length];
			int t=0;
			for (AlarmTableColumn col: AlarmTableColumn.values()) {
				menuItems[t]= new JCheckBoxMenuItem(col.popupTitle);
				menuItems[t].addActionListener(this);
				headerPopup.add(menuItems[t++]);
			}
		}

		/**
		 * @see MouseListener
		 */
		public void mouseClicked(MouseEvent e) {
			showPopup(e);
		}

		/**
		 * @see MouseListener
		 */
		public void mousePressed(MouseEvent e) {
			showPopup(e);
		}

		/**
		 * @see MouseListener
		 */
		public void mouseReleased(MouseEvent e) {
			showPopup(e);
		}
		
		/**
		 * Show the popup menu
		 * 
		 * @param e The mouse event that triggered the pop
		 */
		private void showPopup(final MouseEvent e) {
			if (!e.isPopupTrigger()) {
				return;
			}
			EDTExecutor.instance().execute(new Runnable() {
				@Override
				public void run() {
					ratioMenu();
					headerPopup.show(e.getComponent(),e.getX(),e.getY());
				}
			});	
		}
		
		/**
		 * Set/unset all the checkbox depending on the
		 * visible/hidden columns
		 */
		private void ratioMenu() {
			TableColumnModel colModel = getColumnModel();
			int pos=0;
			for (AlarmTableColumn col: AlarmTableColumn.values()) {
				try {
					colModel.getColumnIndex(col);
					menuItems[pos].setSelected(true);
				} catch (IllegalArgumentException iae) {
					menuItems[pos].setSelected(false);
				}
				pos++;
			}
		}

		/**
		 * @see ActionListener
		 */
		public void actionPerformed(ActionEvent e) {
			// Look for the source of this event
			JCheckBoxMenuItem source=null;
			int column=-1;
			for (int t=0; t<menuItems.length; t++) {
				if (e.getSource()==menuItems[t]) {
					source=menuItems[t];
					column=t;
					break;
				}
			}
			if (source==null) {
				System.out.println("Unknown source of event: "+e.getSource());
				return;
			}
			addRemoveColumn(AlarmTableColumn.values()[column], source.isSelected());
		}
		
	}
	
	/**
	 * The filter of table entries.
	 * <P>
	 * Accepts all the entries containing the passed string in one of the
	 * visible columns.
	 * <P>
	 * The filter can be applied to hide entries instead of select (i.e. NOT logic)
	 * 
	 * @author acaproni
	 *
	 */
	private class AlarmTableFilter extends RowFilter<AlarmTableModel,Integer> {
		
		/**
		 * If <code>true</code> the selected entries are those that do not
		 * contain the filter string
		 */
		private boolean applyAsNot=false;
		
		/**
		 * The string to compare to the visible columns to accept the antries
		 */
		private String filterString;
		
		/**
		 * Set the filter
		 * 
		 * @param flt The string used to filter
		 * @param not If <code>true</code> the filter is applied with a NOT policy
		 */
		public void setFilter(String flt, boolean not) {
			if (flt==null || flt.isEmpty()) {
				throw new IllegalArgumentException("The string for filtering can't be null nor empty");
			}
			applyAsNot=not;
			filterString=flt;
		}

		@Override
		public boolean include(
			Entry<? extends AlarmTableModel, ? extends Integer> entry) {
			boolean ret=false;
			// TODO Auto-generated method stub
			TableColumnModel colModel = getColumnModel();
			int modelRow=entry.getIdentifier();
			for (int t=0; t< colModel.getColumnCount(); t++) {
				TableColumn tc=colModel.getColumn(t);
				int idx=tc.getModelIndex();
				Object obj = model.getValueAt(modelRow,idx);
				if (!(obj instanceof String)) {
					continue;
				}
				if (obj.toString().contains(filterString)) {
					ret= true;
				}
			}
			if (applyAsNot) {
				return !ret;
			}
			return ret;
		}
		
	}
	
	/** 
	 * The model of the table
	 */
	private final AlarmTableModel model;
	
	/**
	 * The panel showing this table
	 */
	private final CernSysPanel panel;
	
	/**
	 * The sorter for sorting the rows of the table
	 */
	private TableRowSorter<AlarmTableModel> sorter;
	
	/**
	 * The table selection model
	 */
	private DefaultListSelectionModel selectionModel;
	
	/**
	 * The cols of the table
	 */
	private TableColumn[] columns;
	
	/**
	 *  The alarm adapter that recives events from the mouse
	 */
	private AlarmTableMouseAdapter mouseAdapter = new AlarmTableMouseAdapter();
	
	/**
	 * The filter of the table activate from the toolbar
	 * <P>
	 * This filter is added or removed from the tale filters depending if the 
	 * user select or unselect the toolbar button
	 */
	private final AlarmTableFilter filter = new AlarmTableFilter();
	
	/**
	 *  The clipboard
	 */
	private ClipboardHelper clipboard = new ClipboardHelper();
	
	/**
	 *  The popup menu shown when the user presses the right mouse button over a row
	 */
	private JPopupMenu popupM = new JPopupMenu("Alarm");
	
	/**
	 * The menu item to acknowledge an alarm
	 */
	private JMenuItem ackMI = new JMenuItem("Acknowledge");
	
	/**
	 * The menu item to save
	 */
	private JMenuItem saveMI = new JMenuItem("Save...");
	
	/**
	 * The menu item to svae the selected alarm into the clipboard
	 */
	private JMenuItem clipMI = new JMenuItem("To clipboard");
	
	/**
	 * The menu to show the reduction chain of an alarm 
	 */
	private JMenuItem showReducedMI = new JMenuItem("Show reduction chain");
	
	/**
	 *  The label returned as renderer when no flag is shown in the 
	 * first column of the table
	 */
	private JLabel emptyLbl = new JLabel();
	
	/**
	 * The dialog showing the table with the alarms involved in a
	 * reduction chain 
	 */
	private ReducedChainDlg reducedDlg=null;
	
	/**
	 * The engine to search alarm entries in the table
	 */
	private final SearchEngine searchEngine;
	
	/**
	 * The ID of the last selected alarm for painting in bold
	 */
	private String selectedAlarmId=null;
	
	/**
	 *  The renderer for the reduced alarm entries i.e.
	 *  the entries normally hidden
	 */
	public static final JLabel reductionRenderer = new JLabel(
			new ImageIcon(AlarmGUIType.class.getResource(AlarmGUIType.iconFolder+"arrow_in.png")),
			JLabel.CENTER);
	
	/**
	 * The renderer for a node that hides children because of a
	 * reduction rule is in place
	 */
	public static final JLabel hasReducedNodesRenderer = new JLabel(
			new ImageIcon(AlarmGUIType.class.getResource(AlarmGUIType.iconFolder+"add.png")),
			JLabel.CENTER);
	
	/**
	 * The undocumented table model
	 */
	private final UndocAlarmTableModel undocModel;

	private volatile AlarmSelectionListener listener;
	
	/**
	 * Constructor 
	 * @param model The model for this table
	 * @param panel The panel showing this table
	 */
	public AlarmTable(AlarmTableModel model, CernSysPanel panel, UndocAlarmTableModel undocModel) {
		super(model);
		if (model==null) {
			throw new IllegalArgumentException("Invalid null model in constructor");
		}
		this.model=model;
		if (undocModel==null) {
			throw new IllegalArgumentException("Invalid null undocumented model in constructor");
		}
		this.undocModel=undocModel;
		if (panel==null) {
			throw new IllegalArgumentException("Invalid null panel in constructor");
		}
		this.panel=panel;
		EDTExecutor.instance().execute(new Runnable() {
			public void run() {
				initGUI();
			}
		});
		searchEngine=new SearchEngine(this,model);
	}
	
	/**
	 * Init the GUI
	 */
	private void initGUI() {
		setShowHorizontalLines(true);
		// Build and set the selection model
		selectionModel = new DefaultListSelectionModel();
		selectionModel.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		setSelectionModel(selectionModel);
		this.setOpaque(false);
		sorter = new TableRowSorter<AlarmTableModel>(model);
		this.setRowSorter(sorter);
		sorter.setMaxSortKeys(2);
		sorter.setSortsOnUpdates(true);
		// Initially sort by timestamp
		List<RowSorter.SortKey> sortKeys = new ArrayList<RowSorter.SortKey>();
		sortKeys.add(new RowSorter.SortKey(AlarmTableColumn.PRIORITY.ordinal(), SortOrder.ASCENDING));
		sortKeys.add(new RowSorter.SortKey(AlarmTableColumn.TIME.ordinal(), SortOrder.DESCENDING));
		sorter.setSortKeys(sortKeys); 

		
		// Remove all the columns not visible at startup
		TableColumnModel colModel = getColumnModel();
		columns = new TableColumn[colModel.getColumnCount()];
		for (int t=0; t<columns.length; t++) {
			columns[t]=colModel.getColumn(t);
			columns[t].setIdentifier(AlarmTableColumn.values()[t]);
			if (columns[t].getIdentifier()==AlarmTableColumn.ICON || 
					columns[t].getIdentifier()==AlarmTableColumn.IS_CHILD ||
					columns[t].getIdentifier()==AlarmTableColumn.IS_PARENT) {
				columns[t].setWidth(20);
				columns[t].setResizable(false);
				columns[t].setPreferredWidth(20);
				columns[t].setMaxWidth(20);
				columns[t].setMinWidth(20);
			} else if (columns[t].getIdentifier()==AlarmTableColumn.PRIORITY) {
				BufferedImage bImg = new BufferedImage(100,100,BufferedImage.TYPE_INT_RGB);
				Graphics2D g2D= bImg.createGraphics();
				FontMetrics fm=g2D.getFontMetrics();
				int sz=fm.stringWidth(PriorityLabel.VERY_HIGH.description);
				columns[t].setPreferredWidth(sz+6);
				columns[t].setMaxWidth(sz+8);
			}
		}
		for (AlarmTableColumn col: AlarmTableColumn.values()) {
			if (!col.visibleAtStartup) {
				colModel.removeColumn(columns[col.ordinal()]);
			} 
		}
		buildPopupMenu();
		addMouseListener(mouseAdapter);
		
		getTableHeader().addMouseListener(new AlarmHeaderMouseAdapter());
		
		// Set the tooltip
		ToolTipManager ttm = ToolTipManager.sharedInstance();
		ttm.setDismissDelay(Integer.MAX_VALUE);
		ttm.setLightWeightPopupEnabled(true);
	}
	
	/**
	 * Build the popup menu
	 */
	private void buildPopupMenu() {
		popupM.add(ackMI);
		popupM.add(showReducedMI);
		popupM.add(new JSeparator());
		popupM.add(saveMI);
		popupM.add(clipMI);
		popupM.pack();
		
		ackMI.addActionListener(this);
		saveMI.addActionListener(this);
		clipMI.addActionListener(this);
		showReducedMI.addActionListener(this);
	}
	
	/**
	 * @see JTable
	 */
	@Override
	public Component prepareRenderer(TableCellRenderer renderer, int rowIndex, int vColIndex) {
		
		TableColumn col = getColumnModel().getColumn(vColIndex);
		AlarmTableEntry entry=null; 
		try {
			entry = model.getRowEntry(sorter.convertRowIndexToModel(rowIndex));
		} catch (Throwable t) {
			// This can happen if the entry has been removed by the thread while
			// this method runs.
			entry=null;
		}
		if (entry==null) {
			return emptyLbl;
		}
		if (col.getIdentifier().equals(AlarmTableColumn.ICON)) {
			if (model.isRowAlarmNew(sorter.convertRowIndexToModel(rowIndex))) {
				return AlarmGUIType.fromAlarm(entry).flagRenderer;
			} else {
				return emptyLbl;
			}
		} else if (col.getIdentifier().equals(AlarmTableColumn.IS_CHILD))  {
			if (entry.isChild()) {
				return AlarmTable.reductionRenderer;
			} else {
				return emptyLbl;
			}
		} else if (col.getIdentifier().equals(AlarmTableColumn.IS_PARENT)) {
			if (entry.isParent()) {
				return AlarmTable.hasReducedNodesRenderer;
			} else {
				return emptyLbl;
			}
		}
		Component c = super.prepareRenderer(renderer, rowIndex, vColIndex);
		if (entry.getAlarmId().equals(selectedAlarmId)) {
			Font f = c.getFont();
			Font bold=f.deriveFont(Font.BOLD);
			c.setFont(bold);
		}
		colorizeCell(c, entry);
	
		if (c instanceof JComponent) {
			JComponent jc = (JComponent) c;
			if (((AlarmTableModel)model).getCellContent(sorter.convertRowIndexToModel(rowIndex), convertColumnIndexToModel(vColIndex))==null) {
				jc.setToolTipText(null);
			} else { 
				jc.setToolTipText("<HTML>"+((AlarmTableModel)model).getCellContent(sorter.convertRowIndexToModel(rowIndex), convertColumnIndexToModel(vColIndex)));
			}
		}
		return c;
	}
	
	/**
	 * Set the background and the foreground of the component depending
	 * on the priority and the state of the passed alarm
	 * 
	 * @param c The component to color
	 * @param priority The alarm to set the color
	 */
	private void colorizeCell(final Component c, final AlarmTableEntry alarm) {
		EDTExecutor.instance().execute(new Runnable() {
			public void run() {
				AlarmGUIType alarmType = AlarmGUIType.fromAlarm(alarm);
				c.setForeground(alarmType.foreg);
				c.setBackground(alarmType.backg);		
			}
		});
	}

	/**
	 * @see ActionListener
	 */
	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==saveMI) {
			saveAlarm(mouseAdapter.selectedAlarm);
		} else if (e.getSource()==clipMI) {
			clipboard.setClipboardContents(mouseAdapter.selectedAlarm.toString());
		} else if (e.getSource()==ackMI) {
			model.acknowledge(mouseAdapter.selectedAlarm);
		} else if (e.getSource()==showReducedMI) {
			showReductionChain(mouseAdapter.selectedAlarm);
		}
	}
	
	/**
	 * Save the alarm in a file
	 * 
	 * @param The alarm to save in a plain text file
	 */
	public void saveAlarm(final Alarm alarm) {
		// Get the user dir property
		final JFileChooser fileChooser = new JFileChooser();
		if (fileChooser.showSaveDialog(this)!=JFileChooser.APPROVE_OPTION) {
			return;
		}
		// Save the file
		new Thread(new Runnable(){
			public void run() {
				// Build the text to write
				SimpleDateFormat dateFormat = new IsoDateFormat();
				StringBuilder str = new StringBuilder(alarm.toString());
				str.append("\n\n");
				str.append("Saved at ");
				str.append(dateFormat.format(new Date(System.currentTimeMillis())));
				str.append("\n\n");
				// Save the file
				File outF = fileChooser.getSelectedFile();
				FileOutputStream fOutS;
				try {
					fOutS = new FileOutputStream(outF,false);
					fOutS.write(str.toString().getBytes());
					fOutS.flush();
					fOutS.close();
				} catch (Exception e) {
					JOptionPane.showInternalMessageDialog(AlarmTable.this, e.getMessage(), "Error saving", JOptionPane.ERROR_MESSAGE);
				}		
			}
		},"SaveThread").start();
	}
	
	/**
	 * Free all the resource 
	 */
	public void close() {
		if (reducedDlg!=null) {
			reducedDlg.close();
		}
	}
	
	/**
	 * Show the dialog with all the nodes reduced by the passed alarm
	 * 
	 * @param alarm The alarm whose children must be shown in a dialog
	 */
	private void showReductionChain(AlarmTableEntry alarm) {
		if (alarm==null) {
			throw new IllegalArgumentException("The alarm can't be null");
		}
		CategoryClient client = model.getCategoryClient();
		if (reducedDlg==null) {
			reducedDlg = new ReducedChainDlg(client,alarm,panel,undocModel);
		} else {
			reducedDlg.setRootAlarm(alarm);
			EDTExecutor.instance().execute(new Runnable() {
				public void run() {
					reducedDlg.setVisible(true);
				}
			});
		}
	}
	
	/**
	 * Set the visible columns in the table.
	 * The columns are displayed following their order in the array.
	 * 
	 * @param cols The visible columns in the table;
	 * 			it can't be <code>null</code> and at least one column must be in the array.
	 */
	public void showColumns(final AlarmTableColumn[] cols) {
		if (cols==null || cols.length==0) {
			throw new IllegalArgumentException("Invalid columns array");
		}
		EDTExecutor.instance().execute(new Runnable() {
			public void run() {
				TableColumnModel colModel = getColumnModel();
				// Remove all the columns
				for (TableColumn column: columns) {
					colModel.removeColumn(column);
				}
				for (AlarmTableColumn aTC: cols) {
					for (int t=0; t<columns.length; t++) {
						if (columns[t].getIdentifier()==aTC) {
							colModel.addColumn(columns[t]);
							break;
						}
					}
				}
			}
		});
	}
	
	/**
	 * Add/remove one column from the table
	 * 
	 * @param col The column to add or remove
	 * @param add If <code>true</code> add the column, otherwise remove the column
	 */
	public void addRemoveColumn(final AlarmTableColumn col, final boolean add) {
		if (col==null) {
			throw new IllegalArgumentException("The column to add/remove can't be null");
		}
		EDTExecutor.instance().execute(new Runnable() {
			public void run() {
				TableColumnModel colModel = getColumnModel();
				if (add) {
					colModel.addColumn(columns[col.ordinal()]);
				} else {
					colModel.removeColumn(columns[col.ordinal()]);
				}
			}
		});
	}

	/**
	 * Search for a string in the table
	 * 
	 * @param string The string to search in the table
	 * @param next If <code>true</code> search for the next entry
	 * @return <code>true</code> if an entry has been found
	 * 
	 * @see SearchEngine
	 */
	public boolean search(String string, boolean next) {
		int ret= searchEngine.search(string, next);
		if (ret!=-1) {
			changeSelection(ret, 1, false, false);
			panel.showMessage("Entry found at "+ret, false);
		} else {
			panel.showMessage("No alarm found",true);
		}
		return ret!=-1;
	}
	
	/**
	 * Override {@link JTable#changeSelection(int, int, boolean, boolean)} to show
	 * the selected alarm in the detail panel.
	 */
	@Override
	public void changeSelection(int rowIndex, int columnIndex, boolean toggle, boolean extend) {
		super.changeSelection(rowIndex, columnIndex, toggle, extend);
		int idx = sorter.convertRowIndexToModel(rowIndex);
		AlarmTableEntry alarmEntry = model.getAlarmAt(idx);
		panel.showAlarmDetails(alarmEntry);
		selectedAlarmId = alarmEntry.getAlarmId();
		
		// TODO 1: Is this the right place to intercept the selection, or should we get it from AlarmTableMouseAdapter#alarmSelected ?
		// TODO 2: Check if this gets called also for alarm de-selection (if that exists...)
		AlarmSelectionListener listenerCopy = listener; // to avoid concurrency issues
		if (listenerCopy != null) {
			Alarm alarm = alarmEntry.getEncapsulatedAlarm();
			if (alarm != null) {
				listenerCopy.notifyAlarmSelected(alarm);
			}
			else {
				listenerCopy.notifyAlarmDeselected();
			}
		}
	}
	
	/**
	 * TODO: This is used only by {@link ViewCoordinator}. Check if we can use a standard listener type.
	 */
	public void setAlarmSelectionListener(AlarmSelectionListener listener) {
		this.listener = listener;
	}
	
	/**
	 * Filter the table by the passed string
	 * <P>
	 * Filtering select all the rows containing the passed string 
	 * in at least on one (visible) column.
	 * 
	 * @param filterString The string used to filter;
	 * 			if <code>null</code> or empty, the table is unfiltered
	 * @param not If the filter must be applied to discard entries instead of to select
	 */
	public void filter(String filterString, boolean not) {
		if (filterString==null || filterString.isEmpty()) {
			// remove the filter
			sorter.setRowFilter(null);
		} else {
			// add the filter
			filter.setFilter(filterString, not);
			sorter.setRowFilter(filter);
		}
	}

	/**
	 * Return the id of the selected alarm;
	 * the id is <code>null</code> if no alarm is selected;
	 * <P>
	 * This method does not ensure that the alarm with the ID
	 * returned by this method is still in the table: it might have been
	 * removed when the panel discards alarms.
	 * 
	 * @return The id of the selected alarm or <code>null</code>
	 * 			if no alarm is selected
	 */
	private String getSelectedAlarmId() {
		return selectedAlarmId;
	}
}

