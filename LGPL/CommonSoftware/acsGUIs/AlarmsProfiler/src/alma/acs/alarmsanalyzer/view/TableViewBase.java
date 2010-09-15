/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2010
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
package alma.acs.alarmsanalyzer.view;

import java.util.Collection;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.part.ViewPart;

import alma.acs.alarmsanalyzer.document.DocumentBase;

public abstract class TableViewBase  extends ViewPart {
	
	/** 
	 * The content provider
	 * 
	 * @author acaproni
	 *
	 */
	class ViewContentProvider implements IStructuredContentProvider {
		public void inputChanged(Viewer v, Object oldInput, Object newInput) {
		}
	
		public void dispose() {
		}
	
		public Object[] getElements(Object parent) {
			DocumentBase container=(DocumentBase)parent;
			Collection<?> items=container.getNumbers();
			if (items==null) {
				return new Object[0];
			} else {
				return items.toArray();
			}
		}
	}
	
	/**
	 * A base class to sort the table by ascending/descending order of a column
	 * 
	 * @author acaproni
	 *
	 */
	public abstract class TableSorterBase extends ViewerSorter {
		/**
		 * Ascending order
		 */
		public static final int ASCENDING=0;
		
		/**
		 * Descending order
		 */
		public static final int DESCENDING=1;
		
		/**
		 * The number of column to sort
		 */
		protected int colIndex;
		
		/**
		 * The order ascending/descending
		 */
		protected int order;
		
		/**
		 * Constructor
		 * 
		 * @param colIndex The index of the column to sort
		 * @param order The order ascending/descending
		 */
		public TableSorterBase(int colIndex, int order) {
			this.colIndex=colIndex;
			this.order=order;
		}
		
		/**
		 * Set the column to order 
		 * 
		 * @param column The index of the column used for ordering
		 */
		public void setColumn(int column) {
			if (column==colIndex) {
				// Same column as last sort; toggle the direction
				order=1-order;
			} else {
				// New column; do an ascending sort
				colIndex=column;
				order=DESCENDING;
			}
		}
		
		@Override
		public abstract int compare(Viewer viewer, Object e1, Object e2);
	}
	
	/**
	 * The names of the columns
	 */
	private final String colNames[];
	
	/**
	 * The width of the columns
	 */
	private final int colWidths[];
	
	/**
	 * The table viewer
	 */
	protected TableViewer tableViewer;
	
	/**
	 * The sorter
	 */
	protected TableSorterBase sorter;
	
	public TableViewBase(String[] colNames, int[] colWidths) {
		this.colNames=colNames;
		this.colWidths=colWidths;
	}
	
	@Override
	public void createPartControl(Composite parent) {
		tableViewer = new TableViewer(parent,SWT.SINGLE| SWT.H_SCROLL| SWT.V_SCROLL);
		
		Table table = tableViewer.getTable();
		table.setLinesVisible(true);
		table.setHeaderVisible(true);
		
		// Set the column
		for (int t=0; t<colNames.length; t++) {
			int style = t==0?SWT.LEFT:SWT.CENTER;
			final TableViewerColumn viewerColumn = new TableViewerColumn(tableViewer, style);
			final TableColumn column = viewerColumn.getColumn();
			column.setText(colNames[t]);
			
			final int index=t;
			// Setting the right sorter
			column.addSelectionListener(new SelectionAdapter() {
				@Override
				public void widgetSelected(SelectionEvent e) {
					sorter.setColumn(index);
					int dir = tableViewer.getTable().getSortDirection();
					if (tableViewer.getTable().getSortColumn() == column) {
						dir = dir == SWT.UP ? SWT.DOWN : SWT.UP;
					} else {

						dir = SWT.DOWN;
					}
					tableViewer.getTable().setSortDirection(dir);
					tableViewer.getTable().setSortColumn(column);
					tableViewer.refresh();
				}
			});
			
			column.pack();
			column.setWidth(colWidths[t]);
		}
		
		tableViewer.setContentProvider(new ViewContentProvider());
		
	}

	@Override
	public void setFocus() {
		tableViewer.getControl().setFocus();
	}
}
