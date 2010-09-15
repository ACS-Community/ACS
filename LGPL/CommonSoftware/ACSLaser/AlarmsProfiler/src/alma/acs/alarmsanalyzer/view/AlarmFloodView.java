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

import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

import alma.acs.alarmsanalyzer.document.flood.FloodContainer;
import alma.acs.alarmsanalyzer.document.flood.FloodContainer.FloodItem;
import alma.acs.alarmsanalyzer.view.MFAView.TableSorter;

public class AlarmFloodView extends TableViewBase {
	/**
	 * The ID of this view
	 */
	public static final String ID = "alma.acs.alarmsanalyzer.alarmflood";
	
	 /* The label provider
	 * 
	 * @author acaproni
	 *
	 */
	private class FloodLabelProvider extends LabelProvider implements ITableLabelProvider {

		/**
		 * No image
		 */
		@Override
		public Image getColumnImage(Object element, int columnIndex) {
			return null;
		}

		/**
		 * Extract the text from the object
		 */
		@Override
		public String getColumnText(Object element, int columnIndex) {
			if (element==null) {
				return "N/A";
			}
			FloodItem item=(FloodItem)element;
			
			switch (columnIndex) {
			case 0: return item.description;
			case 1: return ""+item.toString();
			default: return "Unknown!";
			}
		}
		
		public Image getImage(Object obj) {
			return PlatformUI.getWorkbench().getSharedImages().getImage(
				ISharedImages.IMG_OBJ_ELEMENT);
		}
		
	};
	
	/**
	 * A class to sort the table by ascending/descending order of a column
	 * 
	 * @author acaproni
	 *
	 */
	private class FloodTableSorter extends TableSorterBase {

		
		/**
		 * Constructor
		 * 
		 * @param colIndex The index of the column to sort
		 * @param order The order ascending/descending
		 */
		public FloodTableSorter(int colIndex, int order) {
			super(colIndex,order);
		}

		@Override
		public int compare(Viewer viewer, Object e1, Object e2) {
			FloodItem a1 = (FloodItem) e1;
			FloodItem a2 = (FloodItem) e2;
			int ret = 0;
			switch (colIndex) {
			case 0:
				ret = a1.description.compareTo(a2.description);
				break;
			case 1:
				ret = a1.toString().compareTo(a2.toString());
				break;
			default:
				ret = 0;
			}
			// If descending order, flip the direction
			if (order == DESCENDING) {
				ret = -ret;
			}
			return ret;
		}

	}

	/**
	 * Constructor
	 */
	public AlarmFloodView() {
		super(FloodContainer.getInstance().colNames,
				new int[] {
				300,
				80
		});
		sorter=new FloodTableSorter(0, TableSorter.ASCENDING);
	}

	@Override
	public void createPartControl(Composite parent) {
		super.createPartControl(parent);
		tableViewer.setLabelProvider(new FloodLabelProvider());
		tableViewer.setSorter(sorter);
		FloodContainer.getInstance().setTableViewer(tableViewer);
		tableViewer.setInput(FloodContainer.getInstance());
	}

}
