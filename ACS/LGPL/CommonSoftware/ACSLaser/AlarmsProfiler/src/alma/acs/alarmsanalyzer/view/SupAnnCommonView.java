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

import alma.acs.alarmsanalyzer.document.SuppressedContainer;
import alma.acs.alarmsanalyzer.document.SuppressedContainer.ReductionValue;

/**
 * The Suppressed and the annunciated view are almost identical: this class groups
 * common methods and properties.
 * 
 * @author acaproni
 *
 */
public class SupAnnCommonView extends TableViewBase {
	/**
	 * The label provider
	 * 
	 * @author acaproni
	 *
	 */
	private class CommonLabelProvider extends LabelProvider implements ITableLabelProvider {

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
			ReductionValue item=(ReductionValue)element;
			
			switch (columnIndex) {
			case 0: return item.ID;
			case 1: return ""+item.getValue();
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
	private class CommonTableSorter extends TableSorterBase {

		
		/**
		 * Constructor
		 * 
		 * @param colIndex The index of the column to sort
		 * @param order The order ascending/descending
		 */
		public CommonTableSorter(int colIndex, int order) {
			super(colIndex,order);
		}

		@Override
		public int compare(Viewer viewer, Object e1, Object e2) {
			ReductionValue a1 = (ReductionValue) e1;
			ReductionValue a2 = (ReductionValue) e2;
			int ret = 0;
			switch (colIndex) {
			case 0:
				ret = a1.ID.compareTo(a2.ID);
				break;
			case 1:
				ret = Integer.valueOf(a1.getValue()).compareTo(a2.getValue());
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
	public SupAnnCommonView() {
		super(SuppressedContainer.getInstance().colNames,new int[] {
				500,
				100
		});
		sorter=new CommonTableSorter(0, TableSorterBase.ASCENDING);
	}
	
	@Override
	public void createPartControl(Composite parent) {
		super.createPartControl(parent);
		tableViewer.setLabelProvider(new CommonLabelProvider());
		tableViewer.setSorter(sorter);
	}
}
