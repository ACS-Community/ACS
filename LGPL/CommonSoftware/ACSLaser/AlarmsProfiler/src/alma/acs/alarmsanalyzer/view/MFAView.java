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

import alma.acs.alarmsanalyzer.document.MostFrequantAlarmsContainer;
import alma.acs.alarmsanalyzer.document.MostFrequantAlarmsContainer.AlarmActNumber;

public class MFAView extends TableViewBase {
	/**
	 * The ID of this view
	 */
	public static final String ID = "alma.acs.alarmsanalyzer.view";
	
	/**
	 * The label provider
	 * 
	 * @author acaproni
	 *
	 */
	private class AlarmLabelProvider extends LabelProvider implements ITableLabelProvider {

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
			AlarmActNumber item=(AlarmActNumber)element;
			switch (columnIndex) {
			case 0: return item.getAlarmID();
			case 1: return ""+item.getNumActivation();
			case 2: if (item.getLastActivationTime()==null){
				return "";
			} else {
				return ""+item.getLastActivationTime();
			}
			case 3: return ""+item.getNumTermination();
			case 4: if (item.getLastTerminationTime()==null) {
				return "";
				} else {
					return ""+item.getLastTerminationTime();
			}
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
	class TableSorter extends TableSorterBase {
		
		
		/**
		 * Constructor
		 * 
		 * @param colIndex The index of the column to sort
		 * @param order The order ascending/descending
		 */
		public TableSorter(int colIndex, int order) {
			super(colIndex,order);
		}
		
		@Override
		public int compare(Viewer viewer, Object e1, Object e2) {
			AlarmActNumber a1 = (AlarmActNumber) e1;
			AlarmActNumber a2 = (AlarmActNumber) e2;
			int ret = 0;
			switch (colIndex) {
			case 0:
				ret = a1.getAlarmID().compareTo(a2.getAlarmID());
				break;
			case 1:
				ret = (int)a1.getNumActivation()-(int)a2.getNumActivation();
				break;
			case 2:
				if (a1.getLastActivationTime()==null) {
					ret=-1;
				} else if (a2.getLastActivationTime()==null) {
					ret=+1;
				} else {
					ret = a1.getLastActivationTime().compareTo(a2.getLastActivationTime());
				}
				break;
			case 3:
				 ret=(int)a1.getNumTermination()-(int)a2.getNumTermination();
				break;
			case 4:
				if (a1.getLastTerminationTime()==null) {
					ret =-1;
				} else if (a2.getLastTerminationTime()==null) {
					ret=+1;
				} else {
					ret=a1.getLastTerminationTime().compareTo(a2.getLastTerminationTime());
				}
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
	
	public MFAView() {
		super(MostFrequantAlarmsContainer.getInstance().colNames,new int[] {
			250,
			100,
			150,
			100,
			150
	});
		sorter=new TableSorter(0, TableSorter.DESCENDING);
	}

	@Override
	public void createPartControl(Composite parent) {
		super.createPartControl(parent);
		tableViewer.setLabelProvider(new AlarmLabelProvider());
		tableViewer.setSorter(sorter);
		MostFrequantAlarmsContainer.getInstance().setTableViewer(tableViewer);
		tableViewer.setInput(MostFrequantAlarmsContainer.getInstance());
	}


}