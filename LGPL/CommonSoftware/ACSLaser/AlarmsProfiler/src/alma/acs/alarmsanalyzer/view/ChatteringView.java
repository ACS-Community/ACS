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

import alma.acs.alarmsanalyzer.document.ChatteringAlarmsContainer;
import alma.acs.alarmsanalyzer.document.ChatteringAlarmsContainer.ChatteringAlarm;

/**
 * The view of chattering alarms.
 * <P>
 * Chattering alarms are alarms whose state changes (ACTIVE/TERMINATE) at least 
 * 3 times per minute.
 * 
 * @author acaproni
 *
 */
public class ChatteringView  extends TableViewBase  {
	
	/**
	 * The ID of this view
	 */
	public static final String ID = "alma.acs.alarmsanalyzer.chatteringview";
	
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
			ChatteringAlarm item=(ChatteringAlarm)element;
			switch (columnIndex) {
			case 0: return item.ID;
			case 1: return ""+item.getNumActive();
			case 2: return ""+item.getNumTerminate();
			case 3: return ""+item.getTotAlarms();
			case 4: return item.getTimestamp().toString();
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
	class ChatteringTableSorter extends TableSorterBase {
		
		/**
		 * Constructor
		 * 
		 * @param colIndex The index of the column to sort
		 * @param order The order ascending/descending
		 */
		public ChatteringTableSorter(int colIndex, int order) {
			super(colIndex,order);
		}
		
		@Override
		public int compare(Viewer viewer, Object e1, Object e2) {
			ChatteringAlarm a1 = (ChatteringAlarm) e1;
			ChatteringAlarm a2 = (ChatteringAlarm) e2;
			int ret = 0;
			switch (colIndex) {
			case 0:
				ret = a1.ID.compareTo(a2.ID);
				break;
			case 1:
				ret = (int)a1.getNumActive()-(int)a2.getNumActive();
				break;
			case 2:
				ret = (int)a1.getNumTerminate()-(int)a2.getNumTerminate();
				break;
			case 3:
				 ret=(int)a1.getTotAlarms()-(int)a2.getTotAlarms();
				break;
			case 4:
				ret=a1.getTimestamp().compareTo(a2.getTimestamp());
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

	public ChatteringView() {
		super(ChatteringAlarmsContainer.getInstance().colNames,new int[] {
			250,
			100,
			150,
			100,
			150
	});
		sorter=new ChatteringTableSorter(1, TableSorterBase.ASCENDING);
	}

	@Override
	public void createPartControl(Composite parent) {
		super.createPartControl(parent);
		tableViewer.setLabelProvider(new AlarmLabelProvider());
		tableViewer.setSorter(sorter);
		ChatteringAlarmsContainer.getInstance().setTableViewer(tableViewer);
		tableViewer.setInput(ChatteringAlarmsContainer.getInstance());
	}

}
