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


import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

import alma.acs.alarmsanalyzer.document.StaleAlarmsContainer;
import alma.acs.alarmsanalyzer.document.StaleAlarmsContainer.StaleAlarm;
import alma.acs.alarmsanalyzer.view.MFAView.TableSorter;

/**
 * The view of stale alarms i.e. the alarm active and never terminated.
 * <P>
 * The purpose of this view is to show alarms never terminated and how long do they remain 
 * active.
 * 
 * @author acaproni
 *
 */
public class StaleView extends TableViewBase {
	
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
			StaleAlarm item=(StaleAlarm)element;
			
			switch (columnIndex) {
			case 0: return item.ID;
			case 1: {
				return item.activationDuration();
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
	private class StaleTableSorter extends TableSorterBase {

		
		/**
		 * Constructor
		 * 
		 * @param colIndex The index of the column to sort
		 * @param order The order ascending/descending
		 */
		public StaleTableSorter(int colIndex, int order) {
			super(colIndex,order);
		}

		@Override
		public int compare(Viewer viewer, Object e1, Object e2) {
			StaleAlarm a1 = (StaleAlarm) e1;
			StaleAlarm a2 = (StaleAlarm) e2;
			int ret = 0;
			switch (colIndex) {
			case 0:
				ret = a1.ID.compareTo(a2.ID);
				break;
			case 1:
				ret = Long.valueOf(a1.activationTime.getTime()).compareTo(Long.valueOf((a2.activationTime.getTime())));
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
	 * The ID of this view
	 */
	public static final String ID = "alma.acs.alarmsanalyzer.staleview";
	
	/**
	 * constructor
	 */
	public StaleView() {
		super(StaleAlarmsContainer.getInstance().colNames,new int[] {
			500,
			100
	});
		sorter=new StaleTableSorter(1, TableSorter.ASCENDING);
	}

	@Override
	public void createPartControl(Composite parent) {
		super.createPartControl(parent);
		tableViewer.setLabelProvider(new AlarmLabelProvider());
		tableViewer.setSorter(sorter);
		StaleAlarmsContainer.getInstance().setTableViewer(tableViewer);
		tableViewer.setInput(StaleAlarmsContainer.getInstance());
	}
}
