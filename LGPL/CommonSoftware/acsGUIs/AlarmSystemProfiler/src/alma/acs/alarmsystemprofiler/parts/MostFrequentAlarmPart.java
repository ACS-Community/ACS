/*******************************************************************************
 * Copyright (c) 2010 - 2013 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Lars Vogel <lars.Vogel@gmail.com> - Bug 419770
 *******************************************************************************/
package alma.acs.alarmsystemprofiler.parts;

import javax.annotation.PostConstruct;

import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;

import alma.acs.alarmsystemprofiler.document.MostFrequantAlarmsContainer;
import alma.acs.alarmsystemprofiler.document.MostFrequantAlarmsContainer.AlarmActNumber;
import alma.acs.alarmsystemprofiler.parts.TableViewBase;

public class MostFrequentAlarmPart extends TableViewBase {
	
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
		
	};
	
	/**
	 * A class to sort the table by ascending/descending order of a column
	 * 
	 * @author acaproni
	 *
	 */
	public class TableSorter extends TableSorterBase {
		
		
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


	
	public MostFrequentAlarmPart() {
		super(MostFrequantAlarmsContainer.getInstance().colNames,new int[] {
			250,
			100,
			150,
			100,
			150
	});
		sorter=new TableSorter(0, TableSorter.DESCENDING);
	}

	@PostConstruct
	public void createComposite(Composite parent) {
		parent.setLayout(new GridLayout(1, false));

		tableViewer.setLabelProvider(new AlarmLabelProvider());
		sorter=new TableSorter(0, TableSorter.DESCENDING);
		tableViewer.setSorter(sorter);
		MostFrequantAlarmsContainer.getInstance().setTableViewer(tableViewer);
		tableViewer.setInput(MostFrequantAlarmsContainer.getInstance());
		
		tableViewer = new TableViewer(parent);

		tableViewer.add("Sample item 1");
		tableViewer.add("Sample item 2");
		tableViewer.add("Sample item 3");
		tableViewer.add("Sample item 4");
		tableViewer.add("Sample item 5");
		tableViewer.getTable().setLayoutData(new GridData(GridData.FILL_BOTH));
	}
}