package alma.acs.alarmsystemprofiler.parts;

import javax.inject.Inject;
import javax.annotation.PostConstruct;

import org.eclipse.swt.widgets.Composite;

import alma.acs.alarmsystemprofiler.document.flood.FloodContainer;
import alma.acs.alarmsystemprofiler.document.flood.FloodContainer.FloodItem;
import alma.acs.alarmsystemprofiler.parts.MostFrequentAlarmPart.TableSorter;
import alma.acs.alarmsystemprofiler.parts.TableViewBase.TableSorterBase;

import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.swt.widgets.Composite;


public class AlarmFloodPart extends TableViewBase {
	
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
	
	@Inject
	public AlarmFloodPart() {
		super(FloodContainer.getInstance().colNames,
				new int[] {
				300,
				80
		});
		sorter=new FloodTableSorter(0, TableSorter.ASCENDING);
	}
	
	@PostConstruct
	public void postConstruct(Composite parent) {
		super.createPartControl(parent);
		tableViewer.setLabelProvider(new FloodLabelProvider());
		tableViewer.setSorter(sorter);
		FloodContainer.getInstance().setTableViewer(tableViewer);
		tableViewer.setInput(FloodContainer.getInstance());
	}
	
	
	
	
}